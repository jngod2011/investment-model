
#******************************************************************
# Libraries and Functions
#****************************************************************** 
options(warn=-1)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

library(quantmod)  
library(randomForest)
library(progress)
library(ggplot2)
library(data.table)
library(ggsci)

sp500_components<-function()
{
  url = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
  txt = join(readLines(url))
  temp = extract.table.from.webpage(txt, 'Ticker', has.header = T)
  tickers = temp[, 'Ticker symbol']
  sector = temp[, 'GICS Sector']
  return(list(tickers=tickers, sector=sector))
}
fill_na<-function(x)
{
  for(i in which(is.na(x)))
  {
    x[i]<-ifelse(i==1,x[!is.na(x)][1],x[i-1])
  }
  return(x)
}
dynamic_threshold_returns<-function(tickers,periodicity="months",period="2000/",opt_rate=0.1)
{
  temp<-data[[tickers]]
  period.ends = endpoints(temp, periodicity)
  period.ends = period.ends[period.ends>0]
  temp = temp[period.ends,]  
  pd<-ifelse(periodicity=="months",20,ifelse(periodicity=="weeks",5,1))
  techind=cbind(BBands(temp[,2:4]),
                MAD=SMA(temp[,6],n=ceiling(20/pd))/SMA(temp[,6],n=ceiling(200/pd)),
                MFI(temp[,2:4],temp[,5]),ATR=ATR(temp[,2:4]),
                CCI=CCI(temp[,2:4]),vol=volatility(temp[,6]))
  techind<-as.data.frame(as.xts(techind)[period,])
  techind<-sapply(techind,fill_na)
  rt<-fill_na(temp[,6])
  rt<-na.omit(rt/Lag(rt,k=1)-1)[period,]
  
  long_pf<-short_pf<-c(0)
  threshold<-0.5
  for(i in 2:(length(rt)-1))
  {
    BOLL<-exp(techind[i,4]-1)
    MAD<-techind[i,5]/1.3
    MFI<-ifelse(abs(techind[i,6]-50)>30,exp(techind[i,6]/techind[i-1,6]-1),1)
    ATR<-ifelse(techind[i,7]/techind[i,8]>1,exp(techind[i,8]/techind[i,7]-1),1)
    CCI_fm<-techind[i-1,11];CCI_lt<-techind[i,11]
    CCI<-ifelse(abs(CCI_lt)>100,exp(CCI_lt/CCI_fm-1),1)
    pr<-mean(c(BOLL,MAD,MFI,ATR,CCI))
    adjust_direction<-ifelse(long_pf[i-1]<0||short_pf[i-1]>0,0,1)
    threshold<-threshold+ifelse(adjust_direction,(1-threshold)*opt_rate,-threshold*opt_rate)
    sg<-ifelse(abs(threshold-0.5)>=qnorm(0.1,0.5,opt_rate),-1,1)
    if(sg==-1)
    {
      long_pf<-c(long_pf,ifelse(pr>=1,rt[i+1],0))
      short_pf<-c(short_pf,ifelse(pr<1,-rt[i+1],0))
    }
    if(sg==1)
    {
      long_pf<-c(long_pf,ifelse(pr<1,rt[i+1],0))
      short_pf<-c(short_pf,ifelse(pr>=1,-rt[i+1],0))
    }
    
    # long_pf<-c(long_pf,ifelse(pr>=1,rt[i+1]*sg,0))
    # short_pf<-c(short_pf,ifelse(pr<1,-rt[i+1]*sg,0))
    # cat(i," ")
  }
  return(list(pf=long_pf+short_pf,vol=c(0,techind[1:(length(rt)-2),ncol(techind)])))
}

#******************************************************************
# Load historical data
#****************************************************************** 

load("data.rda")
SPY<-data[["SPY"]]["2000/"]
period.ends = endpoints(SPY, "weeks")
period.ends = period.ends[period.ends>0]
time_stamp<-index(SPY)
SPY<-as.data.frame(SPY[period.ends,6])
SPY<-SPY/Lag(SPY)-1
SPY<-SPY[-1,]
time_stamp<-time_stamp[period.ends][-1]
tickers<-data[['tickers']]


#******************************************************************
# Strategy
#****************************************************************** 

pb<-progress_bar$new(total=length(tickers))
weight1<-weight2<-weight3<-weight4<-c()
pf<-c()
for(i in 1:length(tickers))
{
  pb$tick()
  res<-try(dynamic_threshold_returns(tickers[i],opt_rate=0.1,
                                     periodicity = "weeks",period="2000/"),silent=T)
  if(!is.list(res)) next
  pf<-rbind(pf,c(tickers[i],res$pf))
  pp<-as.numeric(as.character(pf[,-1]))
  weight1<-rbind(weight1,rep(1,length(pp)))
  weight2<-rbind(weight2,(cumprod(1+pp))/sd(pp))
  weight3<-rbind(weight3,sapply(res$vol,FUN=function(x){return(ifelse(!x,0,x))}))
}
pf1<-pf
pf<-as.data.frame(pf)
pf[,-1]<-sapply(pf[,-1],as.character)
pf[,-1]<-sapply(pf[,-1],as.numeric)
rownames(pf)<-pf[,1]
pf<-pf[,-1]
weight1<-sapply(as.data.frame(weight1),function(x){ifelse(sum(x),return(x/sum(x)),return(rep(0,length(x))))})
weight2<-sapply(as.data.frame(weight2),function(x){ifelse(sum(x),return(x/sum(x)),return(rep(0,length(x))))})
weight3<-sapply(as.data.frame(weight3),function(x){ifelse(sum(x),return(x/sum(x)),return(rep(0,length(x))))})
pf1<-colSums(pf*weight1)
pf2<-colSums(pf*weight2)
pf3<-colSums(pf*weight3)
plot(cumprod(1+pf3),type="l")
lines(cumprod(1+pf2),col="red")
lines(cumprod(1+pf1),col="blue")
lines(cumprod(1+SPY),col="green")


#******************************************************************
# Plot
#****************************************************************** 
plot_table<-data.table(date=time_stamp,Equal_weight=cumprod(1+pf1),Sharpe_ratio_weight=cumprod(1+pf2),
                       Volatility_weight=cumprod(1+pf3),SPY=cumprod(1+SPY))
plot_table<-melt(plot_table,id.vars="date")
names(plot_table)<-c("date","Portfolios","Return")
plot_table<-as.data.frame(plot_table)
plot_table$Portfolios<-as.character(plot_table$Portfolios)
plot_table$date<-as.Date(plot_table$date)
ggplot(data=plot_table,aes(x=date,y=Return,group=Portfolios,colour=Portfolios))+
  geom_line(size=1.1, alpha=0.8)+theme_minimal()+scale_color_nejm()+
  labs(y="Returns(%)",title="Returns(%) of Different Portfolios",subtitle="from 2000-01 to 2018-05")+
  theme(legend.position="bottom")
