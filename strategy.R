rm(list=ls())
## LIBRARY ##
library("data.table")
library("ggplot2")
library("ggsci")
library("RColorBrewer")
library("ggpubr")

## DATA ##
z<-read.table("close.csv",header=T,sep=",",colClasses = "character")
z<-data.table(z)
z$date<-as.Date(z$date)
z<-z[z$date>=as.Date("2010-03-01")];z<-z[z$date<=as.Date("2017-10-01")]
wt<-read.table("weight.csv",header = T,sep=",",colClasses = "character")
wt$weight<-as.numeric(wt$weight)/100
#insert weight
s1<-paste0(z$code,substr(z$date,1,4),substr(months(z$date),1,1))
s2<-paste0(wt$consTickerSymbol,substr((wt$effDate),1,4),substr(months(as.Date(wt$effDate)),1,1))
wt0<-rep(NA,length(z$date))
for(i in 1:length(wt$effDate))
{
  wt0[s1==s2[i]]<-wt$weight[i]
}
fill_wt<-function(x)
{
  for(i in which(is.na(x)))
  {
    x[i]<-ifelse(sum(!is.na(x))>0,ifelse(i==1,x[!is.na(x)][1],x[i-1]),0)
  }
  return(x)
}
z[,':='("wt"=wt0)]
z[,"wt":=fill_wt(wt),by=code]
#factors
z<-z[order(code,date)]
fdmt<-read.table("fundamantal-monthly.csv",sep=",",header=T,colClasses="character")
fdmt<-data.table(fdmt)
# roe quality profitability
# epsg(EPS growth) growth
# nav(net asset value growth) growth
# (PE=close/eps)  value
fdmt<-fdmt[,list(code,year,quarter,roe,epsg,nav,eps)]
s1<-paste0(year(z$date),month(z$date),z$code)
s2<-paste0(fdmt$year,fdmt$quarter,fdmt$code)
ftb<-c()
for(i in 1:length(s1))
{
  ftb<-rbind(ftb,fdmt[s2==s1[i],])
}
z[,':='("roe"=as.numeric(ftb$roe),"epsg"=as.numeric(ftb$epsg),"navg"=as.numeric(ftb$nav),"pe"=as.numeric(ftb$eps))]
z$close<-as.numeric(z$close)
z[,':='("pe"=close/pe)]
#calculate returns
p1<-z
p1[,':='("qreturn"=(close-shift(close,type="lag"))/shift(close,type="lag")),by=code]
#shift for the reason that returns earned in the next quarter
#p1[,":="("qreturn"=shift(qreturn,n=5,type="lag")),by=code]
#shift for the reason that fundamental data comes out in the coming quarter

p1<-p1[p1$date>"2010-10-01"]
p1$date<-as.character(as.numeric(year(p1$date))*100+as.numeric(month(p1$date)))
setorder(p1,date,code)
save(p1,file="p1.rda")
load("stk_melt22.rda")
stk_melt<-data.table(stk_melt)
setkey(p1,code,date)
setkey(stk_melt,variable,date)
p1<-p1[stk_melt]
p1$qreturn<-NULL
colnames(p1)[colnames(p1)=="value"]<-"qreturn"
colnames(p1)[colnames(p1)=="variable"]<-"code"
p1[,"wt":=fill_wt(wt),by=code]
p1=melt(p1,id.vars = c('date','code','wt','qreturn'),measure.vars = c('roe','epsg','navg','pe'))
#score
p1[, ':='(S1=(value-mean(value,na.rm = T))/sd(value,na.rm = T),
          S2=(value-mean(value,na.rm = T))/median(abs(value-median(value,na.rm = T)),na.rm = T),
          S3=frankv(value,ties.method = 'average',na.last = 'keep',order =1)),by=.(date,variable)]
p1[,S3:=S3/max(S3,na.rm = T),by=.(date,variable)]
p1
d=dcast(p1,date+code+wt+qreturn~variable,value.var = 'S3')
d
#deal with missing data
d[,':='(comp_quality=ifelse(is.na(roe),0,roe)/ifelse(is.na(roe),NA,1),
        comp_growth=(ifelse(is.na(epsg),0,epsg)+ifelse(is.na(navg),0,navg))/
          ifelse(is.na(epsg)&is.na(navg),NA,ifelse(is.na(epsg),0,1)+ifelse(is.na(navg),0,1)),
        comp_value=ifelse(is.na(pe),0,pe)/ifelse(is.na(pe),NA,1))]
p1<-d
p1<-d[,Comp:=comp_quality+comp_growth+comp_value]
score<-data.frame(date=p1$date,Ticker=p1$code,Score=p1$Comp)
save(score,file="socre33.rda")
## ONE FACTOR BACKTEST ##
fractile<-function(x,n){
  if(sum(!is.na(x))<n){return(rep(1L*NA,length(x)))}
  rnk=rank(x,ties.method='first',na.last='keep')
  qnt=quantile(rnk,probs=seq(0,1,length.out=n+1),na.rm=T,names=F)
  cut(rnk,breaks=qnt,include.lowest=T,labels=F,right=F)
}
single_factor<-function(p1,NoBas)
{
  p1[,Basket:=fractile(V1,4),by=date]
  Perf=p1[,sum(wt*qreturn,na.rm=T)/sum(ifelse(is.na(qreturn),0,wt)),by=.(date,Basket)]
  BMPerf = p1[,sum(wt*qreturn,na.rm=T)/sum(ifelse(is.na(qreturn),0,wt)),by=date]
  setnames(BMPerf,c('date','BM'))
  BMPerf
  setkey(BMPerf,date)
  setkey(Perf,date)
  Perf=BMPerf[Perf]
  Perf[,RelRtn:=V1]
  Perf=dcast(Perf,date~Basket,value.var='V1')
  setnames(Perf,c('date','Low','Mid1','Mid2','High','NaN'))
  Perf[,LS:=High-Low]
  Perf=melt(Perf,id.vars='date')
  setorder(Perf,date)
  Perf<-Perf[Perf$variable=="High",][,-2]
  colnames(Perf)<-c("MTH","value")
  return(list(Perf,BMPerf))
}
single_factor(p1,4)
cpi<-read.table("cpi.csv",sep=",",colClasses="character",stringsAsFactors=F)[,-3]
colnames(cpi)<-c("date","cpi")
cpi$date<-sub("\\.","",cpi$date)
cpi$cpi<-as.numeric(cpi$cpi)
cpi<-cpi[length(cpi$cpi):1,]
cpi$cpi[-1]<-diff(cpi$cpi)/cpi$cpi[-length(cpi$cpi)]
cpi<-cpi[-1,]

p2<-p1
colnames(p2)[colnames(p2)=="roe"]<-"V1"
sroe<-single_factor(p2,4)[[1]][,2]
p2<-p1
colnames(p2)[colnames(p2)=="epsg"]<-"V1"
sepsg<-single_factor(p2,4)[[1]][,2]
p2<-p1
colnames(p2)[colnames(p2)=="navg"]<-"V1"
snavg<-single_factor(p2,4)[[1]][,2]
p2<-p1
colnames(p2)[colnames(p2)=="pe"]<-"V1"
spe<-single_factor(p2,4)[[1]][,2]
sbm<-single_factor(p2,4)[[2]][,2]
bm_sty_cpi<-data.frame(BM=sbm,roe=sroe,epsg=sepsg,navg=snavg,pe=spe,cpi=cpi$cpi)
colnames(bm_sty_cpi)<-c("BM","roe","epsg","navg","pe","cpi")
save(bm_sty_cpi,file="factor77.rda")
save(p1,file="p111.rda")
p1[,Basket:=fractile(Comp,4),by=date]
p3<-p1
p3[,wt:=wt/(sum(wt)),by=.(Basket,date)]
longleg<-p3[p3$Basket==4,][,1:3]
noleg<-p3[(p3$Basket==2)|(p3$Basket==3)|(is.na(p3$Basket))][,1:3]
shortleg<-p3[p3$Basket==1,][,1:3]
awt_bm<-rbind(longleg,noleg,shortleg)
noleg$wt<-0
shortleg$wt<-0
awtt<-data.frame(wt=rbind(longleg,noleg,shortleg))
p3<-p1
p3[,wt:=wt/sum(wt),by=date]
awt<-cbind(awtt,p3$wt,awtt$wt.wt-p3$wt)
colnames(awt)<-c("date","code","wt","wt_bm","wt_active")
save(awt,file="awt11.rda")

as.Date(as.character(as.numeric(substr(p1$date[1],1,4))*100+as.numeric(substr(p1$date[1],5,6))),"%Y%m")

load("test_weight.rda")
cbind(wsolution1,awt[awt$date=="201010",]$wt)

length(awt$date)
save(awt,file="wt_port1.rda")

Perf=p1[,sum(wt*qreturn,na.rm=T)/sum(ifelse(is.na(qreturn),0,wt)),by=.(date,Basket)]
BMPerf = p1[,sum(wt*qreturn,na.rm=T)/sum(ifelse(is.na(qreturn),0,wt)),by=date]
setnames(BMPerf,c('date','BM'))
BMPerf
setkey(BMPerf,date)
setkey(Perf,date)
Perf=BMPerf[Perf]
Perf[,RelRtn:=V1]
Perf=dcast(Perf,date~Basket,value.var='V1')
setnames(Perf,c('date','Low','Mid1','Mid2','High','NaN'))
Perf[,LS:=High-Low]
Perf=melt(Perf,id.vars='date')
setorder(Perf,date)

load("wsolution1.rda")
wsolution1<-data.table(wsolution1)
setorder(wsolution1,V1)
wsolution1<-as.data.frame(wsolution1)
wsolution11<-wsolution1[,-1]
load("wsolution2.rda")
wsolution2<-data.table(wsolution2)
setorder(wsolution2,V1)
wsolution2<-as.data.frame(wsolution2)
wsolution22<-wsolution2[,-1]
save(p1,file="p111.rda")
VaR_cal<-function(table)
{
  var11<-var12<-var13<-c()
  for(i in 1:length(table[1,]))
  {
    table[,i]<-p1[p1$date==colnames(table)[i],qreturn]*as.numeric(table[,i])
    var11<-c(var11,mean(table[,i])-qnorm(1-0.1)*sd(table[,i]))
    var12<-c(var12,mean(table[,i])-qnorm(1-0.05)*sd(table[,i]))
    var13<-c(var13,mean(table[,i])-qnorm(1-0.01)*sd(table[,i]))
  }
  return(list(var11,var12,var13))
}
VaR_table<-function(table1,table2,table3)
{
  var_table<-data.frame(c(table1[1],cumprod(1+table1)[3]-1,cumprod(1+table1)[6]-1,cumprod(1+table1)[12]-1),
                        c(table2[1],cumprod(1+table2)[3]-1,cumprod(1+table2)[6]-1,cumprod(1+table2)[12]-1),
                        c(table3[1],cumprod(1+table3)[3]-1,cumprod(1+table3)[6]-1,cumprod(1+table3)[12]-1))
  colnames(var_table)<-c("90%","95%","99%")
  row.names(var_table)<-c("1m return","3m return","6m return","12m return")
  return(var_table)
}
orport<-data.table(awt[,1:3])
setorder(orport,date,code)
orport<-as.data.frame(dcast(orport,code~date,value.var="wt"))[,-1]
v1<-VaR_cal(orport)
VaR_table(v1[[1]],v1[[2]],v1[[3]])

orport_n<-orport
for(i in 1:length(orport_n[1,]))
{
  orport_n[,i]<-rnorm(length(orport_n[,1]),mean(orport_n[,i]),sd(orport_n[,i]))
}
orport_n<-VaR_cal(orport_n)
VaR_table(orport_n[[1]],orport_n[[2]],orport_n[[3]])

v1<-VaR_cal(wsolution11)
VaR_table(v1[[1]],v1[[2]],v1[[3]])
v1<-VaR_cal(wsolution22)
VaR_table(v1[[1]],v1[[2]],v1[[3]])

norm_h<-p1[p1$date==201708,]$qreturn

norm_h<-data.frame(x=norm_h)

n = length(norm_h[,1])
mean = mean(norm_h$x)
sd = sd(norm_h$x)
binwidth = 0.01 # passed to geom_histogram and stat_function
dnorm.count<-function(x,mean=0,sd=1,log=FALSE,n=1,binwidth=1)
{
  n*binwidth*dnorm(x=x,mean=mean,sd=sd,log=log) 
}
ggplot(norm_h,aes(x=x))+
  geom_histogram(data=norm_h,aes(x=x),binwidth=binwidth,colour="white",fill="lightpink3",show.legend = T)+
  theme_light()+
  stat_function(fun=dnorm.count,args=c(mean=mean,sd=sd,n=n,binwidth=binwidth),colour="steelblue3",size=1.3,alpha=0.8)+
  ylab("Observations")+xlab("1 day return")+
  scale_x_continuous(breaks=c(-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05,0.1,0.15,0.2,0.25,0.3,0.35))

p1[,wt,by=date]
return1<-c()
for(i in 2:length(wsolution1[1,]))
{
  r1<-data.table(p1[p1$date==colnames(wsolution1)[i],])
  setorder(r1,code)
  return1<-c(return1,sum(r1$qreturn*as.numeric(wsolution1[,2])))
}
return2<-c()
for(i in 2:length(wsolution2[1,]))
{
  r1<-data.table(p1[p1$date==colnames(wsolution2)[i],])
  setorder(r1,code)
  return2<-c(return2,sum(r1$qreturn*as.numeric(wsolution2[,2])))
}
plot(cumsum(return2),type='l')
opr<-p1[,qreturn*wt/sum(wt),by=.(date,Basket)]
plot(cumsum(opr[,sum(V1),by=.(Basket,date)][Basket==2,]$V1)[15:84],type='l')
lines(cumsum(return1),col="red")
lines(cumsum(return2),col="blue")
creturn<-data.frame(date=unique(opr$date)[15:84],Original=100*cumsum(opr[,sum(V1),by=.(Basket,date)][Basket==3,]$V1)[15:84],
                    Min_TE=100*cumsum(return1),
                    MVO=100*cumsum(return2))
creturn$date<-paste0(substr(as.character(creturn$date),1,4),"-",substr(as.character(creturn$date),5,6))
creturn<-data.table(creturn)
creturn<-as.data.frame(melt(creturn,id.vars="date"))
colnames(creturn)<-c("date","Portfolios","Returns")
ggplot(creturn,aes(x=date,y=Returns,group=Portfolios,colour=Portfolios))+geom_line(size=1.5,alpha=0.7)+
  theme_minimal()+theme(axis.text.x=element_text(angle = 0))+
  labs(y="Returns(%)",title="Returns(%) of Different Portfolios",subtitle="from 2011-12 to 2017-09")+
  scale_x_discrete(breaks=c("2011-01","2012-01","2013-01","2014-01","2015-01","2016-01","2017-01")) +
  theme(plot.title=element_text(hjust=0.5,size=19,color="grey30"),plot.subtitle=element_text(size=14,color="grey50"))+
  scale_color_nejm()


plot(scale(return1))
mean(return1)-qnorm(0.05)*var(return1)
qnorm(1-0.05/2)

hrt<-p1[as.numeric(p1$date)>=201610,1:4]
hrt[,wt:=wt/sum(wt),by=date]
hrt[,qreturn:=wt*qreturn];hrt$wt<-NULL
hrt<-dcast(hrt,code~date,value.var="qreturn")[,-1]
hrt<-VaR_cal(as.data.frame(hrt))
VaR_table(hrt[[1]],hrt[[2]],hrt[[3]])

load("TE_s.rda")
load("TV_s.rda")
TE_s<-data.table(t(TE_s));TV_s<-data.table(t(TV_s))
TE_s$BM<-TV_s$BM<-NULL
colnames(TE_s)[1:11]<-colnames(TV_s)[1:11]<-c("IT","Utilities","Healthcare","Materials","RealEstate","Industials","ConsumerSta","Telecom","Energy","Financials","ConsumerDis")
TE_s[,"date":=paste0(substr(unique(p1$date)[15:84],1,4),"-",substr(unique(p1$date)[15:84],5,6),"-","15")]
TV_s[,"date":=paste0(substr(unique(p1$date)[15:84],1,4),"-",substr(unique(p1$date)[15:84],5,6),"-","15")]
TE_s<-melt(TE_s[-1,],id.vars="date");TV_s<-melt(TV_s[-1,],id.vars="date")
colnames(TE_s)[2]<-"Factors";colnames(TV_s)[2]<-"Factors"
colourCount=16
getPalette = colorRampPalette(pal_simpsons("springfield", alpha = 0.8)(8))
ggplot(data=TV_s,aes(x=as.Date(date),y=value,fill=Factors,colour=Factors))+
  geom_bar(stat="identity",position="stack")+
  theme_minimal()+
 # theme(panel.border=element_blank(), panel.grid.major.x=element_blank())+
  labs(y="Total Variance",x="Date")+scale_color_manual(values = getPalette(colourCount))+
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(nrow=2))

ggplot(data=TE_s,aes(x=as.Date(date),y=value,fill=Factors,colour=Factors))+
  geom_bar(stat="identity",position="stack")+
  theme_minimal()+
  # theme(panel.border=element_blank(), panel.grid.major.x=element_blank())+
  labs(y="Tracking Error",x="Date")+scale_color_manual(values = getPalette(colourCount))+
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(nrow=2))


#plot
require(ggplot2)
Cump$date<-as.character(Cump$date)
g=ggplot(data=Cump[Basket%in%c('Low','Mid1','Mid2','High')],aes(x=date,y=Cum.Perf))
g=g+geom_line(aes(group=Basket,color=Basket),size=1)
g=g+theme_bw(base_family='Times')
print(g)

Cump1<-Cump[c(which(Basket=="High"),which(Basket=="Low")),]
Cump1[Basket=="Low",Cum.Perf:=(-Cum.Perf)]
BM1<-cbind(rep("BM",length(BMPerf$BM)),BMPerf)
BM1$date<-as.character(BM1$date)
BM1$BM<-cumsum(BM1$BM)
colnames(BM1)[1]<-"benchmark"
g1<-ggplot()
g1<-g1+geom_area(data=Cump1,aes(x=date,y=Cum.Perf,fill=Basket),position = "stack",alpha=0.4)
g1<-g1+geom_line(aes(x=date,y=BM,colour=benchmark),BM1,size=1.2)
g1<-g1+theme_bw()
print(g1)

IC=p1[,cor(qreturn,Comp,method='spearman',use='pairwise.complete.obs'),by=date]
IC[,mean(V1,na.rm=T)]
setnames(IC,c('date','Info.Coef'))

g=ggplot(data=IC,aes(x=date,y=Info.Coef))
g=g+geom_bar(stat='identity')
g=g+theme_bw(base_family='Times')
print(g)


setkey(p1,date)
setkey(BMPerf,date)
Data=BMPerf[p1]
Data

HR = Data[,sum(qreturn>BM,na.rm=T)/sum(!is.na(qreturn)),by=.(date,Basket)]
HR_avg=HR[,mean(V1,na.rm=T),by=Basket]
setorder(HR_avg,Basket)
HR_avg
HR[,HitRate:=V1-0.5]
g=ggplot(data=HR[!is.na(Basket)],aes(x=date,y=HitRate))+geom_bar(stat='identity')
g=g+facet_wrap(~Basket,nrow=1)+theme_bw(base_family='Times')
print(g)

Turnover<-function(x)
{
  setorder(x,date)
  Mth<-x[,unique(date)]
  sapply(2:length(Mth),function(m){
    Basket1<-x[date==Mth[m-1],unique(code)]
    Basket2<-x[date==Mth[m],unique(code)]
    length(setdiff(Basket1,Basket2))/length(Basket1)
  })
}

TO_Long<-Turnover(Data[Basket==3,])
TO_Short<-Turnover(Data[Basket==1,])
TO_Long
TO_Short
mean(TO_Long)
mean(TO_Short)

gg<-p1[Basket==4,sum(wt/sum(wt)*qreturn),by=(date)]

p1[,Basket:=fractile(Comp,4),by=date]
dat1<-p1[Basket==4,]
dat1[,WGT1:=0]
dat1[Basket==4,WGT1:=wt]
dat1[,WGT1:=WGT1/sum(WGT1),by=date]
dat1[is.na(qreturn),qreturn:=0]
dat1[,WGT11:=WGT1*(1+qreturn)]
dat1[,WGT11:=WGT11/sum(WGT11),by=date]
dat1
Wgt1<-as.data.frame(dcast(dat1,code~date,value.var = 'WGT11'))
Wgt2<-as.data.frame(dcast(dat1,code~date,value.var = 'WGT1'))

WgtChg<-sapply(2:(ncol(Wgt1)-1),function(i){
  ifelse(is.na(Wgt2[,i+1]),0,Wgt2[,i+1])-ifelse(is.na(Wgt1[,i]),0,Wgt1[,i])
})
colnames(WgtChg)<-colnames(Wgt2)[-(1:2)]
WgtChg<-as.data.table(WgtChg)
WgtChg[,code:=Wgt2$code]
WgtChg<-melt(WgtChg,id.vars = 'code')
setnames(WgtChg,old=c('variable','value'),new=c('date','Turnover'))
WgtChg[,TCost:=ifelse(Turnover>0,Turnover*0.005,-Turnover*0.006)]
WgtChg
Turnover<-WgtChg[,sum(abs(Turnover)),by=date]
Turnover
Turnover[,mean(V1)]
p1[,date:=as.character(date)]
setkey(p1,date,code)
setkey(WgtChg,date,code)
p1<-WgtChg[p1]

Perf<-p1[,.(Pre_cost=sum(wt*qreturn),
            Post_cost=sum(wt*qreturn)-sum(TCost,na.rm=T)),by=date]
Perf[,.(mean(Pre_cost)*4,mean(Post_cost)*4)]
Turnover[order(-V1)]
Top5<-cbind(rep("top5",5),as.character(Turnover$date[1:5]))
Top10<-cbind(rep("top10",10),as.character(Turnover$date[1:10]))
Top20<-cbind(rep("top20",20),as.character(Turnover$date[1:20]))
rr<-rbind(Top5,Top10,Top20)
pp1<-melt(Perf,measure.vars = c("Pre_cost","Post_cost"))[c(-1,-28),]
rank5<-data.frame(rank=rep("top1/5",10),Transaction_cost=rep("1",10),return=numeric(10),stringsAsFactors = F)
rank10<-data.frame(rank=rep("top10",20),Transaction_cost=rep("1",10),return=numeric(20),stringsAsFactors = F)
rank20<-data.frame(rank=rep("top20",40),Transaction_cost=rep("1",10),return=numeric(40),stringsAsFactors = F)
rrr<-rbind(rank5,rank10,rank20)
colnames(pp1)[2:3]<-c("transaction_cost","return")
pp1<-as.data.frame(pp1)
pp1$date<-as.character(pp1$date)
pp1$transaction_cost<-as.character(pp1$transaction_cost)
pp1$return<-as.numeric(pp1$return)

for(i in 1:length(rr[,1]))
{
  if(sum(which(pp1$date==rr[i,2]))){
    rrr[c(2*i-1,2*i),2]<-as.vector(pp1[which(pp1$date==rr[i,2]),2])
    rrr[c(2*i-1,2*i),3]<-as.vector(pp1[which(pp1$date==rr[i,2]),3])}
}
rrr<-as.data.table(rrr)
rrr<-rrr[,mean(return),by=c("rank","Transaction_cost")]
colnames(rrr)[3]<-"return"
rrr$return<-round(rrr$return,digits = 4)
p <- ggplot(data=rrr, aes(x=rank, y=return, fill=Transaction_cost)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()
p<-p+geom_text(aes(label=return), vjust=1.6, color="black",
          position = position_dodge(0.9), size=3.7)
p + scale_fill_manual(values=c('#999999','#E69F00'))
#p + scale_fill_brewer(palette="Blues")

