library("data.table")
load('data.rda')

z<-z[order(code,date)]
z0<-z
z<-z[order(date)]
zxts<-xts(z,z$date)
ept<-z$date[endpoints(zxts,"quarters")[-1]]
find_qtr<-function(quarters_end,dateindex) return(which(dateindex==quarters_end))
z<-z[unlist(sapply(ept,find_qtr,z$date)),]
fdmt<-read.table("fundamental.csv",sep=",",header=T,colClasses="character")
fdmt<-data.table(fdmt)
# roe quality profitability
# epsg(EPS growth) growth
# nav(net asset value growth) growth
# (1/PE=eps/close)earning-yield value
fdmt<-fdmt[,list(code,year,quarter,roe,epsg,nav,eps)]
s1<-paste0(substr(z$date,1,4),quarter(z$date),z$code)
s2<-paste0(fdmt$year,fdmt$quarter,fdmt$code)
ftb<-c()
for(i in 1:length(s1))
{
  ftb<-rbind(ftb,fdmt[s2==s1[i],])
}
z[,':='("roe"=as.numeric(ftb$roe),"epsg"=as.numeric(ftb$epsg),"navg"=as.numeric(ftb$nav),"EY_value"=as.numeric(ftb$eps))]
z$close<-as.numeric(z$close)
z[,':='("EY_value"=EY_value/close)]
#### FACTORS ####
p<-z
p[,':='("qreturn"=(close-shift(close,type="lag"))/shift(close,type="lag")),by=code]
p1<-p
p1[,":="("qreturn"=shift(qreturn,type="lag")),by=code]

####################
p1=melt(p1,id.vars = c('code','date','close','weights','daily_return'),measure.vars = c('F1','F2','F3'))
#score
p1[, ':='(S1=(value-mean(value,na.rm = T))/sd(value,na.rm = T),
          S2=(value-mean(value,na.rm = T))/median(abs(value-median(value,na.rm = T)),na.rm = T),
          S3=frankv(value,ties.method = 'average',na.last = 'keep',order =1)),by=.(date,variable)]
p1[,S3:=S3/max(S3,na.rm = T),by=.(date,variable)]
p1
d=dcast(p1,code+date+close+weights+K+D+daily_return~variable,value.var = 'S3')
d
#deal with missing data
d[,':='(comp=(ifelse(is.na(F1),0,F1)+ifelse(is.na(F2),0,F2)+ifelse(is.na(F3),0,F3))/
          ifelse(is.na(F1)&is.na(F2)&is.na(F3),NA,ifelse(is.na(F1),0,1)+ifelse(is.na(F2),0,1)+ifelse(is.na(F3),0,1)))]
#if more kinds of factors, sum
#use one factor model

####################
fractile<-function(x,n){
  if(sum(!is.na(x))<n){return(rep(1L*NA,length(x)))}
  rnk=rank(x,ties.method='first',na.last='keep')
  qnt=quantile(rnk,probs=seq(0,1,length.out=n+1),na.rm=T,names=F)
  cut(rnk,breaks=qnt,include.lowest=T,labels=F,right=F)
}
NoBas=3
p1[,Basket:=fractile(RSI,3),by=date]
p1

Perf=p1[,sum(weights*daily_return,na.rm=T)/sum(ifelse(is.na(daily_return),0,weights)),by=.(date,Basket)]
Perf
BMPerf = p1[,sum(weights*daily_return,na.rm=T)/sum(ifelse(is.na(daily_return),0,weights)),by=date]
setnames(BMPerf,c('date','BM'))
BMPerf
setkey(BMPerf,date)
setkey(Perf,date)
Perf=BMPerf[Perf]
Perf

Perf[,RelRtn:=V1-BM]
Perf

Perf=dcast(Perf,date~Basket,value.var='RelRtn')
Perf

setnames(Perf,c('date','Low','Mid','High'))
Perf[,LS:=High-Low]
Perf
Perf=melt(Perf,id.vars='date')
Perf

ExRtn= Perf[,4*mean(value,na.rm=T),by=variable]
RskAdjRtn=Perf[,sqrt(4)*mean(value,na.rm=T)/sd(value,na.rm=T),by=variable]
ExRtn
RskAdjRtn


setorder(Perf,date)
Cump=Perf[,.(date=date,CumPerf=cumsum(value)),by=variable]
Cump=Cump[!is.na(CumPerf),]
setnames(Cump,c('Basket','date','Cum.Perf'))
Cump
#learning ggplot 
# %in%
require(ggplot2)
g=ggplot(data=Cump[Basket%in%c('Low','Mid','High')],aes(x=date,y=Cum.Perf))
g=g+geom_line(aes(group=Basket,color=Basket),size=1)
g=g+theme_bw(base_family='Times')
print(g)

g=ggplot(data=Cump[Basket =='LS'],aes(x=date,y=Cum.Perf))
g=g+geom_line(size=1)
g=g+theme_bw(base_family='Times')
print(g)

IC=p1[,cor(daily_return,K-D,method='spearman',use='pairwise.complete.obs'),by=date]
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

HR = Data[,sum(daily_return>BM,na.rm=T)/sum(!is.na(daily_return)),by=.(date,Basket)]
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
