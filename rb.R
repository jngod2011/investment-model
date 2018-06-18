maxmin_in9<-function(v,flag)
{
  tt<-numeric(0)
  for(i in 1:length(v))
  {
    tt<-c(tt,ifelse(i<9,NA,ifelse(flag,max(v[(i-8):i]),min(v[(i-8):i]))))
  }
  return(tt)
}
p<-z[,list(date,maxmin_in9(as.numeric(high),1),maxmin_in9(as.numeric(low),0),as.numeric(close),wt),by=code]
setnames(p,c("code","date","max9","min9","close","weights"))
p<-p[,"RSV":=(close-min9)/(max9-min9)*100,by=code]

cal_kdj<-function(rsv)
{
  KDJ<-numeric(0)
  for(i in 1:length(rsv))
  {
    k<-ifelse((i==1)||is.na(rsv[i]),50,2/3*KDJ[i-1,1]+1/3*rsv[i])
    d<-ifelse(i==1,2/3*50+1/3*k,2/3*KDJ[i-1,2]+1/3*k)
    KDJ<-rbind(KDJ,c(k,d))
  }
  colnames(KDJ)<-c("K","D")
  return(as.data.frame(KDJ))
}
cal_rsi <- function(v,n){
  up<-0
  low<-0
  RS <- c()
  RSI <- c()
  RS[1:n]<-NA
  d<-c(0,diff(v))
  for (i in (n+1):length(v)){
    for(j in (i-n):(i-1)){
      up <- ifelse(d[j]>=0,up+d[j],up)
      low <- ifelse(d[j]<0,low+d[j],low)
    }
    RS[i]<-ifelse(low!=0,up/low,NA)
    
  }
  if (length(RS)!=length(v)) RS<-rep(NA,length(v))
  RSI<-ifelse(is.na(RS),100,100-100/(1+RS))
  return(RSI)
}
prsi<-p[,list(date,cal_rsi(close,2)),by=code]
names(prsi)<-c("code","date","RSI")
ptemp<-p[,cal_kdj(RSV),by=code]
p[,':='("K"=ptemp$K,"D"=ptemp$D,"RSI"=prsi$RSI)]
p[,c('max9','min9','RSV'):=NULL]
#p1<-p[-which(p$code=="CSI300"),]
p1<-p
p1[,":="("F1"=(K-D)-(shift(K,type="lag")-shift(D,type="lag")),"F2"=K-D),by=code]
p1[,"F3":=(((K-D)*(shift(K,type="lag")-shift(D,type="lag")))<0)*F1]
p1<-na.omit(p1)
#save(p1,file="data2.rda")
