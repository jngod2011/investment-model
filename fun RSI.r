#factor RSI
#v<-p$close[p$code=="000001"]
cal_rsi <- function(v,n){
  up<-0
  low<-0
  RS <- c()
  RSI <- c()
  RS[1:n]<-NA
  d<-c(0,diff(v))
  for (i in (n+1):length(v)){
    for(j in (i-n):(i-1)){
      up <- ifelse(d[j]>=0,up+d[j],up+0)
      low <- ifelse(d[j]<0,low+d[j],low+0)
    }
    RS[i]<-ifelse(low==0,NA,up/low)
    
  }
  for (m in 1:length(RS)){
    RSI[m]<-ifelse(is.na(RS[m]),100,100-100/(1+RS[m]))}
  return(RSI)
}

prsi<-p[,list(date,cal_rsi(close,14)),by=code]
prsi
