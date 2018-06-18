### LIBRARY ###
library("quantmod")
library("data.table")
load("Data.rda")

codeY<-"600036"
codeX<-"600000"
from<-as.Date("2017-01-01")
to<-as.Date("2017-09-30")
src<-"yahoo"
code<-c(codeX,codeY)
pair_data<-priceDL(code,from,to,src)

library("Quandl")
mydata = Quandl("FRED/GDP", start_date="2017-01-01",end_date="2017-02-01")

install.packages('devtools') #assuming it is not already installed
library(devtools)
install_github('andreacirilloac/updateR')
library(updateR)
updateR(admin_password = 'gta336cc')

aa<-table(as.character(z[,2]))
aaa<-table(as.character(c(z1[,2])$code))
aa==aaa
### FUNCTION ###
priceDL<-function(code,from,to,src)
{
  pv<-c()
  cn<-c()
  for(i in 1:length(code))
  {
    code0<-code[i]
    exchange<-substr(code0,1,1)
    code_in<-ifelse(exchange=="0"||exchange=="3",paste0(code0,".SZ"),ifelse(exchange=="6",paste0(code0,".SS")))
    ts_table<-getSymbols(code_in,from=from,to=to,auto.assign=F,src=src)
    pv<-cbind(pv,Cl(`ts_table`))
  }
  for(i in 1:length(code))
  {
    cn<-c(cn,paste0("X",i))
  }
  colnames(pv)<-cn
  return(pv)
}
