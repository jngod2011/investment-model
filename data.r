library("quantmod")
library("data.table")

stocklist_300<-c()
for(i in 1:8)
{
  pages<-i
  temp_url<-paste0("http://vip.stock.finance.sina.com.cn/corp/view/vII_NewestComponent.php?page=",pages,"&indexid=000300")
  list300<-readLines(temp_url)
  content<-list300[grep('</div></td>',list300)]
  content1<-unlist(strsplit(content[as.logical(index(content)%%2)],split=">"))
  content2<-unlist(strsplit(content1[index(content1)%%4==3],split="<"))
  temp_list<-content2[as.logical(index(content2)%%2)]
  stocklist_300<-c(stocklist_300,temp_list)
}
from<-as.Date("2010-01-01")
to<-as.Date("2017-12-31")
src<-"yahoo"
z<-priceDL(stocklist_300,from,to,src)
z<-na.omit(z)
z[,2]<-as.character(z[,2])
bm<-read.table("benchmark.csv",header=T,sep=",")
v_k<-as.numeric(unlist(strsplit(as.character(bm[,6]),split = "K")))*1000
k<-as.data.frame(cbind(as.character(bm[,1]),rep("CSI300",length(bm[,1])),as.vector(bm[,3]),as.vector(bm[,4]),as.vector(bm[,5]),as.vector(bm[,2]),v_k))
colnames(k)<-colnames(z)
k$date<-as.Date(as.character(k$date),"%Y年%m月%d日")
k$open<-gsub(",","",as.character(k$open))
k$high<-gsub(",","",as.character(k$high))
k$low<-gsub(",","",as.character(k$low))
k$close<-gsub(",","",as.character(k$close))



z<-rbind(z,k)
rownames(z)<-1:length(z$date)
z<-data.table(z)

##########
z<-read.table("quarter.csv",header=T,sep=",",colClasses = "character")
z<-data.table(z)
z<-z[,c(3,2,4,5,6,1,7)]
z$date<-as.Date(z$date)
wt<-read.table("weight.csv",header = T,sep=",",colClasses = "character")
wt$weight<-as.numeric(wt$weight)/100
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
    x[i]<-ifelse(i==1,x[!is.na(x)][1],x[i-1])
  }
  return(x)
}
z[,':='("wt"=wt0)]
wt0<-z[,fill_wt(wt),by=code]$V1
z[,wt:=wt0]
save(z,stocklist_300,file="data1.rda")

#####
priceDL<-function(code,from,to,src)
{
  pv<-numeric(0)
  cn<-numeric(0)
  for(i in 1:length(code))
  {
    code0<-code[i]
    exchange<-substr(code0,1,1)
    code_in<-ifelse(exchange=="0"||exchange=="3",paste0(code0,".SZ"),ifelse(exchange=="6",paste0(code0,".SS")))
    ts_table<-try(getSymbols(code_in,from=from,to=to,auto.assign=F,src=src),silent = T)
    if ('try-error' %in% class(ts_table)) next
    Dt=index(ts_table)
    ts_table<-as.data.frame(ts_table)
    temptable<-data.frame(date=Dt,code=code0,open=ts_table[,1],high=ts_table[,2],low=ts_table[,3],close=ts_table[,4],volume=ts_table[,5])
    pv<-rbind(pv,temptable)
  }
  return(pv)
}

