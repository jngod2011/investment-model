tickers = sp500_components()$tickers

data <- new.env()
for(i in 1:len(tickers))
{
  try(getSymbols(tickers[i], src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T))
  cat(i,' ')
}
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)

# remove companies with less than 5 years of data
rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 ) 
rm(list=names(rm.index), envir=data)

bt.prep(data, align='keep.all', dates='1995::')
data$tickers<-tickers
save(data,file = "data.rda")