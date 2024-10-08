
library(VaRES)
source("DALC.R")
source("constants.R")
source("A_Sz.R")
source("ESlib.R")
source("HistoricalSimulation.R")
library(ggfortify)
library(fitdistrplus)
library(esreg)

setwd(getSrcDirectory(function(){})[1])

tickers = get_tickers()

methods = c('t2')

print(methods)
print(length(tickers))

Z <- data.frame(Stock=rep(NA,length(tickers)*length(methods)), Method=rep(NA,length(tickers)*length(methods)), Z=rep(NA,length(tickers)*length(methods)), stringsAsFactors = FALSE )



i_main = 1
print(Sys.time())

#pb <- txtProgressBar(style = 3)

for(selected in tickers)
{
  params = NULL
  fit = NULL
  singleStock = get_returns_for_single_stock(selected,start_date)
  #cat(selected,length(singleStock$Close))
  for(method in methods) {
    assign("last_start",NULL, envir = .GlobalEnv)
    stockName = get_stockname(selected)
    DF = MultiPeriodES(singleStock = singleStock,method = method,selected =stockName)
    Z$Stock[i_main] = get_stockname(selected)
    Z$Method[i_main] = method
    Z$Z[i_main] = ZES(DF) 
    i_main= i_main +1
    
   # setTxtProgressBar(pb,i_main / (length(tickers)*length(methods)))
    
  }
  
  #print(singleStock)
  print("finished")

  
}

path = paste(resultsFullPath,"Z.txt",sep = "")
#print(path)


Z$ZAE = abs(Z$Z)
Z$ZSE = Z$Z^2
write.table(Z, path ,sep="\t",col.names=NA)

for( method in methods)
{
  path = paste(resultsFullPath,"Z_",method,".txt",sep = "")
  Z1 = subset(Z , Method==method)
  write.table(Z1, paste(resultsFullPath,"Z_",method,".txt",sep = "") ,sep="\t",col.names=NA) 
}

print(Sys.time())
