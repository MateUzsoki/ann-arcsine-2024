source("DALC.R")
source("constants.R")
library(moments)
library(aTSA)



tickers = get_tickers()

stockStats <- data.frame(skewness=rep(NA,100), kurtosis=rep(NA,100)
                         , mean = rep(NA,100), sd = rep(NA,100), median  = rep(NA,100)
                         , stock=rep(NA,100), adf1=rep(NA,100)
                         ,adf2=rep(NA,100),adf3=rep(NA,100),stringsAsFactors = FALSE )
UseMethod("moments::skewness")
methods(skewness)
          
for(i in 1:100)
{
  singleStock = get_returns_for_single_stock(tickers[i],start_date)
  X = singleStock$LogReturn
  stockName = get_stockname(tickers[i])
  print(stockName)
  
  stockStats$skewness[i] =moments::skewness(X)
  stockStats$kurtosis[i] =moments::kurtosis(X)
  stockStats$stock[i] = stockName
  stockStats$mean[i] = mean(X)
  stockStats$sd[i] = sd(X)
  stockStats$median[i] = median(X)
  # adf = adf.test(X, nlag=120,output=TRUE)
  # stockStats$adf1[i] = max(adf$type1[,3])
  # stockStats$adf2[i] = max(adf$type2[,3])
  # stockStats$adf3[i] = max(adf$type3[,3])
  
  
}

path = paste(getwd(),"/../OtherResults/kurtosis_skewness.txt",sep = "")
write.table(stockStats, path ,sep="\t",col.names=NA,dec = ",")
