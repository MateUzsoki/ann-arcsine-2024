
#Ne sz?moljunk minden adathoz ?j neur?lis h?l?t

require(neuralnet)
source("DALC.R")
source("A_Sz.R")
library(zoo)

library(oce)

library(magrittr)
library(keras)

CalculateAnn <-function(train_window_size,keep_model_for, doStandarziationY,extra="", isESMA=FALSE, baseMethod)
{
  
method = GetANNMethodName(train_window_size,keep_model_for, isESMA, doStandarziationY,extra, baseMethod)
print(method)
tickers = get_tickers()

#methods = get_methods()

ann_count = 1

Z <- data.frame(Stock=rep(NA,100), Method=rep(NA,100), Z=rep(NA,100), stringsAsFactors = FALSE )


#selected = tickers[1]
#method = methods[1]

#stockName = get_stockname(selected)
#ath = GetStockOutputPath(method,stockName)


i_main = 1



#method = 'ANN-AS-ESMA1000T126K-SIGM'
#method= 'ANN-AS-1000T126K-SIGM'
#train_window_size = 1000
#keep_model_for = 126

DF_method = NULL
DF_stock = NULL
DF_arcsine = NULL



for(selected in tickers)
{ 

  #selected = tickers[1]
  #method = methods[1]
  
  stockName = get_stockname(selected)
  path = GetStockOutputPath(method,stockName)
  
  if(file.exists(path))
  {
    print(paste(Sys.time(),":", stockName, " exists!"))
    
    DF_Result =read.table(path,sep="\t",header = TRUE, row.names = 1)
    DF_method = df_append(DF_method,DF_Result)
    #DF_arcsine = df_append(DF_arcsine,DF_Test)
    DF_stock = df_append(DF_stock,DF_Result)
    
    #DF_LFR_full <- read.table(path,sep="\t",header = TRUE, row.names = 1)
  }
  else
  {
    inprogressPath = GetStockOutputPath(paste(method,"-started",sep=""),stockName)
    
    if(file.exists(inprogressPath))
    {
      next
    }
    else 
    {
      write.table(DF_stock[1,1], inprogressPath ,sep="\t",col.names=NA,dec=",")
    }
    
  
  #DF <- read.table(path,sep="\t",header = TRUE, row.names = 1)
  #DF = head(DF,-1) #remove first row
  
  print(paste(Sys.time(),":", stockName))

  
  DFH <- read.table(GetStockOutputPath("historical",stockName),sep="\t",header = TRUE, row.names = 1)
 
  #test_count = nrow(DFH) - train_count
  DFH = DFH[-1,]
  
  DFH["ES_D"] = DFH["ES"] - DFH["VAR"]
  
  DF_LFR_full <- read.table(GetStockOutputPath(baseMethod,stockName),sep="\t",header = TRUE, row.names = 1)

  
  DF_LFR = head(DF_LFR_full,-1) ##remove last row, so we have same amount of data as Historical
  
  #DF_LFR["ES_D"] = DF_LFR["ES"] - DF_LFR["VAR"]
  0
  DF_LFR= ZES_detailed(DF_LFR)

  

  #we fill the first 250 rows with zeros
  #we need the head (x, -1) so we only have the ESR_MA for past periods.
  
  DF_LFR_ES_First250 = as.numeric(head(DF_LFR["ES"],250)[,1])
  #we fill the first 250 rows with zeros
  #we need the head (x, -1) so we only have the ESR_MA for past periods.
  DF_LFR["ESR_MA"] = c(DF_LFR_ES_First250 ,head(rollmean(DF_LFR['ESR'],250), -1) ) 
  #DF_LFR["ESR_MA"] = c(rep(0,250) ,head(rollmean(DF_LFR['ESR'],250), -1) ) 
  
  
  #DF_LFR["ESR"] = oce::despike(DF_LFR["ESR"], reference = "median")

  
  DF_LFR["VAR_REAL"] =   DF_LFR["VAR"] # DFH["VAR"]
  
  DF_LFR["ES_MA"] = c(DF_LFR_ES_First250 ,head(rollmean(DF_LFR['ES'],250), -1) ) 
  
  
  if(isESMA)
  {
    DF_LFR["ES_REAL"] =  DF_LFR["ESR_MA"] # DF_LFR["ESR"]
    DF_LFR["ES_D_REAL"] =   (DF_LFR["ESR_MA"] -DF_LFR["ES_MA"])#*250
  }
  else
  {
    DF_LFR["ES_REAL"] =  DF_LFR["ESR"]
    DF_LFR["ES_D_REAL"] =   (DF_LFR["ES_REAL"] -DF_LFR["ES"])
    
  }
  
  
  DF_LFR["VAR_D_REAL"] =  (DF_LFR["VAR_REAL"] - DF_LFR["VAR"]) 
  
  
  
  #DF_LFR["ES_D_REAL"] = oce::despike(DF_LFR["ES_D_REAL"], reference = "median")
  
  #DF_LFR["Date"] <- NULL
  #DF_LFR["X"] <- NULL
  
  DF_LFR_PREV = head(DF_LFR,-1) ##remove last row
  DF_LFR = tail(DF_LFR,-1) ##remove first row, so we have same amount of data as Historical

  DF_LFR["VAR_D_PREV"] = DF_LFR_PREV["VAR_D_REAL"]  
  DF_LFR["ES_D_PREV"] = DF_LFR_PREV["ES_D_REAL"]

  
  
  total_row_count = nrow(DF_LFR)
  
  DF_stock = NULL
  
  pb = txtProgressBar(min = 0, max = total_row_count-train_window_size-1, initial = 1) 
  
  i= 1
  
  while (i <= (total_row_count-train_window_size-keep_model_for))
  #for (i in 1268:1368)
  {
    
    train_window_start = i
    train_window_end = i+train_window_size
    
    DF_Train =    DF_LFR[train_window_start:train_window_end,]
    DF_Test = DF_LFR[train_window_end:(train_window_end+keep_model_for),]
    
    X_train = as.matrix(DF_Train["ES_D_PREV"])
    X_test = as.matrix(DF_Test["ES_D_PREV"])
    
    Y_train = as.matrix(DF_Train["ES_D_REAL"])
    y_test = as.matrix(DF_Test["ES_D_REAL"])
    
    y_min = min(Y_train)
    y_max = max(Y_train)
    
    if(doStandarziationY)
    {
      Y_train= Range01(Y_train)
    }
    #x_min = min(X_train)
    #x_max = max(X_train)
    #X_train= Range01(X_train)

    
   # Y_train_descaled = DeRange01(Y_train_scaled,y_min,y_max)
  #  min(Y_train_descaled)
  #  y_min
  #  max(Y_train_descaled)
  #  y_max
    
    n_units = 1 
    
    model <- keras_model_sequential() 

    model %>%
  #  layer_dense(units = n_units, 
  #           activation = 'sigmoid', 
               #input_shape = 1     # ettől most gyorsabb lesz
  #              ) %>% 
      
      layer_dense(units = 4, activation = 'sigmoid') %>%
      
      layer_dense(units = 4, activation = 'sigmoid') %>%
    
      layer_dense(units = 1, activation = 'linear')
    
    
    model %>% compile(
      loss = 'mae',
      optimizer = optimizer_nadam(),
      metrics = c('mae')
    )
    
    model %>% fit(
      X_train, Y_train, 
      epochs = 200, batch_size = 10, verbose = 0, 
      validation_split = 0.1
    )
    
    Y_predict = model %>% predict (
      X_test
    ) 
    
    # library(deepviz)
    # library(magrittr)
    # 
    # model %>% plot_model()

    Y_predict_original = Y_predict
    
    if(doStandarziationY)
    {
      Y_predict = DeRange01(Y_predict_original,y_min,y_max)
    }
    DF_Result = DF_Test[1:4]
    
    DF_Result["ES"]=   DF_Test["ES"] + Y_predict   # +0.0025 ABT
    DF_Result["VAR"]=  DF_Test["VAR"] # + Predict$net.result[,1]
    DF_Result["ES_D"]=  Y_predict  
    DF_Result["VAR_D"]= 0 #Predict$net.result[,1] 
    
    DF_Result["ES_D_PREV"] = DF_Test["ES_D_PREV"] 
    DF_Result["ES_D_REAL"] = DF_Test["ES_D_REAL"] 
    DF_Result["ES_REAL"] =  DF_Test["ES_REAL"]
    
    DF_Result["ES_BASE"]=  DF_Test["ES"]
    DF_Result["ESMA_BASE"]=  DF_Test["ES_MA"]
    DF_Result["VAR_BASE"]=  DF_Test["VAR"] 
    DF_Result["AEmVpXVI_BASE"] = DF_Test["AEmVpXVI"]
    DF_Result["ESR_BASE"] = DF_Test["ESR"]
    DF_Result["ESR_MA_BASE"] = DF_Test["ESR_MA"]

    DF_Result = ZES_detailed(DF_Result)
    DF_Result["stock"] =stockName 
    DF_Result["year"] = substring(DF_Result$Date,1,4)
    DF_Result["month"] = substring(DF_Result$Date,1,7)
    DF_Result["method"] = method
    DF_Result["ES_PERF_ANN"] =  (DF_stock["ES"]-DF_stock["ESR"])/DF_stock["ESR"]
    DF_Result["ES_PERF_AS"] =  (DF_stock["ES_BASE"]-DF_stock["ESR"])/DF_stock["ESR"]
    
    DF_Test["stock"] =stockName 
    DF_Test["year"] = substring(DF_Test$Date,1,4)
    DF_Test["month"] = substring(DF_Test$Date,1,7)
    DF_Test["method"] = baseMethod

    DF_method = df_append(DF_method,DF_Result)

    DF_arcsine = df_append(DF_arcsine,DF_Test)
    
    DF_stock = df_append(DF_stock,DF_Result)
    

    i= i+keep_model_for;
  }
  library(reticulate)

  DF_stock["ES_PERF_ANN"] =  (DF_stock["ES"]-DF_stock["ESR"])/DF_stock["ESR"]
  DF_stock["ES_PERF_AS"] =  (DF_stock["ES_BASE"]-DF_stock["ESR"])/DF_stock["ESR"]

  path = GetStockOutputPath(method,stockName)
  write.table(DF_stock, path ,sep="\t",col.names=NA)

  
  outputYearlySummaryToCSV(DF_method,method)
  #outputYearlySummaryToCSV(DF_arcsine,"arcsine")
  
  print(paste(Sys.time(), "-removing:",inprogressPath))
  file.remove(inprogressPath)
  }
}

path = paste(resultsFullPath,"Z_",method,".txt",sep = "")
write.table(Z, path ,sep="\t",col.names=NA)



outputYearlySummaryToCSV(DF_method,method)

#0.00546978111022437
#0.00223290145839381 train_window_size = 250

return(0)
}
