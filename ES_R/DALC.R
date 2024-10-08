
#setwd(getSrcDirectory(function(){})[1])

library(stringr)
library(dplyr)
library(tidyr)
source("constants.R")

GetStockOutputPath <-function(method, stockName)
{
  print(paste('Reading file: ',resultsFullPath,method, "_",stockName,".txt",sep=""))
  return(paste(resultsFullPath,method, "_",stockName,".txt",sep=""))
}

GetANNInputData <- function()
{
  files = list.files(path = paste(resultsFullPath),pattern=paste("^","arcsine","_.*(txt)$",sep="") , full.names = TRUE)
  DF_input <- read.table(paths[1],sep="\t",header = TRUE, row.names = 1)
  DF_input = ZES_detailed(DF_input)
  es_difference = DF_input
  es_difference["value"] = DF_input$ESR - DF_input$ES 
  es_difference = es_difference["value"]
  rownames(es_difference) <- DF_input[,4]
  
  ES_tibble <- es_difference %>%
    tk_tbl() %>%
    mutate(index = as_date(index)) %>%
    as_tbl_time(index = index)
  
  return(ES_tibble)
}

df_append <- function(DF_all,newDFRows)
{
  
  if(is.null(DF_all))
  {
    DF_all = newDFRows
  }
  else
  {
    DF_all = rbind(newDFRows,DF_all)
  }
  
  return (DF_all)
}


get_tickers <- function()
{
  path= paste(getwd(),dataPath,sep="")
   return(list.files(path = path,full.names = TRUE))
}
get_detailed_simulation_results_for_method <- function(method,Z_yearly,rewrite)
{
#  method = "clg"
  #rewrite = FALSE
  
  files = list.files(path = paste(resultsFullPath),pattern=paste("^",method,"_.*(txt)$",sep="") , full.names = FALSE)
  paths = list.files(path = paste(resultsFullPath),pattern=paste("^",method,"_.*(txt)$",sep="") , full.names = TRUE)
  print(paste(method, length(files),sep=" "))
  
  DF_method = NULL
  
  
  for(f in 1: length(files))
  {
    DF <- read.table(paths[f],sep="\t",header = TRUE, row.names = 1)
    
    #DF = DF[1:4]
    DF = ZES_detailed(DF)
   
    DF["method"] = method
    DF["stock"] =stockname = unlist(strsplit(get_stockname(files[f]),"[_]"))[2]
    DF["year"] = substring(DF$Date,1,4)
    
    DF["month"] = substring(DF$Date,1,7)
    print(paste(stockname,method,lengths(DF["X"]), sep=" "))
    
    DF_method = df_append(DF_method,DF)
    
    if(rewrite == TRUE)
    {
      write.table(DF, f ,sep="\t",col.names=NA)
    }
  }
  
  outpath= paste(resultsFullPath,"DF",method,".txt",sep="")
 # write.table(DF_method, outpath ,sep="\t",col.names=NA,dec=",")
  
  yearlySummary = NULL
  yearlySummary <- DF_method %>% 
    group_by(year,method,stock)  %>%
    summarise(Z = mean(AEmVpXVI))
  
  yearlySummary["ZAE"] = abs(yearlySummary$Z)
  outpath= paste(resultsFullPath,"DFYearlySummary",method,".txt",sep="")
  write.table(yearlySummary, outpath ,sep="\t",col.names=NA,dec=",")
  
  print(sum(yearlySummary$ZAE))
  Z_yearly = df_append(Z_yearly,yearlySummary)
  
  return(Z_yearly)
}

get_detailed_simulation_results <- function()
{
  path= paste(resultsFullPath,sep="")
  #methods = get_top_methods()
  methods = get_methods()
  #methods = c( "ANN11")
  Z_yearly = NULL

  for(m in 1:length(methods))
  {
    print(m)
    method = methods[m]
    print(method)
    Z_yearly = get_detailed_simulation_results_for_method(method,Z_yearly,FALSE)
  }
  

  outpath= paste(resultsFullPath,"DFYearlySummary.txt",sep="")
  write.table(Z_yearly, outpath ,sep="\t",col.names=NA,dec=",")
  
}

get_detailed_simulation_result_for_single_stock <- function(method)
{
  path= paste(resultsFullPath,sep="")

  print(method)
  
  Z_yearly = NULL
  Z_yearly = get_detailed_simulation_results_for_method(method,Z_yearly,FALSE)

  outpath= paste(resultsFullPath,"DFYearlySummary",method,".txt",sep="")
  write.table(Z_yearly, outpath ,sep="\t",col.names=NA,dec=",")
  
}

get_top_methods <- function()
{
  methods = c( "lfr", "loglog","genpowerweibull", "asylaplace", "TL2","explog","expext","kum","chen","burr7","ANN11")
  
  return(methods)
}
get_methods <- function()
{
  path= paste(resultsFullPath,sep="")
  files = list.files(path = path,pattern="Z_." , full.names = FALSE)
  
  
  methods = files
  for(i in 1:length(files))
  {
    file = files[i]
    method = substr(file, 3, str_length(file)-4)
    methods[i] = method
  }
  return(methods)
} 

get_stockname <- function(selected) {
  
  ci = length(unlist(strsplit(selected,"/")))
  filename = unlist(strsplit(selected,"/"))[ci]
  
  stockname = unlist(strsplit(filename,"[.]"))[1]
  #print(stockname)
    return (stockname)
}


get_stockname_by_index <- function(index) {
  tickers = get_tickers()
  return (get_stockname(tickers[index]))
}




get_returns_for_single_stock <- function(selected,start_date) {
  
  base=read.table(selected,header=TRUE, sep=",")
  
  base$Date = as.Date(base$Date) #Change date column data type from factor to date. 
  
  #print(sapply(base$Close[1], class))
  
  baseFiltered <- subset(base,(end_date >= base$Date) & (base$Date >= start_date) )
  
  if(sapply(baseFiltered$Close[1], class) == "factor")
  {
    baseFiltered$Close = as.numeric(levels(baseFiltered$Close))[baseFiltered$Close]
  }
  if(sapply(baseFiltered$Close[1], class) == "character")
  {
    baseFiltered$Close = as.numeric(baseFiltered$Close)
  }
  
  n <- length(baseFiltered$Close)
  lrest <- log(baseFiltered$Close[-1]/baseFiltered$Close[-n])
  
  baseWithLog = baseFiltered[-1,]
  baseWithLog$LogReturn = lrest
  
  #log(baseFiltered$Close[3]/ baseFiltered$Close[2])
  return(baseWithLog)
}
  
outputYearlySummaryToCSV <- function(DF_method,method)
{
  yearlySummary <- DF_method %>% 
    group_by(year,method,stock)  %>%
    summarise(Z = (mean(AEmVpXVI)))
  
  yearlySummary = yearlySummary[!is.na(yearlySummary$Z),]
  
  yearlySummary["Z_abs"] = abs(yearlySummary["Z"])
  
  outpath= paste(resultsFullPath,"DFYearlySummary",method,".txt",sep="")
  write.table(yearlySummary, outpath ,sep="\t",col.names=NA,dec=",")
  
  print(paste(Sys.time(), ":", method,":", sum(yearlySummary[yearlySummary$method==method,]$Z), ' ', sum(yearlySummary[yearlySummary$method==method,]$Z_abs) ))
}

RemoveAllStartedMarkers <-function()
{

  files = list.files(path = resultsFullPath,pattern=".started." , full.names = TRUE)
  
  if(length(files)>0)
  {
    
    for(i in 1:length(files))
    {
      file = files[i]
      modTime = file.info(file)$mtime
      theDiff = as.numeric(difftime(Sys.time(),modTime,units = "mins"))
      
      if(is.na(theDiff))
      {
        next
      }
      
      if(theDiff > 120 )
      {
        print(paste(Sys.time(), "-removing:",file))
        file.remove(file)
      }
    }
  }
}


GetANNMethodName <- function(train_window_size,keep_model_for, isESMA, doStandarziationY, extra="", baseMethod="AS")
{
  if(baseMethod == "arcsine")
  {
    baseMethod = "AS"
  }
  
  standStr = ""
  
  if(doStandarziationY)
  {
    standStr = "S"
  }
  
  ESMAStr = ""
  
  if(isESMA)
  {
    ESMAStr = "ESMA"
  }
  
  methodName = paste("ANN-",baseMethod,"-",standStr,extra, ESMAStr, train_window_size,"T",keep_model_for,"K","-SIGM", sep="")
  print(methodName)
  return(methodName)
}


