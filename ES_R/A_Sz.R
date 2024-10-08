source("constants.R")
source("ESlib.R")
source("HistoricalSimulation.R")
library(ggplot2)
library(reshape2)

Z2_2017 <- function(DF) {

   conf =  1- confLevel
   corrA = conf
  # corrA = math.ceil(T*conf) / T # correcting confidence so perfect match gives 0
   DF['I'] = DF['VAR'] + DF['X']
   I = DF['I']
   
   I[I > 0] = 0
   I[I < 0] = 1
   
   DF['XI'] = I* DF['X']
   DF['AE'] = DF['ES']* (corrA)
   DF['AEpXI'] = DF['AE']+ DF['XI']
   
   Z22 = mean(DF$AEpXI)
   
   return (Z22)
}


ZES_detailed <- function(ES_pred){  #2017-es cikk (48)
  
  confidence =  1-confLevel
  
  #X has to be the real timeseries
  ES_pred['I'] = ES_pred['VAR'] + ES_pred['X']
  I = ES_pred['I']
  
  I[I > 0] = 0
  I[I < 0] = 1
  
  ES_pred['I'] = I
  
  ES_pred['XVI'] = I*(ES_pred['VAR'] + ES_pred['X']) 
  ES_pred['AEmV'] = confidence * (ES_pred['ES'] - ES_pred['VAR'])
  ES_pred['AEmVpXVI'] = ES_pred['XVI'] + ES_pred['AEmV']
  ES_pred['ESR'] = ES_pred['VAR'] + (1/confidence) * ES_pred['XVI'] *-1
  
  return(ES_pred)
}


ESR <- function(ES_pred) {
  ES_pred = ZES_detailed(ES_pred)
  ESR = mean(ES_pred$ESR)
  
  return (ESR)
}

ZES <- function(ES_pred) {
  ES_pred = ZES_detailed(ES_pred)
  ZES = mean(ES_pred$AEmVpXVI)

return (ZES)
}


drawFigure5 <-function() {
   dev.off()
   plot(xlim = c(-10,10),ylim=c(0,0.5), density(rt(10000,3)), col='magenta',lwd=2)
   lines(density(rt(10000,5)), col='green',lwd=2) 
   lines(density(rt(10000,10)), col='red',lwd=2)  
   lines(density(rt(10000,100)), col='blue',lwd=2)  
   
   
   
   # lines(density(rt(10000,100)), col='blue')  
   # hist(rt(10000,3),col='magenta' , breaks=100, prob=TRUE)
   # hist(rt(10000,5),col='green' , breaks=100,prob=TRUE, add=TRUE)
   # hist(rt(10000,10),col='red' , breaks=100,prob=TRUE, add=TRUE)
   # hist(rt(10000,100),col='blue' , breaks=100,prob=TRUE, add=TRUE)

   legend("topleft", legend = c("df:3", "df:5", "df:10", "df:100"),
          col = c("magenta", "green","red","blue"), lty = 1, cex = 0.8)
   
   print("Acerbi-Székely Figure 5: Student-t distributions")
   print("df=100, df=10, df=5, df=3")
   
   M= 100000
   
   T = data.frame(DF3=rep(NA,M), stringsAsFactors = FALSE )
   T$DF3<-rt(M,3)
   T$DF5<-rt(M,5)
   T$DF10<-rt(M,10)
   T$DF100<-rt(M,100)
   
   TData<- melt(T)
   ggplot(TData,aes(x=value, fill=variable)) + geom_density(alpha=0.2, size=0.6) + xlim(-5,5)
   
}

Simulate <-function(rowCount,df,X_real){
   rowCount = length(X_real)
   #df  = 3
   
   DF <- data.frame(X=rep(NA,rowCount), ES=rep(NA,rowCount), VAR=rep(NA,rowCount), stringsAsFactors = FALSE )
   
   for(i in 0:(rowCount))
   {
      X_sim = rt(10000,df)
      es = EShistorical(X_sim,confLevel)
      var = VaRhistorical(X_sim,confLevel)
      
      DF$ES[i] = es
      DF$VAR[i] = var
      DF$X[i] = X_real[i]
   }
   
   return(DF)
}
   
Zsim <-function(df_real,Zfunc,df_pred=100)
{
   #X_real = rt(250, df_real)
   X_real = rt(250, df_pred)
   ES_pred = Simulate(250,df_pred,X_real)
   
   
   Z0 = ZES(ES_pred)
   
   M= 10000
   
   Z = data.frame(Z=rep(NA,M), stringsAsFactors = FALSE )
   ZSum = 0
   for (i in 1:M){
      simulated = rt(250, df_real)
      Hi  = ES_pred
      Hi$X = simulated
      Z$Z[i] = Zfunc(Hi)
      if(Z$Z[i] < Z0)
      {
         ZSum = ZSum + 1
      }
      
      
   }
   #print(ZSum/M)
   
   return(data.matrix(Z))
}


ZTest <- function()
{
  library(reshape)
  
   dev.off()
   
   Z <- data.frame(DF3=rep(NA,M), DF5=rep(NA,M), DF10=rep(NA,M), DF100=rep(NA,M) , stringsAsFactors = FALSE )
   
   
   Z$DF3 = Zsim(df_real = 3,Zfunc =  ZES)
   Z$DF5 = Zsim(df_real = 5,Zfunc =  ZES)
   Z$DF10 = Zsim(df_real = 10,Zfunc =  ZES)
   Z$DF100 = Zsim(df_real = 100,Zfunc =  ZES)
   Z$ID <- seq.int(nrow(Z))
   Z$ID = NULL
   sapply(Z,class)

   getPower(ZPred = Z$DF100,ZReal = Z$DF3,Signl =  0.05)
   getPower(ZPred = Z$DF100,ZReal = Z$DF5,0.05)
   getPower(ZPred = Z$DF100,ZReal = Z$DF10,0.05)
   getPower(ZPred = Z$DF100,ZReal = Z$DF100,0.05)
   
   getP(ZPred = Z$DF100,ZReal = Z$DF3)
   getP(ZPred = Z$DF100,ZReal = Z$DF5)
   getP(ZPred = Z$DF100,ZReal = Z$DF10)
   getP(ZPred = Z$DF100,ZReal = Z$DF100)
   

   data<- reshape::melt(Z)
   
   ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25) +xlim(-0.2,0.02)
}


plotCdf <- function(df_real,colorStr,df_pred=100,a=1,b=1, label="",add=TRUE) {
  
  
  # 
  # if(!add)
  # {
  #    
  #    plot(density(aSim$Z), col=colorStr, xlim = c(-0.4,0.2),ylim=c(0,80) )
  # }
  # else
  # {
  #    lines(density(aSim$Z), col=colorStr)
  # }
  # 
  #hist(aSim$Z,breaks = 100, prob=TRUE, col=colorStr,add=add)
  #hist(aSim$Z,xlim = c(-0.4,0.2),ylim=c(0,50),breaks = 25, prob=TRUE, col=colorStr,add=add)
  #lines(density(aSim$Z), col=colorStr)  
  
  
  
  
  print(colorStr)   
  #df_real = 10
  aSim = Zsim(df_real = df_real,df_pred = df_pred,Zfunc = ZES)
  cdf= ecdf(aSim)
  
  cdfDF = data.frame(ecdf(aSim))
  cdfDF$ID <- seq.int(nrow(cdfDF))
  ggplot(cdfDF, aes(x=Z)) + geom_density(alpha=0.25)
  

  
}

getPower <-function(ZPred,ZReal,Signl)
{
  ZSignl = quantile(ZPred,probs=Signl)
  ZiX = data.frame(ZReal)
  return(length(ZiX[ZiX$Z<ZSignl,])/M)
}
getP <-function(ZPred,ZReal)
{
  Zp = ZReal[1]
  ZiX = data.frame(ZReal)
  return(length(ZiX[ZiX$Z<Zp,])/M)
}
  

Ztest2 <-function()
{
  data<- reshape::melt(Z)
  
  
  df <- data.frame(
    x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
    g = gl(2, 100)
  )
  
  test = c(rnorm(100, 0, 3), rnorm(100, 0, 10))
  
  
  ggplot(data, aes(x=value, colour = variable)) + stat_ecdf()
  
  a = -250
  b = 250
  c = 1000
  
  
  ZCDF <- data.frame(x=(a:b)/c, DF3= 1-ecdf(Z$DF3)((a:b)/c), DF5=1-ecdf(Z$DF5)((a:b)/c), DF10=1-ecdf(Z$DF10)((a:b)/c), DF100=ecdf(Z$DF100)((a:b)/c) , stringsAsFactors = FALSE )
  dataCDF<- reshape::melt(ZCDF, id.vars="x")
  #ggplot(dataCDF,aes(x=value, y=x)) + geom_density(alpha=0.25) #+ theme(panel.background = element_blank())
  
  ggplot(ZCDF,aes(x=x)) + geom_line(data=ZCDF,aes(y=DF3,color='DF3'))  + geom_line(data=ZCDF,aes(y=DF5,color='DF5'))   + geom_line(data=ZCDF,aes(y=DF10,color='DF10'))   + geom_line(data=ZCDF,aes(y=DF100,color='DF100')) 
  
  ggplot(dataCDF,aes(x=x, fill=variable)) + geom_line(data=dataCDF,aes(y=value,color=variable), size=0.8) + geom_hline(yintercept=0.05) + geom_vline(xintercept= ZSignl)+xlim(-0.2,0.02)
  
  
  ggplot(ZCDF,aes(x=x)) + geom_line(data=ZCDF,aes(y=DF3,color='DF3'))  + geom_line(data=ZCDF,aes(y=DF5,color='DF5'))   + geom_line(data=ZCDF,aes(y=DF10,color='DF10'))   + geom_line(data=ZCDF,aes(y=DF100,color='DF100')) 
  
  
  x <- data.frame(v1=rnorm(100),v2=rnorm(100,1,1),v3=rnorm(100,0,2))
  
  sapply(x,class)
  library(ggplot2);library(reshape2)
  data<- melt(x)
  ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)
  
  M= 10000
  
  
  plotCdf(df_real=3,'magenta', add=false)
  plotCdf(df_real=5,'green')
  plotCdf(df_real=10,'red')
  plotCdf(df_real=100,'blue')
  
  aa = zsim(df_real=3,df_pred=100,z2_2017)
  hist(aa$z,xlim = c(-0.5,0.2),breaks = 100, prob=true, col='red')
  lines(density(aa$z))  
  
  
  
  hist(aa$z,xlim = c(-0.5,0.2),breaks = 100, prob=true,add= true, col='blue')
  lines(density(aa$z))  
}
