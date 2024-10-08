
source("constants.R")
source("mle.R")
source("standardise.R")
source("VaRLib.R")
source("DistParamLib.R")
source("DALC.R")
library(VaRES)
library(cvar)
library(stats4)

MultiPeriodES <-function(singleStock, selected = 'none', method='none', isLoss=TRUE)
{
  X = singleStock$LogReturn
  N = length(X)
  
  path= GetStockOutputPath(method,stockName)
  
  DF <- data.frame(X=rep(NA,N-T), ES=rep(NA,N-T), VAR=rep(NA,N-T), Date=rep(NA,N-T), stringsAsFactors = FALSE )
  
  print(path)
  
  if(file.exists(path))
  {
    #col.names = list('','X','ES','VAR','Date' )
   #DF <- read.table(path,sep="\t",header = TRUE, row.names = 1)
#    write.table(DF, path ,sep="\t",col.names=NA)
  }
  else
    {
      for(i in 1:(N-T))
      {
        
        # print(paste("starting",i))
        ssPeriod = X[i:(i+T-1)]
        xi= X[(i+T)]
        DF$X[i] = xi
        
        if(!isLoss)
        {
          losses = ssPeriod*-1
        }
        else
        {
          losses = ssPeriod
        }
        
        subLoc = 0
        divScale = 1
        
        subLoc = GetSubLoc(method,losses)

        #print(subLoc)        
        #print(mean(losses))
        
        losses = losses - subLoc
        
        divScale = GetDivScale(method,losses)
        
        
        losses = losses / divScale
        
        #print(mean(losses))
        
        assign("x", losses, envir = .GlobalEnv)
        assign("subLoc", subLoc, envir = .GlobalEnv)
        assign("last_ES_i", i , envir = .GlobalEnv)
        params = CalculateParams(losses,method,i)
        assign("params",params, envir = .GlobalEnv)
        
        es = CalculateES(losses,confLevel,method,params)
        var = CalculateVaR(losses,confLevel,method, params)
        
        es = es * divScale
        es = es - subLoc
        
        var = var * divScale
        var = var - subLoc
            
        DF$ES[i] = es
        
        DF$VAR[i] = var
        
        print
        
        if(method=='t5' | is.null(params) )
        {
          DF$P1 = 0
          DF$P2 = 0
          DF$P3 = 0
          DF$P4 = 0
          DF$P5 = 0
          DF$P6 = 0
        }
        else
        {
          DF$P1 = params@coef[1]
          DF$P2 = params@coef[2]
          DF$P3 = params@coef[3]
          DF$P4 = params@coef[4]
          DF$P5 = params@coef[5]
          DF$P6 = params@coef[6]
        }
        DF$Scale = divScale
        DF$Location = subLoc
     # debug   print(paste(selected,i,"m:" , method ,"es: ",es,"var:",var,"@",Sys.time(), " p1:" , DF$P1 , "p2:",DF$P2, "p3:",DF$P3 , "p4:",DF$P4  ))
        length(ssPeriod)
        #print(paste("finished",i))
      }
        
      
      DF$Date = singleStock$Date[(T+1):N]
      write.table(DF, path ,sep="\t",col.names=NA)
    }

  
  
  return(DF)
}





CalculateES <-function(losses,conf, method,params)
{
  if(method=='QR')
  {
    forecast = params
      
    z =  singleStock$LogReturn[250]
    
    z[z > 0] = 0
    z[z < 0] = 1
    
    forecast$xq = c(1,z)
    forecast$xe = c(1,z)
    
    estimates =  fitted(forecast)
    
    VaR = estimates[1]*-1
    ES = estimates[2]*-1
    
    return(ES)
    
  }
  if(method=='gjrgarch-arma-std' || method =='sgarch-arma-std' || method =='egarch-arma-std' || method == 'sgarch-arma-std-2-2'|| method =='igarch-arma-std' )
  {
    sl = 1 - conf
    
    modelfor = params
    
    shape = modelfor@model$pars[17]
    
    f = function(x) qdist("std", p=x, mu = 0, sigma = 1,shape=shape)
    
    es= ((fitted(modelfor) + sigma(modelfor)*integrate(f, 0, sl)$value/sl) * -1)
    
    return(es[2])
  }
  if(method=='gjrgarch-arma-norm' || method =='sgarch-arma-norm' || method =='egarch-arma-norm' || method=='igarch-arma-norm')
  {
    sl = 1 - conf
    
    modelfor = params

    f = function(x) qdist("norm", p=x, mu = 0, sigma = 1)
    
    es= ((fitted(modelfor) + sigma(modelfor)*integrate(f, 0, sl)$value/sl) * -1)
    
    return(es[2])
  }
  if(method=='gjrgarch-arma' || method=='sgarch-arma-sstd' || method=='egarch-arma-sstd' || method=='igarch-arma-sstd')
  {
    sl = 1 - conf
    
    modelfor = params
    skew = modelfor@model$pars[16]
    
    shape = modelfor@model$pars[17]
  
    f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1,  skew  = skew, shape=shape)
    
    es= ((fitted(modelfor) + sigma(modelfor)*integrate(f, 0, sl)$value/sl) * -1)
    
    return(es[2])
  }
  else if(method == 'historical') {
    return (mean(losses[losses > quantile(losses, conf)]))
  }
  
  if(method == 'normal')
  {
    s = sd(losses)
    m = mean(losses)
    return(esnormal(1-conf,s,mu = m)*-1)
  }
  if(method == 't')
  {
    s = sd(losses)
    m = mean(losses)
    return(esT(1-conf,n =params@coef[1])*-1)
  } 
  if(method == 't4')
  {
    s = sd(losses)
    m = mean(losses)
    return( cvar::ES(qt,dist.type = "qf", df=4 ,intercept = m, slope = s ))
  }
  if(method == 't2')
  {
    s = sd(losses)
    m = mean(losses)
    return( cvar::ES(qt,dist.type = "qf", df=2 ,intercept = m, slope = s ))
  }
  if(method == 't5')
  {
 
    s = sd(losses)
    m = mean(losses)
    return( cvar::ES(qt,dist.type = "qf", df=5 ,intercept = m, slope = s ))
  }
  
  if(method== 'ast') #Generalized asymmetric Student's t distribution
  {
    return((esast(1-conf,nu1=params@coef[1], nu2= params@coef[2], alpha= params@coef[3]) - as.numeric(params@coef[4])) *-1 )
  }
  if(method=='aep')
  {
    return(esaep(1-conf,q1 = params@coef[1], q2=params@coef[2], alpha=params@coef[3])*-1)
  }
  if(method=='aep-std')
  {
    s = sd(losses)
    m = mean(losses)
    es = cvar::ES(paep,dist.type = "cdf", intercept = m, slope = s )
    
    return(es)
  }
  if(method=='arcsine')
  {
    a = params@coef[1]
    b = params@coef[2]

    es = esarcsine(1-conf,a = a, b=b)*-1
    return(es)
  }
  if(method=='asylaplace')
  {
    tau = params@coef[1]
    kappa = params@coef[2]
    theta = params@coef[3]
    
    es = esasylaplace(1-conf,tau=tau,kappa=kappa,theta=theta)*-1
    return(es)
  }
  else if (method=="asypower")
  {
    return(esasypower(1-conf,a=params@coef[1], lambda = params@coef[2], delta = params@coef[3]) *-1 )
  }
  else if (method=="betadist")
  {
    return( (esbetadist(1-conf,a=params@coef[1], b = params@coef[2]) - as.numeric(params@coef[3])) * -1 )
  }
  else if (method=="beard")
  {
    return( (esbeard(1-conf,a=params@coef[1], b = params@coef[2], rho = params@coef[3] ) ) * -1 )
  }
  else if (method=="burr")
  {
    return( (esburr(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="burr7")
  {
    return( (esburr7(1-conf,k=params@coef[1], c = params@coef[2]) ) * -1 )
  }
  else if (method=="Cauchy")
  {
    return(esCauchy(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if (method=="chen")
  {
    return(eschen(1-conf,b=params@coef[1], lambda = params@coef[2]) *-1 )
  }
  else if (method=="clg")
  {
    return(esclg(1-conf,a=params@coef[1], b = params@coef[2], theta= params@coef[3]) *-1 )
  }
  else if (method=="compbeta")
  {
    return(escompbeta(1-conf,a=params@coef[1], b = params@coef[2]) *-1 )
  }
  else if (method=="dagum")
  {
    return(esdagum(1-conf,a=params@coef[1], b = params@coef[2], c= params@coef[3]) *-1 )
  }
  else if (method=="expexp")
  {
    return(esexpexp(1-conf,lambda=params@coef[1], a = params@coef[2]) *-1 )
  }
  else if (method=="expext")
  {
    return(esexpext(1-conf,lambda=params@coef[1], a = params@coef[2]) *-1 )
  }
  else if (method=="expgeo")
  {
    return(esexpgeo(1-conf,theta = params@coef[1], lambda = params@coef[2]) *-1 )
  }
  else if (method=="explog")
  {
    return(esexplog(1-conf,a = params@coef[1], b = params@coef[2]) *-1 )
  }
  else if (method=="explogis")
  {
    return(esexplogis(1-conf,a = params@coef[1], b = params@coef[2]) *-1 )
  }
  else if (method=="exppois")
  {
    return(esexppois(1-conf,b=params@coef[1], lambda = params@coef[2]) *-1 )
  }
  else if (method=="f")
  {
    return((esF(1-conf,d1=params@coef[1], d2= params@coef[2]) - as.numeric(params@coef[3])  )  *-1)
  }  
  else if (method=="frechet")
  {
    return( (esfrechet(1-conf,alpha=params@coef[1], sigma= params@coef[2]) - as.numeric(params@coef[3])) * -1 )
  }
  else if(method=="Gamma")
  {
    return((esGamma(1-conf,a=params@coef[1], b= params@coef[2]) - as.numeric(params@coef[3])) *-1 )
  }
  else if (method=="genpareto")
  {
    c= params@coef[2]
    if(c==0) 
    {
      c= 0.000000000001
    }
    return( (esgenpareto(1-conf,k=params@coef[1], c= c) ) * -1 )
  }
  else if (method=="genpowerweibull")
  {
    return(esgenpowerweibull(1-conf,a=params@coef[1], theta = params@coef[2]) *-1 )
  }
  else if(method=="gumbel")
  {
    return(esgumbel(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if(method=="laplace")
  {
    return(eslaplace(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if(method=="logcauchy")
  {
    return(eslogcauchy(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if(method=="logistic")
  {
    return(eslogistic(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if (method=="invgamma")
  {
    return( (esinvgamma(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="kum")
  {
    return( (eskum(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="kumburr7")
  {
    return( (eskumburr7(1-conf,a=params@coef[1], b = params@coef[2] , k = params@coef[3] , c = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumexp")
  {
    return( (eskumexp(1-conf,lambda=params@coef[1], a = params@coef[2] , b = params@coef[3] ) ) * -1 )
  }
  else if (method=="kumgamma")
  {
    return( (eskumgamma(1-conf,a=params@coef[1], b = params@coef[2] , c = params@coef[3] , d = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumgumbel")
  {
    return( (eskumgumbel(1-conf,a=params@coef[1], b = params@coef[2] , mu = params@coef[3] , sigma = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumhalfnorm")
  {
    return( (eskumhalfnorm(1-conf,sigma=params@coef[1], a = params@coef[2] , b = params@coef[3] ) ) * -1 )
  }
  else if (method=="kumloglogis")
  {
    return( (eskumloglogis(1-conf,a=params@coef[1], b = params@coef[2] , alpha = params@coef[3] , beta = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumnormal")
  {
    return( (eskumnormal(1-conf,mu=params@coef[1], sigma = params@coef[2] , a = params@coef[3] , b = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumpareto")
  {
    return( (eskumpareto(1-conf,K=min(x), a = 1 , b = params@coef[2] , c = params@coef[3] ) - as.numeric(params@coef[1]) )   * -1 )
  }
  else if (method=="kumweibull")
  {
    return( (eskumweibull(1-conf,a=params@coef[1], b = params@coef[2] , alpha = params@coef[3] , sigma = params@coef[4] ) ) * -1 )
  }
  else if (method=="lfr")
  {
    return( (eslfr(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="LNbeta")
  {
    return( (esLNbeta(1-conf,lambda =params@coef[1], a = params@coef[2] , b = params@coef[3]  ) ) * -1 )
  }
  
  else if (method=="loggamma")
  {
    return( (esloggamma(1-conf,a=params@coef[1], r = params@coef[2]) ) * -1 )
  }
  else if (method=="logisexp")
  {
    return( (eslogisexp(1-conf,lambda=params@coef[1], a = params@coef[2]) ) * -1 )
  }
  
  else if (method=="logisrayleigh")
  {
    return( (eslogisrayleigh(1-conf,a=params@coef[1], lambda = params@coef[2]) ) * -1 )
  }
  else if (method=="loglaplace")
  {
    return( (esloglaplace(1-conf,a =params@coef[1], b = params@coef[2] , delta = params@coef[3]  ) ) * -1 )
  }
  else if (method=="loglog")
  {
    return( (esloglog(1-conf,a=params@coef[1], lambda = params@coef[2]) ) * -1 )
  }
  else if (method=="loglogis")
  {
    return( (esloglogis(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="lognorm")
  {
    return( (eslognorm(1-conf,mu=params@coef[1], sigma = params@coef[2]) ) * -1 )
  }
  else if (method=="lomax")
  {
    return( (eslomax(1-conf,a=params@coef[1], lambda = params@coef[2]) ) * -1 )
  }
  else if (method=="Mlaplace")
  {
    return( (esMlaplace(1-conf,theta =params@coef[1], phi = params@coef[2] , psi = params@coef[3]  ) ) * -1 )
  }
  else if (method=="moexp")
  {
    return( (esmoexp(1-conf,lambda =params@coef[1], a = params@coef[2]  ) ) * -1 )
  }
  else if (method=="moweibull")
  {
    return( (esmoweibull(1-conf,a =params@coef[1], b = params@coef[2] , lambda = params@coef[3]  ) ) * -1 )
  }
  else if (method=="MRbeta")
  {
    return( (esMRbeta(1-conf,a =params@coef[1], b = params@coef[2] , r = params@coef[3] , q = params@coef[4]  ) ) * -1 )
  }
  else if (method=="nakagami")
  {
    return( (esnakagami(1-conf,m =params@coef[1], a = params@coef[2]  ) ) * -1 )
  }
  else if (method=="paretostable")
  {
    return( (esparetostable(1-conf,lambda =params@coef[1], nu = params@coef[2] , sigma = params@coef[3]  ) ) * -1 )
  }
  else if (method=="PCTAlaplace")
  {
    return( (esPCTAlaplace(1-conf,a =params@coef[1], theta = params@coef[2]  ) ) * -1 )
  }
  else if (method=="rgamma")
  {
    return( (esrgamma(1-conf,a =params@coef[1], theta = params@coef[2] ,  phi = params@coef[3]  ) ) * -1 )
  }
  else if (method=="schabe")
  {
    return( (esschabe(1-conf,gamma =params@coef[1], theta = params@coef[2]  ) ) * -1 )
  }
  else if (method=="stacygamma")
  {
    return( (esstacygamma(1-conf,params@coef[1],  params@coef[2] ,  params@coef[3]  ) ) * -1 )
  }
  else if(method=="TL2")
  {
    return(esTL2(1-conf,b=params@coef[1]) *-1 )
  }
  else if(method=="tsp")
  {
    return(estsp(1-conf,a=params@coef[1], theta= params@coef[2]) *-1 )
  }
  else if (method=="triangular")
  {
    return(estriangular(1-conf,a=params@coef[1], b= params@coef[2], c = as.numeric(params@coef[3])) *-1 )
  }
  else if (method=="perks")
  {
    return(esperks(1-conf,a=params@coef[1], b= params@coef[2]) *-1 )
  }
  else if (method=="quad")
  {
    return(esquad(1-conf,a=params@coef[1], b= params@coef[2]) *-1 )
  }
  else if (method=="uniform")
  {
    return( (esuniform(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="Weibull")
  {
    return( (esWeibull(1-conf,alpha=params@coef[1], sigma= params@coef[2]) - as.numeric(params@coef[3])) * -1 )
  }
  else if (method=="xie")
  {
    return( (esxie(1-conf,params@coef[1],  params@coef[2] ,  params@coef[3]  ) ) * -1 )
  }
  
  
}
