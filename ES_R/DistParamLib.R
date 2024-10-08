source("GridSearch.R")
source("GenParamLib.R")
source("constants.R")
library(rugarch)

CalculateParams<-function(series,method,i)
{
  #Valid choices are
  #"norm", "snorm", "std", 
  #"sstd" Skew Student-T Distribution
  #, "ged" Generalized Error Distribution , "sged", "nig", "jsu

  solver = 'hybrid'

  if(method == 'ANN-1-logistic')
  {
    
  }
  
  if(method=='QR')
  {
    x = series[1:249]
    
    x[x > 0] = 0
    x[x < 0] = 1
    
    y = series[2:250]
    
    fit = esreg(y ~ x, alpha=1-confLevel)
    
    params<-fit
  }
  if(method=='igarch-arma-std')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "iGARCH"),  distModel = "std", solver = solver)
  }
  if(method=='igarch-arma-sstd')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "iGARCH"),  distModel = "sstd", bounds = list(shape=c(1,59), skew=c(1,29)), solver = solver)
  }
  if(method=='igarch-arma-norm')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "iGARCH"),  distModel = "norm", solver =solver)
  }
  if(method=='egarch-arma-sstd')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "eGARCH"),  distModel = "sstd", bounds = list(shape=c(1,59), skew=c(1,29)), solver = solver)
  }
  if(method=='egarch-arma-std')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "eGARCH"),  distModel = "std", solver =solver)
  }
  if(method=='egarch-arma-norm')
  {
    solver = 'gosolnp'
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "eGARCH"),  distModel = "norm", solver =solver)
  }
  
  if(method=='sgarch-arma-sstd')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "sGARCH"),  distModel = "sstd", bounds = list(shape=c(1,59), skew=c(1,29)), solver = solver)
  }
  if(method=='sgarch-arma-std')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "sGARCH"),  distModel = "std")
  }
  if(method=='sgarch-arma-std-2-2')
  {
    solver = 'gosolnp'
    params <- G_process(meanModel = list(armaOrder = c(3,3), include.mean = TRUE), varianceModel =  list(model = "sGARCH",  garchOrder = c(3, 3)),  distModel = "std")
  }  
  if(method=='sgarch-arma-norm')
  {
    solver = 'hybrid'
    if(i == 6028)
    {
      solver = 'gosolnp'
      print(solver)
    }
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "sGARCH"),  distModel = "norm", solver =solver)
  }
  if(method=='gjrgarch-arma-std')
  {
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "gjrGARCH"),  distModel = "std")
  }
  
  if(method=='gjrgarch-arma-norm')
  {
    solver = 'hybrid'
    if(i == 6530)
    {
      solver = 'gosolnp'
    }
    
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "gjrGARCH"),  distModel = "norm", solver =solver)
  }
  if(method=='gjrgarch-arma')
  {
    #, include.mean = TRUE), variance.model = list(model = "gjrGARCH")
    params <- G_process(meanModel = list(armaOrder = c(1,1), include.mean = TRUE), varianceModel =  list(model = "gjrGARCH"),  distModel = "sstd", bounds = list(shape=c(1,59), skew=c(1,29)))
  }
  else if(method== 'ast') #Generalized asymmetric Student's t distribution
  {
    func <- LLast
    
    start <-list(nu1=0.1, nu2=0.1, alpha = 0.5, loc = 0.01)
    
    if(!is.null(params))
    {
      start <-list(nu1 =as.numeric(params@coef[1]) , nu2 = as.numeric(params@coef[2])
                   , alpha = as.numeric(params@coef[3]) , loc = as.numeric(params@coef[4] ))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <-list(nu1=0.1, nu2=0.1, alpha = 0.5, loc = 0.01)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params<-SearchastStartParameters()
    }
    
    
  }
  if(method=='aep')
  {
    func<-LLaep
    start <- list(q1=5,q2=10, alpha = 0.5)
    
    if(!is.null(params))
    {
      start <-list(q1 =as.numeric(params@coef[1]) , q2 = as.numeric(params@coef[2])
                   , alpha = as.numeric(params@coef[3]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(q1=5,q2=10, alpha = 0.5)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params<-SearchaepStartParameters()
    }
    
  }
  if(method=='arcsine')
  {
    oldParams = params
    
    func<-LLarcsine
    start <- list(a= min(series)-0.001,b=max(series)+0.001)
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params = oldParams
      print("oldParams used")
    }
  }
  if(method=='asylaplace')
  {
    func<-LLasylaplace
    start <- list(tau=1,kappa=1,theta=0)
    params<-C_MLE(func,start, 1)
  }
  else if (method=='asypower')
  {
    params <- FitDistrThreeParameter(0.5,1,1, LLasypower,SearchasypowerStartParameters)
  }
  else if(method=="beard")
  {
    params <- FitDistrThreeParameter(1,-8,39, LLbeard ,SearchbeardStartParameters)
  }
  else if (method=='betadist')
  {
    func<-LLbetadist
    start <- list(a=0.1, b=0.1,loc=0.001)
    if(!is.null(params))
    {
      start <-list(a =as.numeric(params@coef[1]) , b = as.numeric(params@coef[2]), loc = as.numeric(params@coef[3] ))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params <- SearchbetadistParameters()
    }
    
  }
  
  else if (method=='burr')
  {
    params <- FitDistrTwoParameter(1,1,LLburr,SearchburrStartParameters)
  }
  
  else if (method=='burr7')
  {
    params <- FitDistrTwoParameter(1,1,LLburr7,Searchburr7StartParameters)
  }
  
  else if (method=='Cauchy')
  {
    func<-LLCauchy
    start <- list(mu=0,sigma=1)
    if(!is.null(params))
    {
      start <-list(mu =as.numeric(params@coef[1]) , sigma = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params <- SearchCauchyStartParameters()
    }
    
  }
  
  else if (method=='chen')
  {
    params <- FitDistrTwoParameter(1,1,LLchen,SearchchenStartParameters)
  }
  else if (method=='clg')
  {
    params <- FitDistrThreeParameter(1,1,1,LLclg,SearchclgStartParameters)
  }
  else if (method=='compbeta')
  {
    params <- FitDistrTwoParameter(1,1,LLcompbeta,SearchcompbetaStartParameters)
  }
  else if (method=='dagum')
  {
    params <- FitDistrThreeParameter(1,1,1,LLdagum,SearchdagumStartParameters)
  }
  
  else if (method=='expexp')
  {
    params <- FitDistrTwoParameter(1,1,LLexpexp,SearchexpexpStartParameters)
  }
  
  else if (method=='expext')
  {
    params <- FitDistrTwoParameter(1,1,LLexpext,SearchexpextStartParameters)
  }
  
  else if (method=='expgeo')
  {
    params <- FitDistrTwoParameter(1,1,LLexpgeo,SearchexpgeoStartParameters)
  }
  
  else if (method=='explog')
  {
    params <- FitDistrTwoParameter(1,1,LLexplog,SearchexplogStartParameters)
  }
  
  else if (method=='explogis')
  {
    params <- FitDistrTwoParameter(1,1,LLexplogis,SearchexplogisStartParameters)
  }
  
  else if (method=='exppois')
  {
    params <- FitDistrTwoParameter(1,1, LLexppois,SearchexppoisStartParameters)
    
  }
  else if(method=='f')
  {
    func<-LLf
    start <- list(d1=100,d2=100,loc=0.01)
    params<-C_MLE(func,start, 1)
  }
  else if(method=='frechet')
  {
    func<-LLfrechet
    start <- list(a=1,s=1,loc=0.01)
    params<-C_MLE(func,start, 1)
  }
  else if (method=='Gamma')
  {
    func<-LLGamma
    start <- list(a=60,b=100, loc=0.5)
    if(!is.null(params))
    {
      start <-list(a =as.numeric(params@coef[1]) , b = as.numeric(params@coef[2]), loc = as.numeric(params@coef[3] ))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params <- SearchGammaStartParameters()
    }
    
  }
  else if (method=='genpareto')
  {
    func<-LLgenpareto
    start <- list(k=0.1,c=0)
    
    if(!is.null(params))
    {
      start <-list(k =as.numeric(params@coef[1]) , c = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params <- SearchgenparetoStartParameters()
    }
    
  }
  else if (method=='genpowerweibull')
  {
    params <- FitDistrTwoParameter(1,1,LLgenpowerweibull,SearchgenpowerweibullStartParameters)
  }
  else if (method=='gumbel')
  {
    func<-LLgumbel
    start <- list(mu=0,sigma=1)
    
    if(!is.null(params))
    {
      start <-list(mu =as.numeric(params@coef[1]) , sigma = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(mu=0,sigma=1)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- SearchgumbelStartParameters()
    }
    
  }
  else if (method=='invgamma')
  {
    params <- FitDistrTwoParameter(1,1,LLinvgamma,SearchinvgammaStartParameters)
  }
  else if (method=='kum')
  {
    params <- FitDistrTwoParameter(1,1,LLkum,SearchkumStartParameters)
  }
  else if (method=='kumburr7')
  {
    params <- FitDistrFourParameter(1,1,1,1,LLkumburr7,Searchkumburr7StartParameters,useOldParams = FALSE)
  }
  else if (method=='kumexp')
  {
    params <- FitDistrThreeParameter(1,1,1,LLkumexp,SearchkumexpStartParameters)
  }
  else if (method=='kumgamma')
  {
    params <- FitDistrFourParameter(1,1,1,1,LLkumgamma,SearchkumgammaStartParameters)
  }
  else if (method=='kumgumbel')
  {
    params <- FitDistrFourParameter(func=LLkumgumbel,searchFunc =SearchkumgumbelStartParameters)
  }
  else if (method=='kumhalfnorm')
  {
    params <- FitDistrThreeParameter(1,1,1,LLkumhalfnorm,SearchkumhalfnormStartParameters)
  }
  else if (method=='kumloglogis')
  {
    params <- FitDistrFourParameter(func=LLkumloglogis,searchFunc =SearchkumloglogisStartParameters)
  }
  else if (method=='kumnormal')
  {
    params <- FitDistrFourParameter(func=LLkumnormal,searchFunc = SearchkumnormalStartParameters)
  }
  else if (method=='kumpareto')
  {
    params <- FitDistrThreeParameter(1,1,1,func=LLkumpareto,searchFunc = SearchkumparetoStartParameters, useOldParams = FALSE)
  }
  else if (method=='kumweibull')
  {
    params <- FitDistrFourParameter(func=LLkumweibull ,searchFunc = SearchkumweibullStartParameters)
  }
  else if (method=='laplace')
  {
    func<-LLlaplace
    start <- list(mu=0,sigma=1)
    
    if(!is.null(params))
    {
      start <-list(mu =as.numeric(params@coef[1]) , sigma = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(mu=0,sigma=1)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- SearchlaplaceStartParameters()
    }
    
  }
  
  else if (method=='lfr')
  {
    params <- FitDistrTwoParameter(1,1,LLlfr ,SearchlfrStartParameters)
  }
  else if (method=='LNbeta')
  {
    params <- FitDistrThreeParameter(1,1,1, LLLNbeta ,SearchLNbetaStartParameters)
  }
  else if (method=='logcauchy')
  {
    func<-LLlogcauchy
    start <- list(mu=0,sigma=1)
    
    if(!is.null(params))
    {
      start <-list(mu =as.numeric(params@coef[1]) , sigma = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(mu=0,sigma=1)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- SearchlogcauchyStartParameters()
    }
    
  }
  else if (method=='loggamma')
  {
    params <- FitDistrTwoParameter(1,1,LLloggamma ,SearchloggammaStartParameters)
  }
  else if (method=='logisexp')
  {
    params <- FitDistrTwoParameter(1,1,LLlogisexp ,SearchlogisexpStartParameters)
  }
  else if (method=='logisrayleigh')
  {
    params <- FitDistrTwoParameter(1,1,LLlogisrayleigh ,SearchlogisrayleighStartParameters)
  }
  else if (method=='logistic')
  {
    func<-LLlogistic
    start <- list(mu=0,sigma=1)
    
    if(!is.null(params))
    {
      start <-list(mu =as.numeric(params@coef[1]) , sigma = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(mu=0,sigma=1)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- SearchlogisticStartParameters()
    }
    
  }
  
  else if (method=='loglaplace')
  {
    params <- FitDistrThreeParameter(1,1,1, LLloglaplapce ,SearchloglaplaceStartParameters)
  }
  
  else if (method=='loglog')
  {
    params <- FitDistrTwoParameter(1,2,LLloglog ,SearchloglogStartParameters)
  }
  
  else if (method=='loglogis')
  {
    params <- FitDistrTwoParameter(1,1,LLloglogisg , SearchkumloglogisStartParameters)
  }
  
  else if (method=='lognorm')
  {
    params <- FitDistrTwoParameter(0,1,LLlognorm , SearchlognormStartParameters)
  }
  
  else if (method=='lomax')
  {
    params <- FitDistrTwoParameter(1,1,LLlomax , SearchlomaxStartParameters)
  }
  
  else if (method=='Mlaplace')
  {
    params <- FitDistrThreeParameter(1,1,1, LLMlaplace ,SearchMlaplaceStartParameters)
  }
  
  else if (method=='moexp')
  {
    params <- FitDistrTwoParameter(1,1,LLmoexp , SearchmoexpStartParameters)
  }
  
  else if (method=='moweibull')
  {
    params <- FitDistrThreeParameter(1,1,1, LLmoweibull ,SearchmoweibullStartParameters)
  }
  
  else if (method=='MRbeta')
  {
    params <- FitDistrFourParameter(0.1,0.9,1, 0.6, LLMRbeta ,SearchMRbetaStartParameters)
  }
  
  else if (method=='nakagami')
  {
    params <- FitDistrTwoParameter(1, 1, LLnakagami ,SearchnakagamiStartParameters,useOldParams = FALSE)
  }
  
  else if (method=='paretostable')
  {
    params <- FitDistrThreeParameter(1,1,1, LLparetostable ,SearchparetostableStartParameters)
  }
  
  else if (method=='PCTAlaplace')
  {
    params <- FitDistrTwoParameter(0.5, 0, LLPCTAlaplace ,SearchPCTAlaplaceStartParameters)
  }
  
  else if (method=='perks')
  {
    func<-LLperks
    start <- list(p1=1, p2=1)
    
    if(!is.null(params))
    {
      start <-list(p1 =as.numeric(params@coef[1]), p2 =as.numeric(params@coef[2]) )
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(p1=1, p2=1)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- SearchperksStartParameters()
    }
    
  }
  
  else if (method=='rgamma')
  {
    params <- FitDistrThreeParameter(1,1,1, LLrgamma ,SearchrgammaStartParameters)
  }
  
  else if (method=='schabe')
  {
    params <- FitDistrTwoParameter(0.5, 1, LLschabe ,SearchschabeStartParameters)
  }
  else if (method=='stacygamma')
  {
    params <- FitDistrThreeParameter(1,1,1, LLstacygamma ,SearchstacygammaStartParameters)
  }
  else if (method=='t')
  {
     params <- FitDistrOneParameter(1,LLT ,S)
  }
  else if (method=='TL2')
  {
    func<-LLTL2
    start <- list(b=1)
    
    if(!is.null(params))
    {
      start <-list(b =as.numeric(params@coef[1]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(b=1)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- SearchTL2StartParameters()
    }
    
  }
  
  else if (method=='tsp')
  {
    func<-LLtsp
    start <- list(a=1,theta=0.5)
    
    if(!is.null(params))
    {
      start <-list(a =as.numeric(params@coef[1]) , theta = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(a=1,theta=0.5)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- SearchtspStartParameters()
    }
    
  }
  
  else if (method=='triangular')
  {
    func<-LLtriangular
    start <- list(a=-0.1,b=0.04,c=0.01)
    if(!is.null(params))
    {
      start <-list(a =as.numeric(params@coef[1]) , b = as.numeric(params@coef[2]) , c = as.numeric(params@coef[3]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params <- SearchtriangularStartParameters()
    }
    
  }
  else if (method=='quad')
  {
    func<-LLquad
    start <- list(a=-0,b=0.1)
    if(!is.null(params))
    {
      start <-list(a =as.numeric(params@coef[1]) , b = as.numeric(params@coef[2]))
    }
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params <- SearchquadStartParameters()
    }
    
  }
  else if(method=='Weibull')
  {
    func<-LLWeibull
    start <- list(a=7,s=0.15,loc=0.01)
    if(!is.null(params))
    {
      start <-list(a =as.numeric(params@coef[1]) , s = as.numeric(params@coef[2]) , loc = as.numeric(params@coef[3]))
    }
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      params <- SearchWeibullStartParameters()
    }
  }
  else if (method=='uniform')
  {
    params <- FitDistrTwoParameter(0.001,0.999,LLuniform,SearchuniformStartParameters)
  }
  
  else if (method=='xie')
  {
    params <- FitDistrThreeParameter(1,1,1, LLxie ,SearchxieStartParameters)
  }
  
  
  return(params)
}

C_MLE <-function(func, start, methodi,upper=c(Inf,Inf,Inf),lower=c(-Inf,-Inf,-Inf), fixed=list() )
{
  #func=LLT
 # start=1
#  methodi=1
  
  
  methods <- c('CG', 'BFGS', 'Nelder-Mead','L-BFGS-B') 
  
  params<- tryCatch(
    {
      if(methodi==length(methods)+1)
      {
        param= mle(minuslogl =  func, start = start,fixed=fixed)
      }
      
      if(methodi<= length(methods))
      {
        param= mle(minuslogl =  func, start = start,method=methods[methodi],fixed=fixed)   
      }
      
    }
    , error=function(cond) { 
      
      print(cond)
      #print(paste(methodi," failed"))
      
      C_MLE(func,start,methodi+1,upper=upper,lower=lower,fixed=fixed)
      
    }
    
    
  )
  
  return(params)
  
  
}

G_process <- function(meanModel,varianceModel,distModel,bounds = NULL,solver = 'hybrid') 
{
  fitold = fit
  fit = NULL
  
  spec = ugarchspec(mean.model = meanModel, variance.model = varianceModel, distribution.model = distModel)
  
  if(!is.null(bounds))
  {
    setbounds(spec)<-bounds  
  }
  
params<- tryCatch(
  {
    fit = ugarchfit(spec, data = x  * - 1 , out.sample = 1 ,solver = solver)
    params<-ugarchforecast(fit, data = NULL, n.ahead = 1, n.roll = 1, out.sample = 1)
    assign("fit",fit, envir = .GlobalEnv)
  }
  , error=function(cond) { 
    if(is.null(fit) )
    {
      
      params<-tryCatch(
        {
          print("searching for different start parameters")
          
          fit = ugarchfit(spec, data = x  * - 1 , out.sample = 1 ,solver = 'gosolnp', solver.control = list(n.sim=100, n.restarts=5))
          params<-ugarchforecast(fit, data = NULL, n.ahead = 1, n.roll = 1, out.sample = 1)
          assign("fit",fit, envir = .GlobalEnv)
        }, error=function(cond) 
        {
          if(is.null(fit))
          {
            print("using previous fit")
            fit = fitold
          }
          params<-ugarchforecast(fit, data = NULL, n.ahead = 1, n.roll = 1, out.sample = 1)
          
          return(params)
        })
    }
    else
    {
      return(params)
    }
  }
)

return(params)
}