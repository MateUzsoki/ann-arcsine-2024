CalculateVaR <-function(losses,conf, method,params)
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
    
    return(VaR)
    
  }
  if(method == 'gjrgarch-arma-std'|| method =='sgarch-arma-std' || method =='egarch-arma-std' || method == 'sgarch-arma-std-2-2' || method == 'igarch-arma-std')
  {
    modelfor = params
    shape = modelfor@model$pars[17]
    var = ((fitted(modelfor) + sigma(modelfor)*qdist("std", p=(1-conf), mu = 0, sigma = 1, shape=shape) )*-1)
    
    return(var[2])
  }
  if(method == 'gjrgarch-arma-norm'|| method =='sgarch-arma-norm' || method =='egarch-arma-norm' || method =='igarch-arma-norm')
  {
    modelfor = params
    
    var = ((fitted(modelfor) + sigma(modelfor)*qdist("norm", p=(1-conf), mu = 0, sigma = 1) )*-1)
    
    return(var[2])
  }
  if(method == 'gjrgarch-arma' || method=='sgarch-arma-sstd' || method=='egarch-arma-sstd'|| method =='igarch-arma-sstd')
  {
    modelfor = params
    skew = modelfor@model$pars[16]
    
    shape = modelfor@model$pars[17]
    
    var = ((fitted(modelfor) + sigma(modelfor)*qdist("sstd", p=(1-conf), mu = 0, sigma = 1, skew  = skew, shape=shape) )*-1)
    
    return(var[2])
  }
  if(method == 'historical') {
    return(as.numeric(quantile(losses, conf)))
  }
  if(method == 't')
  {
    return(varT(1-conf,n=params@coef[1])*-1)
  }
  if(method == 'normal')
  {
    s = sd(losses)
    m = mean(losses)
    return(varnormal(1-conf,s,mu = m)*-1)
  }
  if(method == 't4')
  {
    s = sd(losses)
    m = mean(losses)
    return( cvar::VaR(pt,dist.type = "cdf", df=4 ,intercept = m, slope = s ))
  }
  if(method == 't2')
  {
    s = sd(losses)
    m = mean(losses)
    return( cvar::VaR(pt,dist.type = "cdf", df=2 ,intercept = m, slope = s ))
  }
  if(method == 't5')
  {
    s = sd(losses)
    m = mean(losses)
    return( cvar::VaR(pt,dist.type = "cdf", df=5 ,intercept = m, slope = s ))
  }
  if(method== 'ast') #Generalized asymmetric Student's t distribution
  {
    return((varast(1-conf,nu1=params@coef[1], nu2= params@coef[2], alpha= params@coef[3]) - as.numeric(params@coef[4])) *-1 )
  }
  if(method=='aep')
  {
    return(varaep(1-conf,q1 = params@coef[1], q2=params@coef[2], alpha=params@coef[3])*-1)
  }
  if(method=='aep-std')
  {
    s = sd(losses)
    m = mean(losses)
    var = cvar::VaR(paep,dist.type = "cdf", intercept = m, slope = s )
    
    return(var)
  }
  if(method=='arcsine')
  {
    a = params@coef[1]
    b = params@coef[2]
    
    var = vararcsine(1-conf,a = a, b=b)*-1
    return(var)
  }
  if(method=='asylaplace')
  {
    tau = params@coef[1]
    kappa = params@coef[2]
    theta = params@coef[3]
    
    var = varasylaplace(1-conf,tau=tau,kappa=kappa,theta=theta)*-1
    return(var)
  }
  else if (method=="asypower")
  {
    return(varasypower(1-conf,a=params@coef[1], lambda = params@coef[2], delta = params@coef[3]) *-1 )
  }
  else if (method=="beard")
  {
    return( (varbeard(1-conf,a=params@coef[1], b = params@coef[2], rho = params@coef[3] ) ) * -1 )
  }
  else if (method=="f")
  {
    return((varF(1-conf,d1=params@coef[1], d2= params@coef[2])- as.numeric(params@coef[3])  )  *-1)
  }
  else if (method=="frechet")
  {
    return((varfrechet(1-conf,alpha=params@coef[1], sigma= params@coef[2]) - as.numeric(params@coef[3])) *-1 )
  }
  else if (method=="betadist")
  {
    return( (varbetadist(1-conf,a=params@coef[1], b = params@coef[2]) - as.numeric(params@coef[3])) * -1 )
  }
  else if (method=="burr")
  {
    return( (varburr(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="burr7")
  {
    return( (varburr7(1-conf,k=params@coef[1], c = params@coef[2]) ) * -1 )
  }
  else if (method=="Cauchy")
  {
    return(varCauchy(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if (method=="chen")
  {
    return(varchen(1-conf,b=params@coef[1], lambda = params@coef[2]) *-1 )
  }
  else if (method=="clg")
  {
    return(varclg(1-conf,a=params@coef[1], b = params@coef[2], theta= params@coef[3]) *-1 )
  }
  else if (method=="compbeta")
  {
    return(varcompbeta(1-conf,a=params@coef[1], b = params@coef[2]) *-1 )
  }
  else if (method=="dagum")
  {
    return(vardagum(1-conf,a=params@coef[1], b = params@coef[2], c= params@coef[3]) *-1 )
  }
  else if (method=="expexp")
  {
    return(varexpexp(1-conf,lambda=params@coef[1], a = params@coef[2]) *-1 )
  }
  else if (method=="expext")
  {
    return(varexpext(1-conf,lambda=params@coef[1], a = params@coef[2]) *-1 )
  }
  else if (method=="expgeo")
  {
    return(varexpgeo(1-conf,theta = params@coef[1], lambda = params@coef[2]) *-1 )
  }
  else if (method=="explog")
  {
    return(varexplog(1-conf,a = params@coef[1], b = params@coef[2]) *-1 )
  }
  else if (method=="explogis")
  {
    return(varexplogis(1-conf,a = params@coef[1], b = params@coef[2]) *-1 )
  }
  else if (method=="exppois")
  {
    return(varexppois(1-conf,b=params@coef[1], lambda = params@coef[2]) *-1 )
  }
  else if(method=="Gamma")
  {
    return((varGamma(1-conf,a=params@coef[1], b= params@coef[2]) - as.numeric(params@coef[3])) *-1 )
  }
  else if (method=="genpareto")
  {
    c= params@coef[2]
    if(c==0) 
    {
      c= 0.000000000001
    }
    
    return( (vargenpareto(1-conf,k=params@coef[1], c= c ))  * -1 )
  }
  else if (method=="genpowerweibull")
  {
    return(vargenpowerweibull(1-conf,a=params@coef[1], theta = params@coef[2]) *-1 )
  }
  else if(method=="laplace")
  {
    return(varlaplace(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if(method=="logcauchy")
  {
    return(varlogcauchy(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if (method=="logisrayleigh")
  {
    return( (varlogisrayleigh(1-conf,a=params@coef[1], lambda = params@coef[2]) ) * -1 )
  }
  else if(method=="logistic")
  {
    return(varlogistic(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if (method=="loglaplace")
  {
    return( (varloglaplace(1-conf,a =params@coef[1], b = params@coef[2] , delta = params@coef[3]  ) ) * -1 )
  }
  else if (method=="loglog")
  {
    return( (varloglog(1-conf,a=params@coef[1], lambda = params@coef[2]) ) * -1 )
  }
  else if (method=="loglogis")
  {
    return( (varloglogis(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="lognorm")
  {
    return( (varlognorm(1-conf,mu=params@coef[1], sigma = params@coef[2]) ) * -1 )
  }
  else if (method=="lomax")
  {
    return( (varlomax(1-conf,a=params@coef[1], lambda = params@coef[2]) ) * -1 )
  }
  else if (method=="Mlaplace")
  {
    return( (varMlaplace(1-conf,theta =params@coef[1], phi = params@coef[2] , psi = params@coef[3]  ) ) * -1 )
  }
  else if (method=="moexp")
  {
    return( (varmoexp(1-conf,lambda =params@coef[1], a = params@coef[2]  ) ) * -1 )
  }
  else if (method=="moweibull")
  {
    return( (varmoweibull(1-conf,a =params@coef[1], b = params@coef[2] , lambda = params@coef[3]  ) ) * -1 )
  }
  else if (method=="MRbeta")
  {
    return( (varMRbeta(1-conf,a =params@coef[1], b = params@coef[2] , r = params@coef[3] , q = params@coef[4]  ) ) * -1 )
  }
  else if (method=="nakagami")
  {
    return( (varnakagami(1-conf,m =params@coef[1], a = params@coef[2]  ) ) * -1 )
  }
  else if (method=="paretostable")
  {
    return( (varparetostable(1-conf,lambda =params@coef[1], nu = params@coef[2] , sigma = params@coef[3]  ) ) * -1 )
  }
  else if (method=="PCTAlaplace")
  {
    return( (varPCTAlaplace(1-conf,a =params@coef[1], theta = params@coef[2]  ) ) * -1 )
  }
  else if (method=="rgamma")
  {
    return( (esrgamma(1-conf,a =params@coef[1], theta = params@coef[2] ,  phi = params@coef[3]  ) ) * -1 )
  }
  else if (method=="schabe")
  {
    return( (varschabe(1-conf,gamma =params@coef[1], theta = params@coef[2]  ) ) * -1 )
  }
  else if (method=="stacygamma")
  {
    return( (varstacygamma(1-conf,params@coef[1],  params@coef[2] ,  params@coef[3]  ) ) * -1 )
  }
  else if (method=="invgamma")
  {
    return( (varinvgamma(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="kum")
  {
    return( (varkum(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="kumburr7")
  {
    return( (varkumburr7(1-conf,a=params@coef[1], b = params@coef[2] , k = params@coef[3] , c = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumexp")
  {
    return( (varkumexp(1-conf,lambda=params@coef[1], a = params@coef[2] , b = params@coef[3] ) ) * -1 )
  }
  else if (method=="kumgamma")
  {
    return( (varkumgamma(1-conf,a=params@coef[1], b = params@coef[2] , c = params@coef[3] , d = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumgumbel")
  {
    return( (varkumgumbel(1-conf,a=params@coef[1], b = params@coef[2] , mu = params@coef[3] , sigma = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumhalfnorm")
  {
    return( (varkumhalfnorm(1-conf,sigma=params@coef[1], a = params@coef[2] , b = params@coef[3] ) ) * -1 )
  }
  else if (method=="kumloglogis")
  {
    return( (varkumloglogis(1-conf,a=params@coef[1], b = params@coef[2] , alpha = params@coef[3] , beta = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumnormal")
  {
    return( (varkumnormal(1-conf,mu=params@coef[1], sigma = params@coef[2] , a = params@coef[3] , b = params@coef[4] ) ) * -1 )
  }
  else if (method=="kumpareto")
  {
    return( (varkumpareto(1-conf,K=min(x), a = 1 , b = params@coef[2] , c = params@coef[3] ) - as.numeric(params@coef[1]) )   * -1 )
  }
  else if (method=="kumweibull")
  {
    return( (varkumweibull(1-conf,a=params@coef[1], b = params@coef[2] , alpha = params@coef[3] , sigma = params@coef[4] ) ) * -1 )
  }
  else if (method=="lfr")
  {
    return( (varlfr(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="LNbeta")
  {
    return( (varLNbeta(1-conf,lambda =params@coef[1], a = params@coef[2] , b = params@coef[3]  ) ) * -1 )
  }
  else if (method=="loggamma")
  {
    return( (varloggamma(1-conf,a=params@coef[1], r = params@coef[2]) ) * -1 )
  }
  else if (method=="logisexp")
  {
    return( (varlogisexp(1-conf,lambda=params@coef[1], a = params@coef[2]) ) * -1 )
  }
  else if(method=="gumbel")
  {
    return(vargumbel(1-conf,mu=params@coef[1], sigma= params@coef[2]) *-1 )
  }
  else if (method=="perks")
  {
    return(varperks(1-conf,a=params@coef[1], b= params@coef[2]) *-1 )
  }
  else if (method=="quad")
  {
    return(varquad(1-conf,a=params@coef[1], b= params@coef[2]) *-1 )
  }
  else if(method=="TL2")
  {
    return(varTL2(1-conf,b=params@coef[1]) *-1 )
  }
  else if(method=="tsp")
  {
    return(vartsp(1-conf,a=params@coef[1], theta= params@coef[2]) *-1 )
  }
  else if (method=="triangular")
  {
    return(vartriangular(1-conf,a=params@coef[1], b= params@coef[2], c = as.numeric(params@coef[3])) *-1 )
  }
  else if (method=="uniform")
  {
    return( (varuniform(1-conf,a=params@coef[1], b = params@coef[2]) ) * -1 )
  }
  else if (method=="Weibull")
  {
    return((varWeibull(1-conf,alpha=params@coef[1], sigma= params@coef[2]) - as.numeric(params@coef[3])) *-1 )
  }
  else if (method=="xie")
  {
    return( (varxie(1-conf,params@coef[1],  params@coef[2] ,  params@coef[3]  ) ) * -1 )
  }
  
}