
library(VaRES)

LLaep <- function(q1, q2, alpha) {
  res = NaN
  
  if(alpha > 0.005 && alpha < 1)
  {
    R = daep(x, q1=q1,q2=q2,  alpha= alpha, log=FALSE)
    res <- -sum(log(R))
  }
  
  if(is.infinite(res))
  {
    res = NaN
  }
  
  #print(paste("q1:",q1,"q2:",q2,"alpha:", alpha, "res:",res))
  
  return(res)
}

LLast <- function(nu1, nu2,alpha, loc) {
  res = NA
  
  if(alpha > 0 && alpha < 1 && nu1 > 0.05 && nu2 > 0.05)
  {
  
    R = dast(x+loc, nu1, nu2, alpha= alpha, log=FALSE)
    res <- -sum(log(R))
  
  }
  
  return(res)
}  



LLarcsine <- function(a, b) {
  res = 0
  
  
  
  if(a < b && a < min(x) && b > max(x))
  {
    R = darcsine(x, a,b, log=FALSE)
    res = -sum(log(R))
  }
  
 # print(paste("a:",a,"b:",b, "res:",res))
  return(res)
}


LLasylaplace <- function(tau,kappa,theta ) {
  R = dasylaplace(x,tau, kappa,theta, log=FALSE)
  -sum(log(R))
}

LLasypower <- function(p1,p2,p3 ) {
  res=NA
  if(p1> 0 && p1< 1 && p2>0 && p3>0)
  {
    R = dasypower(x,a = p1, lambda = p2,delta = p3, log=FALSE)
    res = -sum(log(R))
  }
  
  return(res)
}

LLbetadist <- function(a, b, loc) {
  R = dbetadist(x+loc,a=a, b=b, log=FALSE)
  -sum(log(R))
}

LLbeard <- function(p1, p2, p3) {
  R = suppressWarnings(dbeard(x,a=p1, b=p2, rho=p3, log=FALSE))
  sum(log(R))
}

LLburr<- function(p1,p2) {
  R = dburr(x,a = p1, b= p2, log=FALSE)
  -sum(log(R))
}

LLburr7<- function(p1,p2) {
  R = dburr7(x,k = p1, c= p2, log=FALSE)
  -sum(log(R))
}

LLCauchy <- function(mu, sigma) {
  R = dCauchy(x, mu=mu,sigma=sigma, log=FALSE)
  -sum(log(R))
}

LLchen<- function(p1,p2) {
  R = dchen(x,b = p1, lambda= p2, log=FALSE)
  -sum(log(R))
}

LLclg<- function(p1,p2,p3) {
  R = dclg(x,a = p1, b= p2,theta=p3, log=FALSE)
  -sum(log(R))
}

LLcompbeta<- function(p1,p2) {
  R = dcompbeta(x,a = p1, b= p2, log=FALSE)
  -sum(log(R))
}

LLdagum<- function(p1,p2,p3) {
  R = ddagum(x,a = p1, b= p2,c=p3, log=FALSE)
  -sum(log(R))
}

LLexpexp<- function(p1,p2) {
  R = dexpexp(x,lambda =  p1, a= p2, log=FALSE)
  -sum(log(R))
}


LLexpext<- function(p1,p2) {
  R = dexpext(x,lambda =  p1, a= p2, log=FALSE)
  -sum(log(R))
}


LLexpgeo<- function(p1,p2) {
  R = dexpgeo(x, theta =  p1, lambda = p2, log=FALSE)
  -sum(log(R))
}

LLexplog<- function(p1,p2) {
  R = dexplog(x, a =  p1, b = p2, log=FALSE)
  -sum(log(R))
}

LLexplogis<- function(p1,p2) {
  R = dexplogis(x, a =  p1, b = p2, log=FALSE)
  -sum(log(R))
}

LLexppois <- function(p1,p2)
{
  res = NA
  
  if(p1 > 0 && p2 > 0)
  {
  R = dexppois(x, b=p1,lambda=p2, log=FALSE)
  res = -sum(log(R))
  }
  
  return (res)
}

LLf <- function(d1, d2,loc) {
  R = dF(x+loc, d1=d1,d2=d2, log=FALSE)
  -sum(log(R))
}


LLfrechet <- function(a,s,loc) {
  R = dfrechet(x+loc, alpha = a,sigma=s, log=FALSE)
  -sum(log(R))
}
LLGamma <- function(a,b,loc) {
  R = dGamma(x+loc,a=a, b = b, log=FALSE)
  -sum(log(R))
}

LLgenpowerweibull<- function(p1,p2) {
  R = dgenpowerweibull(x, a =  p1, theta = p2, log=FALSE)
  -sum(log(R))
}

LLgumbel <- function(mu, sigma)
{
  R = dgumbel(x,mu=mu,sigma=sigma, log=FALSE) 
  -sum(log(R))
}


LLgenpareto <- function(k,c) {
  R = dgenpareto(x,k=k, c = c, log=FALSE)
  -sum(log(R))
}

LLinvgamma <- function(p1, p2) {
  res = 0
  
  if(p1 > 0 && p2 > 0)
  {
    R = dinvgamma(x,a=p1, b=p2, log=FALSE)
    
    #print(paste(p1,p2,sum(log(R))))
    res = sum(log(R))
  }
  
  return(res)
}

LLkum<- function(p1,p2) {
  R = dkum(x, a =  p1, b = p2, log=FALSE)
  -sum(log(R))
}

LLkumburr7<- function(p1,p2,p3,p4) {
  R = dkumburr7(x, a =  p1, b = p2, k=p3, c=p4 ,log=FALSE)
  -sum(log(R))
}

LLkumexp<- function(p1,p2,p3) {
  R = dkumexp(x, lambda =  p1, a = p2,b=p3 ,log=FALSE)
  -sum(log(R))
}

LLkumgamma<- function(p1,p2,p3,p4) {
  R = dkumgamma(x, a =  p1, b = p2, c=p3, d=p4 ,log=FALSE)
  -sum(log(R))
}

LLkumgumbel<- function(p1,p2,p3,p4) {
  R = dkumgumbel(x, a =  p1, b = p2, mu=p3, sigma=p4 ,log=FALSE)
  -sum(log(R))
}

LLkumhalfnorm<- function(p1,p2,p3) {
  R = dkumhalfnorm(x, sigma =  p1, a = p2,b=p3 ,log=FALSE)
  -sum(log(R))
}

LLkumloglogis<- function(p1,p2,p3,p4) {
  res = NA
  
  var = varkumloglogis(0.025, a=p1,b=p2,alpha=p3,beta=p4)
  
  if(! is.nan(var))
  {
    R = dkumloglogis(x, a =  p1, b = p2, alpha=p3, beta=p4 ,log=FALSE)
    res = -sum(log(R))
  }
  
  return(res)
}

LLkumnormal<- function(p1,p2,p3,p4) {
  R = dkumnormal(x, mu =  p1, sigma = p2, a=p3, b=p4 ,log=FALSE)
  -sum(log(R))
}

LLkumpareto<- function(p1,p2,p3) {
  res = NA
  
  R = dkumpareto(x+p1, K = min(x)+p1, a = 1, b=p2, c=p3 ,log=FALSE)
  
  #print(paste(p1,p2,p3,  -sum(log(R))))
  -sum(log(R))
  
}

LLkumweibull<- function(p1,p2,p3,p4) {
  R = dkumweibull(x, a =  p1, b = p2, alpha=p3, sigma=p4 ,log=FALSE)
  -sum(log(R))
}

LLlaplace <- function(mu, sigma)
{
  R = dlaplace(x,mu=mu,sigma=sigma, log=FALSE) 
  -sum(log(R))
}

LLlfr<- function(p1,p2) {
  R = dlfr(x, a =  p1, b = p2 ,log=FALSE)
  -sum(log(R))
}

LLLNbeta<- function(p1,p2, p3) {
  R = dLNbeta(x, lambda =  p1, a = p2 , b = p3, log=FALSE)
  -sum(log(R))
}


LLlogcauchy <- function(mu, sigma)
{
  R = dlogcauchy(x,mu=mu,sigma=sigma, log=FALSE) 
  -sum(log(R))
}

LLloggamma <- function(p1,p2) {
  R = dloggamma(x, a =  p1, r = p2 ,log=FALSE)
  -sum(log(R))
}

LLlogisexp <- function(p1,p2) {
  R = dlogisexp(x, lambda =  p1, a = p2 ,log=FALSE)
  -sum(log(R))
}

LLlogisrayleigh <- function(p1, p2)
{
  R = dlogisrayleigh(x,a=p1,lambda=p2, log=FALSE) 
  -sum(log(R))
}


LLlogistic <- function(mu, sigma)
{
  R = dlogistic(x,mu=mu,sigma=sigma, log=FALSE) 
  -sum(log(R))
}

LLloglaplapce<- function(p1,p2, p3) {
  R = dloglaplace(x, a =  p1, b = p2 , delta = p3, log=FALSE)
  -sum(log(R))
}

LLloglog <- function(p1,p2) {
  R = dloglog(x,a = p1, lambda= p2, log=FALSE)
  -sum(log(R))
} 

LLloglogis <- function(p1,p2) {
  R = dloglog(x,a = p1, b= p2, log=FALSE)
  -sum(log(R))
} 

LLlognorm <- function(p1,p2) {
  R = dlognorm(x,mu = p1, sigma= p2, log=FALSE)
  -sum(log(R))
} 

LLlomax <- function(p1,p2) {
  R = dlomax(x,a = p1, lambda= p2, log=FALSE)
  -sum(log(R))
} 


LLMlaplace <- function(p1,p2,p3) {
  R = dMlaplace(x,theta = p1, phi= p2,psi = p3, log=FALSE)
  -sum(log(R))
} 

LLmoexp <- function(p1,p2) {
  R = dmoexp(x,lambda = p1, a= p2, log=FALSE)
  -sum(log(R))
}

LLmoweibull <- function(p1,p2,p3) {
  R = dmoweibull(x,a = p1, b= p2,lambda = p3, log=FALSE)
  -sum(log(R))
} 

LLMRbeta <- function(p1,p2,p3,p4) {
  R = dMRbeta(x,a = p1, b= p2, r=p3, q=p4, log=FALSE)
  -sum(log(R))
}

LLnakagami <- function(p1,p2) {
  R = dnakagami(x,m = p1, a= p2, log=FALSE)
  -sum(log(R))
}

LLnormal <- function(mu,sigma) {
  R = dnormal(x,mu = mu, sigma= sigma, log=FALSE)
  -sum(log(R))
} 

LLpareto <- function(p1,p2) {
  R = dpareto(x,K = p1, c= p2, log=FALSE)
  -sum(log(R))
} 

LLparetostable <- function(p1,p2,p3) {
  R = dparetostable(x,lambda = p1, nu = p2, sigma = p3, log=FALSE)
  -sum(log(R))
}

LLPCTAlaplace <- function(p1,p2) {
  R = dPCTAlaplace(x,a = p1, theta = p2, log=FALSE)
  -sum(log(R))
} 

LLperks <- function(p1,p2) {
  R = dperks(x,a = p1, b= p2, log=FALSE)
  -sum(log(R))
} 

LLrgamma <- function(p1,p2,p3) {
  R = drgamma(x,a = p1, theta = p2, phi = p3, log=FALSE)
  -sum(log(R))
}

LLschabe <- function(p1,p2) {
  R = dschabe(x,gamma = p1, theta= p2, log=FALSE)
  -sum(log(R))
}

LLstacygamma <- function(p1,p2,p3) {
  R = dstacygamma(x,p1,p2,p3, log=FALSE)
  -sum(log(R))
}

LLT <- function(p1) {
  R = dT(x,n=p1, log=FALSE)
  -sum(log(R))
}

LLTL2 <- function(b) {
  R = dTL2(x,b=b, log=FALSE)
  -sum(log(R))
}

LLtsp <- function(a,theta) {
  R = dtsp(x,a=a, theta = theta, log=FALSE)
  -sum(log(R))
}

LLtriangular <- function(a,b, c) {
  R = dtriangular(x,a=a, b = b, c=c, log=FALSE)
  -sum(log(R))
}

LLquad <- function(a,b) {
  R = dquad(x,a=a, b = b, log=FALSE)
  -sum(log(R))
}

LLuniform  <- function(p1,p2) {
  R = duniform(x,a = p1, b= p2, log=FALSE)
  -sum(log(R))
}

LLWeibull <- function(a,s, loc) {
  R = dWeibull(x+loc, alpha = a,sigma=s, log=FALSE)
  -sum(log(R))
}

LLxie  <- function(p1,p2,p3) {
  R = dxie(x,p1,p2,p3, log=FALSE)
  -sum(log(R))
}