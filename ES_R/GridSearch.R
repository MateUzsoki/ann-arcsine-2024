source("GenGridSearch.R")

SearchastStartParameters<-function()
  
{
  #ast
  
  for(nu1 in 1:100)
  {
    step=0.1
    for(nu2 in 1:100)  
    {
      for(alpha in 0:10)
      {
        print(paste(nu1*step,nu2*step, alpha*step))
        params=C_MLE(LLast,start=list(nu1=nu1*step,nu2=nu2*step, alpha=alpha*step, loc=0.01),1)
        if(!is.null(params))
        {
          print( paste("ezj?:" , nu1, nu2, alpha, " params:",params@coef[1], params@coef[2] , params@coef[3] , params@coef[4]))
          return(params)
        }
      }
      
    }
  }
  return(NULL)
}

SearchasypowerStartParameters<-function()
{
  return(SearchStartForThreeParameters(LLasypower))
}

SearchaepStartParameters<-function()
  
{
  #aep
  
  for(q1 in 1:100)
  {
    step=1
    for(q2 in 1:100)  
    {
      for(alpha in 0:10)
      {
        print(paste(q1*step,q2*step, alpha*0.1))
        params=C_MLE(LLaep,start=list(q1=q1*step,q2=q2*step, alpha=alpha*0.1),1)
        if(!is.null(params))
        {
          print( paste("ezj?:" , q1, q2, alpha, " params:",params@coef[1], params@coef[2] , params@coef[3] , params@coef[4]))
          return(params)
        }
      }
      
    }
  }
  return(NULL)
}

SearchburrStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLburr))
}

Searchburr7StartParameters<-function()
{
  return(SearchStartForTwoParameters(LLburr7))
}

SearchbeardStartParameters<-function()
{
  return(SearchStartForThreeParameters(LLbeard, maxp1 = 50, maxp2 = 50, maxp3 = 50 , step = 1 ))
}

SearchbetadistParameters<-function()
{
  for(a in 1:50)
  {
    step=10
    for(b in 1:50)  
    {
      for(loc in 0:10)
      {
        print(paste(a*step,b*step,loc*0.001))
        print(LLbetadist(a=a*step,b=b*step,loc=loc*0.001))
        print(sum(dbetadist(x+loc,a=a, b=b, log=FALSE) < 0))
        
        params=C_MLE(LLbetadist,start=list(a=a*step,b=b*step,loc=loc*0.001),1)
        if(!is.null(params))
        {
          print( paste("ez:" , a, b, loc," params:",params@coef[1], params@coef[2], params@coef[3]))
          return(params)
        }
      }
    }
  }
}

SearchCauchyStartParameters<-function()
  
{
  
  for(mu in -100:100)
  {
    step=0.1
    for(sigma in -100:100)  
    {
      
      print(paste(mu*step,sigma*step))
      params=C_MLE(LLCauchy,start=list(mu= mean(x) +  mu*step,sigma= sd(x) + sigma*step),1)
      if(!is.null(params))
      {
        print( paste("ezj?:" , mu, sigma, " params:",params@coef[1], params@coef[2]))
        return(params)
      }
      
    }
  }
  return(NULL)
}


SearchchenStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLchen))
}

SearchclgStartParameters<-function()
{
  return(SearchStartForThreeParameters(LLclg))
}
SearchcompbetaStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLcompbeta))
}

SearchdagumStartParameters<-function()
{
  return(SearchStartForThreeParameters(LLdagum))
}

SearchexpexpStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLexpexp))
}

SearchexpextStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLexpext))
}

SearchexpgeoStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLexpgeo))
}

SearchexplogStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLexplog))
}

SearchexplogisStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLexplogis))
}

SearchexppoisStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLexppois, step = 1, minp1 = 1, maxp1 = 50, minp2=1, maxp2 = 100))
}

SearchGammaStartParameters<-function()
  
{
  #Gamma
  
  for(a in 20:100)
  {
    step=1
    for(b in 50:200)  
    {
      for(loc in 0:10)
      {
        print(paste(a*step,b*step,loc*0.1))
        params=C_MLE(LLGamma,start=list(a=a*step,b=b*step,loc=loc*0.1),1)
        if(!is.null(params))
        {
          print( paste("ez:" , a, b, loc," params:",params@coef[1], params@coef[2], params@coef[3]))
          return(params)
        }
      }
      
    }
  }
  return(NULL)
}

SearchgenparetoStartParameters<-function()
{
  step=0.01
  for(k in 1:10)
  {
    for(c in -10:10)
    {
     
        params=C_MLE(LLgenpareto,start=list(k=k*step,c=c*step),1)
        
        if(!is.null(params))
        {
          print( paste("ezj?:" , k, c,  " params:",params@coef[1], params@coef[2], params@coef[3]))
          return(params)
        }
      
    }
  }
}

SearchgenpowerweibullStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLgenpowerweibull))
}

SearchgumbelStartParameters<-function()
  
{
  #gumbel
  
  for(mu in -10:10)
  {
    step=0.1
    for(sigma in 0:20)  
    {
      
      print(paste(mu*step,sigma*step))
      params=C_MLE(LLgumbel,start=list(mu=mu*step,sigma=sigma*step),1)
      if(!is.null(params))
      {
        print( paste("ezj?:" , mu, sigma, " params:",params@coef[1], params@coef[2]))
        return(params)
      }
      
    }
  }
  return(NULL)
}

SearchinvgammaStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLinvgamma, minp1 = 0, maxp1 = 100, minp2 = 0, maxp2 = 100, step = 0.01))
}


SearchkumStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLkum, minp1 = 0, maxp1 = 50, minp2 = 0, maxp2 = 50, step = 0.1))
}

Searchkumburr7StartParameters<-function()
{
  return(SearchStartForFourParameters(LLkumburr7))
}

SearchkumexpStartParameters<-function()
{
  return(SearchStartForThreeParameters(LLkumexp))
}
SearchkumgammaStartParameters<-function()
{
  return(SearchStartForFourParameters(LLkumgamma))
}

SearchkumgumbelStartParameters<-function()
{
  return(SearchStartForFourParameters(LLkumgumbel))
}

SearchkumhalfnormStartParameters<-function()
{
  return(SearchStartForThreeParameters(LLkumhalfnorm))
}

SearchkumloglogisStartParameters<-function()
{
  return(SearchStartForFourParameters(LLkumloglogis))
}

SearchkumnormalStartParameters<-function()
{
  return(SearchStartForFourParameters(LLkumnormal))
}

SearchkumparetoStartParameters<-function()
{
  return(SearchStartForThreeParameters(LLkumpareto))
}

SearchkumweibullStartParameters<-function()
{
  return(SearchStartForFourParameters(LLkumweibull))
}

SearchlaplaceStartParameters<-function()
  
{
  #laplace
  
  for(mu in -10:10)
  {
    step=0.1
    for(sigma in 0:20)  
    {
      
      print(paste(mu*step,sigma*step))
      params=C_MLE(LLlaplace,start=list(mu=mu*step,sigma=sigma*step),1)
      if(!is.null(params))
      {
        print( paste("ezj?:" , mu, sigma, " params:",params@coef[1], params@coef[2]))
        return(params)
      }
      
    }
  }
  return(NULL)
}

SearchlfrStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLlfr))
}

SearchLNbetaStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLLNbeta))
}

SearchlogcauchyStartParameters<-function()
  
{
  print("gridsearch")
  #logcauchy
  
  params=C_MLE(LLlogcauchy,start=list(mu=4,sigma=0.2),1)
  
  if(is.null(params))
  {
    for(mu in -50:50)
    {
      step=0.2
      for(sigma in 0:50)  
      {
        
        
        params=C_MLE(LLlogcauchy,start=list(mu=mu*step,sigma=sigma*step),1)
        if(!is.null(params))
        {
          print(paste(mu*step,sigma*step))
          print( paste("ezj?:" , mu, sigma, " params:",params@coef[1], params@coef[2]))
          return(params)
        }
        
      }
    }
  }
  else
  {
    return(params)
  }
  return(NULL)
}


SearchloggammaStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLloggamma))
}

SearchlogisexpStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLlogisexp))
}

SearchlogisrayleighStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLlogisrayleigh))
}

SearchlogisticStartParameters<-function()
  
{
  #logistic
  
  for(mu in -10:10)
  {
    step=0.1
    for(sigma in 0:20)  
    {
      
      print(paste(mu*step,sigma*step))
      params=C_MLE(LLlogistic,start=list(mu=mu*step,sigma=sigma*step),1)
      if(!is.null(params))
      {
        print( paste("ezj?:" , mu, sigma, " params:",params@coef[1], params@coef[2]))
        return(params)
      }
      
    }
  }
  return(NULL)
}

SearchloglaplaceStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLloglaplapce))
}

SearchloglogStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLloglog,maxp1 = 50, maxp2 = 50))
}

SearchloglogisStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLloglogis ,maxp1 = 50, maxp2 = 50))
}

SearchlognormStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLlognorm))
}

SearchlomaxStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLlomax))
}

SearchMlaplaceStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLMlaplapce))
}


SearchmoexpStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLmoexp))
}

SearchMRbetaStartParameters <- function()
{
  return(SearchStartForFourParameters(LLMRbeta))
}

SearchmoweibullStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLmoweibull))
}

SearchnakagamiStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLnakagami))
}

SearchparetostableStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLparetostable ))
}

SearchPCTAlaplaceStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLPCTAlaplace))
}

SearchPCTAlaplaceStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLPCTAlaplace))
}

SearchperksStartParameters<-function()
  
{

  for(p1 in 1:100)
  {
    step=0.1
    for(p2 in 1:100)  
    {
  
        print(paste(a*step,b*step))
        params=C_MLE(LLperks,start=list(p1=p1*step,p2=p2*step),1)
        
        if(!is.null(params))
        {
          print( paste("ezj?:" , p1, p2, " params:",params@coef[1], params@coef[2] ))
          return(params)
        }
      
    }
    
    
  }
  return(NULL)
}

SearchrgammaStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLrgamma ))
}

SearchschabeStartParameters <- function()
{
  return(SearchStartForTwoParameters(LLschabe,minp1=0, maxp1= 1, minp2 = 0, maxp2 = 100))
}

SearchstacygammaStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLstacygamma ))
}

SearchquadStartParameters<-function()
  
{
  #quad
  
  for(a in 0:100)
  {
    step=0.1
    for(b in 0:100)  
    {
      if(b>a )
      {
        print(paste(a*step,b*step))
        params=C_MLE(LLquad,start=list(a=a*step,b=b*step),1)
        
        if(!is.null(params))
        {
          print( paste("ezj?:" , a, b, " params:",params@coef[1], params@coef[2] ))
          return(params)
        }
      }
    }
    
    
  }
  return(NULL)
}



SearchTStartParameters<-function()
{
  return(SearchStartForOneParameters(LLT,minp1 = 1, maxp1 = 100))
}

SearchTL2StartParameters<-function()
  
{
  #tsp
  
  for(b in 1:100)
  {
    step=0.1
    
      print(paste(b*step))
      params=C_MLE(LLTL2,start=list(b=b*step),1)
      if(!is.null(params))
      {
        print( paste("ezj?:" , b, " params:",params@coef[1]))
        return(params)
      }
      
    
  }
  return(NULL)
}


SearchtriangularStartParameters<-function()
  
{
  #triangular
  
  for(a in -10:10)
  {
    step=0.01
    for(b in -10:10)  
    {
      for(c in -10:10)
      {
        if(a<c && a < b && c < b && c > a)
        {
          print(paste(a*step,b*step,c*step))
          params=C_MLE(LLtriangular,start=list(a=min(x)+a*step,b=max(x)+b*step,c=getmode(x)+c*step),1)
          
          if(!is.null(params))
          {
            print( paste("ezj?:" , a, b,c, " params:",params@coef[1], params@coef[2] , params@coef[3]))
            return(params)
          }
        }
      }
      
    }
  }
  return(NULL)
}


SearchtspStartParameters<-function()
  
{
  #tsp
  
  for(a in 1:100)
  {
    step=0.1
    for(theta in -100:100)  
    {
      
      print(paste(a*step,theta*step))
      params=C_MLE(LLtsp,start=list(a=a*step,theta=theta*step),1)
      if(!is.null(params))
      {
        print( paste("ezj?:" , a, theta, " params:",params@coef[1], params@coef[2]))
        return(params)
      }
      
    }
  }
  return(NULL)
}

SearchWeibullStartParameters<-function()
  
{
  #Weibull
  
  for(a in 0:100)
  {
    step=0.1
    for(s in 0:100)  
    {
      
      print(paste(a*step,s*step))
      params=C_MLE(LLWeibull,start=list(a=a*step,s=s*step,loc=0.1),1)
      if(!is.null(params))
      {
        print( paste("ezj?:" , a, s, " params:",params@coef[1], params@coef[2]))
        return(params)
      }
      
    }
  }
  return(NULL)
}

SearchuniformStartParameters<-function()
{
  return(SearchStartForTwoParameters(LLuniform, minp1 = 0, maxp1 = 100, minp2 = 0, maxp2 = 100, step = 0.01))
}

SearchxieStartParameters <- function()
{
  return(SearchStartForThreeParameters(LLxie))
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




#sum(dGamma(x+0.2,a=47, b =100 , log=FALSE) < 0)
