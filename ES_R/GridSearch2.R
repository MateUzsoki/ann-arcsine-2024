

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


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



#sum(dGamma(x+0.2,a=47, b =100 , log=FALSE) < 0)
