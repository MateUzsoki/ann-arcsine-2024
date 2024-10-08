
SearchStartForOneParameters<-function(func, step=0.1,minp1=1,maxp1=10)
{
    for(p1 in minp1:maxp1)
    {
      print(paste("1p:",p1*step))
      
      start=list(p1=p1*step)
      
      params<-C_MLE(func,start, 1)
      
      if(!is.null(params))
      {
        print( paste("OK:" , p1,  " params:",params@coef[1]))
        assign("last_start",start, envir = .GlobalEnv)
        return(params)
      }
      
    }

  return(NULL)
}

SearchStartForTwoParameters<-function(func, step=0.1,minp1=1,maxp1=10,minp2=-10,maxp2=10)
{
  for(p1 in minp1:maxp1)
  {
    for(p2 in minp2:maxp2)
    {
      print(paste(p1*step,p2*step))
      
      start=list(p1=p1*step,p2=p2*step)
                 
      params=C_MLE(func,start,1)
      
      if(!is.null(params))
      {
        print( paste("OK:" , p1, p2,  " params:",params@coef[1], params@coef[2], params@coef[3]))
        assign("last_start",start, envir = .GlobalEnv)
        return(params)
      }
      
    }
  }
  
  return(NULL)
}


SearchStartForThreeParameters<-function(func, step=0.1,minp1=1,maxp1=10,minp2=-10,maxp2=10, minp3=1, maxp3=10)
{
  for(p1 in minp1:maxp1)
  {
    for(p2 in minp2:maxp2)
    { 
      for(p3 in minp3:maxp3)
      {
        print(paste(p1*step,p2*step,p3*step))
        
        start =list(p1=p1*step,p2=p2*step, p3 = p3*step)
        
        params=C_MLE(func,start,1)
        
        if(!is.null(params))
        {
          print( paste("OK:" , p1, p2,p3,  " params:",params@coef[1], params@coef[2], params@coef[3]))
          assign("last_start",start, envir = .GlobalEnv)
          last_start <<-start
          print(last_start)
          return(params)
        }
      }
    }
  }
  
  return(NULL)
}

SearchStartForFourParameters<-function(func, step=0.1,minp1=1,maxp1=10,minp2=1,maxp2=10, minp3=1, maxp3=10, minp4 = 1, maxp4 = 10)
{
  for(p1 in minp1:maxp1)
  {
    for(p2 in minp2:maxp2)
    { 
      for(p3 in minp3:maxp3)
      {
        for(p4 in minp4:maxp4)
        {
          print(paste(p1*step,p2*step,p3*step , p4*step))
          
          start =list(p1=p1*step,p2=p2*step, p3 = p3*step, p4 = p4*step)
          
          params=C_MLE(func,start,1)
          
          if(!is.null(params))
          {
            print( paste("OK:" , p1, p2,p3,p4,  " params:",params@coef[1], params@coef[2], params@coef[3] , params@coef[4]))
            assign("last_start",start, envir = .GlobalEnv)
            last_start <<-start
            #print(last_start)
            return(params)
          }
        }
      }
    }
  }
  
  return(NULL)
}