

FitDistrOneParameter<- function(p1,func,searchFunc, useOldParams = TRUE) {

  #p1 = 1
  start <- list(p1=p1)
  
  if(!is.null(params) && useOldParams)
  {
    start <-list(p1 =as.numeric(params@coef[1]) )
  }

  params<-C_MLE(func,start, 1)
  
  if(is.null(params))
  {
    start <- list(p1=p1)
    params<-C_MLE(func,start, 1)
  }
  
  if(is.null(params))
  {
    params <- searchFunc()
  }
  
  return(params)
} 

FitDistrTwoParameter<- function(p1,p2,func,searchFunc, useOldParams = TRUE) {
  
    
    start <- list(p1=p1, p2=p2)
    
    if(!is.null(params) && useOldParams)
    {
      start <-list(p1 =as.numeric(params@coef[1]), p2 =as.numeric(params@coef[2]) )
    }
    
    
    params<-C_MLE(func,start, 1)
    
    if(is.null(params))
    {
      start <- list(p1=p1, p2=p2)
      params<-C_MLE(func,start, 1)
    }
    
    if(is.null(params))
    {
      params <- searchFunc()
    }
    
    return(params)
} 

FitDistrThreeParameter<- function(p1,p2,p3,func,searchFunc, useOldParams = TRUE) {
  
  
  start <- list(p1=p1, p2=p2, p3 = p3)
  
  if(!is.null(params) && useOldParams)
  {
    start <-list(p1 =as.numeric(params@coef[1]), p2 =as.numeric(params@coef[2]) , p3=as.numeric(params@coef[3]) )
  }
  
  params<-C_MLE(func,start, 1)
  
  if(is.null(params))
  {
    start <- list(p1=p1, p2=p2, p3 = p3)
    params<-C_MLE(func,start, 1)
  }
  
  if(is.null(params) && !is.null(last_start))
  {
    print("using previous start")
    print(start)
    params<-C_MLE(func,last_start, 1)
  }
  
  if(is.null(params))
  {
    params <- searchFunc()
  }
  
  return(params)
} 

FitDistrFourParameter<- function(p1=1,p2=1,p3=1,p4=1, func,searchFunc, useOldParams = TRUE) {
  
  
  start <- list(p1=p1, p2=p2, p3 = p3,p4 = p4)
  
  if(!is.null(params) && useOldParams)
  {
    start <-list(p1 =as.numeric(params@coef[1]), p2 =as.numeric(params@coef[2]) , p3=as.numeric(params@coef[3]) , p4=as.numeric(params@coef[3]) )
  }
  
  params<-C_MLE(func,start, 1)
  
  if(is.null(params))
  {
    start <- list(p1=p1, p2=p2, p3 = p3,p4 = p4)
    params<-C_MLE(func,start, 1)
  }
  
  if(is.null(params) && !is.null(last_start))
  {
    print("using previous start")
    print(start)
    params<-C_MLE(func,last_start, 1)
  }
  
  if(is.null(params))
  {
    params <- searchFunc()
  }
  
  return(params)
} 