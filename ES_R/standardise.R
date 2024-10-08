
Range01 <- function(x){
  x2= x - min(x)
  x3= x2 / (max(x)-min(x))
  return (x3)
  #(x-min(x))/(max(x)-min(x))
}

DeRange01 <- function(x3, y_min, y_max){
  
  x4= x3*(y_max-y_min)
  x5 = x4 + y_min
  return(x5)
  
}

MinMax <-function (x)
{
  print(min(x))
  print(max(x))
}

GetSubLoc <- function(method, x)
{
  subLoc = 0
  if(
    method == "betadist" ||
    method == "f" || method=="frechet" 
     || method =="Gamma" || method=="genpareto" #genpareto nincs
    || method=="Weibull" || method=="quad"   #quad nincs haszn?lva
       )
  {
    subLoc = min(x)
  }
   else if(  method == "beard" #nincs
           || method =="burr" || method =="burr7"
           || method=="chen" || method=="compbeta" 
           || method == "dagum" #nincs
           || method == "expexp" || method == "expext" 
           || method == "expgeo" #nincs
           || method == "explog"  || method=="exppois"  
           || method == "genpowerweibull" 
           || method == "kum" || method == "kumburr7"|| method == "kumexp" 
           || method == "kumgamma" #nincs#
           || method == "kumhalfnorm" 
           || method == "kumloglogis" #nincs
           || method == "kumpareto" #nincs
           || method == "kumweibull" #nincs
           || method == "lfr" || method == "LNbeta" || method == "loggamma"  || method =="logisexp"|| method =="logisrayleigh" 
           || method=="loglaplace" #nincs
           ||  method == "logcauchy" || method == "loglog"  
           || method == "loglogis" #nincs
           || method == "lognorm" || method == "lomax"
           || method == "moexp" #nincs
           || method == "moweibull" || method == "MRbeta"
           || method == "nakagami" # nincs
           || method == "paretostable" #nincs
     #      || method == "schabe"
           || method == "stacygamma"
           || method == "tsp" || method== "TL2"
           || method == "uniform" #nincs
           || method == "xie")
   {
     subLoc = min(x) - 0.0001
   }
  
  else if (method == "invgamma"   )
  {
    subLoc = min(x) - 0.01
  }
  
  else if (method == "schabe"   )
  {
    subLoc = mean(x) + 0.5
  }
  else if (method == "t")
  {
    subLoc = mean(x)
  }
  
  
  return(subLoc)
}


GetDivScale <- function(method,x)
{
  divScale = 1
  
  if(method=="betadist" || method == "arcsine" || method=="compbeta"
     || method == "t" || method == "tsp" || method== "TL2"
     
     || method == "kum"
     || method == "LNbeta"  || method == "loggamma" 
     || method == "MRbeta"
     || method == "uniform"
     || method == "xie")
  {
    maxAllowed = 0.99
    maxx = max(x)
    
    if(maxx > maxAllowed)
    {
       divScale =  maxx / maxAllowed
    }
  }
  
  return(divScale)
  
}
