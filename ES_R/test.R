# asylaplace works
# aep  -> sd(x) , works
# aep_std works
#ast works -> sd(x)
#f distribution -> works
#frechet works


library(VaRES)

set.seed(1001)


N <- 10000

x <- rnorm(N, mean = 0, sd = 1)

mean(x)

sd(x)

pnorm(-1,0,1)

dx = pnorm(runif(100000), 0, 1)
df = data.frame()

losses = x
xx = -50:50/100
DF <- data.frame(X=xx
                 #,frechet = pfrechet(xx+params@coef[3]-minLosses,params@coef[1],params@coef[2])
                 ,f = pf(xx+params@coef[3]-minLosses,params@coef[1],params@coef[2])
                 #,AEP=daep(xx, q1=q1,q2=q2,alpha=sd(x)) 
                 #,AEPSTD=daep(xx) 
                 , norm=pnorm(xx-minLosses,mean = mean(x),sd = sd(x) )
          #       , ast=dast(xx)
                 
                 #,asylaplace=dasylaplace(xx,tau=tau, kappa=kappa,theta=theta)
                 #,asylaplaceSTD=dasylaplace(xx)
                 
                 
                 , stringsAsFactors = FALSE )


esfrechet(0.025,params@coef[1],params@coef[2])- as.numeric(params@coef[3]) + minLosses
esnormal(0.025,mu =  mean(x),sigma = sd(x))

varfrechet(0.025,params@coef[1],params@coef[2])- as.numeric(params@coef[3])+minLosses
varnormal(0.025,mu =  mean(x),sigma = sd(x))+minLosses

ggplot(data = DF , aes(x= X)) +  geom_line(size=1,colour='blue',aes(y=f))  +  geom_line(size=1,colour='red', aes(y=norm )) + geom_hline(yintercept = 0.025)

+  geom_line(size=1,colour='blue',aes(y=frechet))

 + geom_line(size=1, aes(y=AEP))

+ geom_line(size=1,colour='magenta', aes(y=AEPSTD))

+ geom_line(colour='green',aes(y=asylaplace))+ geom_line(colour='magenta',aes(y=asylaplaceSTD)) 
+ geom_line(colour='red',aes(y=ast))


mean(losses)
sd(losses)

tau = params@coef[1]
kappa = params@coef[2]
theta = params@coef[3]

q1 = params@coef[1]
q2 = params@coef[2]
alpha = params@coef[3]

cvar::ES(paep,dist.type = "cdf", intercept = 0,q1 = q1, q2=2,alpha=alpha , slope = 1, x=0.025 )

esaep(0.025,q1 = q1,q2=q2,alpha=alpha)


CVAR::

esaep(0.025)
esast(0.025)
esnormal(0.025)
esarcsine(0.025)
esasylaplace(0.025)

varaep(0.025)
varast(0.025)
varnormal(0.025)
vararcsine(0.025,a=-2,b=1)
varasylaplace(0.025)



LL <- function(mu, sigma) {
     R = dnorm(x, mu, sigma)
    -sum(log(R))
}

library(stats4)
    
params2 = mle(LL, start = list(mu = 1, sigma=1))
esnormal(0.025, mu = params2@coef[1],sigma = params2@coef[2] )

 
LLast <- function(nu1, nu2, alpha) {
  R = dast(x, nu1, nu2, alpha, log=FALSE)
  -sum(log(R))
}   

params = mle(minuslogl =  LLast, start = list(nu1=100, nu2=0.1, alpha=1000) )

nu1 = params@coef[1]
nu2 = params@coef[2]
alpha = params@coef[3]

esast(0.025)
esast(0.025, nu1=100, nu2=0.1, alpha =1000)
esast(0.025, nu1=nu1, nu2=nu2, alpha=alpha)


params = mle(minuslogl =  LLaep, start = list(q1=1, q2=1, alpha=0.5) )

paep(x, q1=1, q2=1, alpha=0.5, log.p=FALSE, lower.tail=TRUE)
varaep(p, q1=1, q2=1, alpha=0.5, log.p=FALSE, lower.tail=TRUE)
esaep(p, q1=1, q2=1, alpha=0.5)


x=rnorm(10000,0,1)
param=NULL



#AEP

for(q1 in 0:100)
{
  for(q2 in 0:100)  
  {
   
      print(paste(q1/10,q2/10))
      param=C_MLE(LLaep,start=list(q1=q1/10,q2=q2/10,alpha=0.2),1)
      if(!is.null(param))
      {
        print( paste("ezj?:" , i1, i2, " param:",param@coef[1], param@coef[2]))
        stop("wow")
      }
    
  }
}


param=NULL
s = sd(x)
m = mean(x)

#AsylLaplace


cvar::VaR(pasylaplace,dist.type = "cdf", intercept = m, slope = s,x=0.025 )
cvar::ES(pasylaplace,dist.type = "cdf", intercept = m, slope = s ,x=0.025)

es = esasylaplace(0.025)*s
es = es+m
es


for(kappai in 0:10)
{
  for(taui in 5:100)  
  {
    for(thetai in -100:100)
    {
      print(paste("taui:",taui,"thetai:",thetai,"kappai:",kappai))
      param=C_MLE(LLasylaplace,start=list(tau=taui/10,theta=thetai/50,kappa=kappai/10),1)
      if(!is.null(param))
      {
        print( paste("ezj?:" , i1, i2, " param:",param@coef[1], param@coef[2]))
        stop("wow")
      }
    }
  }
}

#f

for(d1 in 0:100)
{
  step=0.1
  for(d2 in 0:100)  
  {
    
    print(paste(d1*step,d2*step))
    param=C_MLE(LLf,start=list(d1=d1*step,d2=d2*step, loc = -0.01),1)
    if(!is.null(param))
    {
      print( paste("ezj?:" , d1, d2, " param:",param@coef[1], param@coef[2]))
      stop("wow")
    }
    
  }
}


#frechet

for(a in 0:100)
{
  step=0.1
  for(s in 0:100)  
  {
    
    print(paste(a*step,s*step))
    param=C_MLE(LLfrechet,start=list(a=a*step,s=s*step),1)
    if(!is.null(param))
    {
      print( paste("ezj?:" , a, s, " param:",param@coef[1], param@coef[2]))
      stop("wow")
    }
    
  }
}

min(x)
x= x1

mean(losses)
mean( dfrechet(1:1000/10000))


if(min(x)<0)
{
  x = x-min(x)
}
dfrechet(x, alpha = alpha,sigma=sigma, log=TRUE)

R = dfrechet(x, alpha = 1,sigma=1, log=FALSE)
x[5]
R[5]
-sum(log(R))



x[65]
sum(R)
R



#Weibull

for(a in 0:100)
{
  step=0.1
  for(s in 0:100)  
  {
    
    print(paste(a*step,s*step))
    param=C_MLE(LLWeibull,start=list(a=a*step,s=s*step,loc=0.1),1)
    if(!is.null(param))
    {
      print( paste("ezj?:" , a, s, " param:",param@coef[1], param@coef[2]))
      stop("wow")
    }
    
  }
}

min(x)
x

fit.gamma <- fitdist(x+0.1, distr = "gamma", method = "mle")
summary(fit.gamma)

params=C_MLE(LLGamma,start=list(a=91,b=443,loc=0.1),1)
