library(VaRES)

options(OutDec= ",")



par(mfrow=c(1,2))

plot(dchen, from=0, to=2, n=100,xlab='x',ylab='f(x)')

par(new=TRUE)

plot(pchen, from=0, to=2, n=100,xlab='x',ylab='F(x)')

##

par(mfrow=c(1,1))

dexppois1 <- function(x)
{
  return(dexppois(x,lambda =1))
}

dexppois2 <- function(x)
{
  return(dexppois(x,lambda =2))
}

dexppois3 <- function(x)
{
  return(dexppois(x,lambda =3))
}


x <- seq(0, 2, 0.02)

options(OutDec= ",")

plot(x, dexppois(x,lambda=3), type="l", col="blue", pch="o", ylab="f(x)", lty=1)

lines(x, dexppois(x,lambda=2), col="red",lty=2)
#points(x, dexppois(x,lambda=2), col="red", pch="*")

lines(x, dexppois(x,lambda=1), col="black",lty=3)
#points(x, dexppois(x,lambda=3), col="black", pch="+")

legend(1.8,3,legend=c("?? = 3","?? = 2","?? = 1"), col=c("blue","red","black")
       , lty=c(1,2,3), ncol=1)


plot(dexppois1, from=0, to=2, n=100,xlab='x',ylab='f(x)')

par(new=TRUE)

plot(dexppois2, from=0, to=2, n=100,xlab='x',ylab='f(x)')

par(new=TRUE)

plot(dexppois3, from=0, to=2, n=100,xlab='x',ylab='f(x)')

##

plot(qnorm,from=pnorm(-5),to=pnorm(5),n=2001)

plot(qnorm,from=pschabe(0),to=pschabe(5),n=2001)

plot(dnorm,from=-1,to=1,n=100)
plot(dschabe, from = -1, to = 1, 100)

plot(dT,from=-1,to=1,n=100)

plot(ddagum, from=-2,to=0,n=100)

plot(dclg, from=-2,to=10,n=100)



plot(qTL2, from=0,to=1,n=1000)

plot(dtriangular, from=0, to=2, n=100,xlab='x',ylab='f(x)')
plot(ptriangular, from=0, to=2, n=100,xlab='x',ylab='F(x)')

plot(dlfr, from=0, to=3, n=100,xlab='x',ylab='f(x)')
plot(plfr, from=0, to=3, n=100,xlab='x',ylab='F(x)')


plot(darcsine, from=0, to=1, n=100,xlab='x',ylab='f(x)')
plot(parcsine, from=0, to=1, n=100,xlab='x',ylab='F(x)')

plot(dburr7, from=0, to=5, n=100,xlab='x',ylab='f(x)')
plot(pburr7, from=0, to=5, n=100,xlab='x',ylab='F(x)')

plot(dexpext, from=0, to=5, n=100,xlab='x',ylab='f(x)')
plot(pexpext, from=0, to=5, n=100,xlab='x',ylab='F(x)')



dkum2<- function(x) {
dkum(x,a=2,b=2)
}

pkum2<- function(x) {
  pkum(x,a=2,b=2)
}

plot(dkum2, from=0, to=1, n=100,xlab='x',ylab='f(x)')
plot(pkum2, from=0, to=1, n=100,xlab='x',ylab='F(x)')

plot(dlomax, from=0, to=5, n=100,xlab='x',ylab='f(x)')
plot(plomax, from=0, to=5, n=100,xlab='x',ylab='F(x)')


plot(dTL2, from=-0,to=1,n=1000,xlab='x',ylab='f(x)')
plot(pTL2, from=-0,to=1,n=1000,xlab='x',ylab='F(x)')

dTL22<- function(x)
{
  dTL2(x,b=2)
}

pTL22<- function(x)
{
  pTL2(x,b=2)
}


plot(dTL22, from=-0,to=1,n=1000,xlab='x',ylab='f(x)')
plot(pTL22, from=-0,to=1,n=1000,xlab='x',ylab='F(x)')

plot(dkumburr7, from=-0,to=1,n=1000,xlab='x',ylab='f(x)')
plot(pkumburr7, from=-0,to=1,n=1000,xlab='x',ylab='F(x)')

plot(dtsp, from=-0,to=1,n=1000,xlab='x',ylab='f(x)')
plot(ptsp, from=-0,to=1,n=1000,xlab='x',ylab='F(x)')

dtsp2 <-function(x) { dtsp(x,a=5) }
ptsp2 <-function(x) { ptsp(x,a=5) }

plot(dtsp2, from=0,to=1,n=1000,xlab='x',ylab='f(x)')
plot(ptsp2, from=0,to=1,n=1000,xlab='x',ylab='F(x)')

qnorm(0.975)

options(OutDec= ",")

plot(tanh, from=-5, to=5, n=100,xlab='x',ylab='tanh(x)')

library(sigmoid)
#sigmoid <-function(x) { 1/(1+exp(-x)) }

plot(sigmoid, from=-5, to=5, n=100,xlab='x',ylab='sigmoid(x)')

#sigmoid <-function(x) { 1/(1+exp(-x)) }

plot(relu, from=-5, to=5, n=100,xlab='x',ylab='relu')


options(OutDec= ".")

a =  qnorm



