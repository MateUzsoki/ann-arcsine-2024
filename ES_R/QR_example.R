base=read.table("http://freakonometrics.free.fr/rent98_00.txt",header=TRUE)
attach(base)
library(quantreg)
library(lpSolve)
tau <- 0.3


# Problem (1) only one covariate
X <- cbind(1,base$area)
K <- ncol(X)
N <- nrow(X)

A <- cbind(X,-X,diag(N),-diag(N))
c <- c(rep(0,2*ncol(X)),tau*rep(1,N),(1-tau)*rep(1,N))
b <- base$rent_euro
const_type <- rep("=",N)

linprog <- lp("min",c,A,const_type,b)
linprog$sol
beta <- linprog$sol[1:K] -  linprog$sol[(1:K+K)]
beta

rq(rent_euro~area, tau=tau, data=base)


# Problem (2) with 2 covariates
X <- cbind(1,base$area,base$yearc)
K <- ncol(X)
N <- nrow(X)


A <- cbind(X,-X,diag(N),-diag(N))
c <- c(rep(0,2*ncol(X)),tau*rep(1,N),(1-tau)*rep(1,N))
b <- base$rent_euro
const_type <- rep("=",N)

linprog <- lp("min",c,A,const_type,b)
beta <- linprog$sol[1:K] -  linprog$sol[(1:K+K)]
beta
rq(rent_euro~ area + yearc, tau=tau, data=base)
