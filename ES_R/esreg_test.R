library(esreg)
source("DALC.R")
source("constants.R")
source("ESlib.R")
library(ggplot2)
library(PerformanceAnalytics)
methods = c('egarch-arma-norm')
method = methods[1]
tickers = get_tickers()
   
selected = tickers[1]
###
params = NULL
fit = NULL
singleStock = get_returns_for_single_stock(selected,start_date)

stockName = get_stockname(selected)

x = singleStock$LogReturn[1:250]

x[x > 0] = 0
x[x < 0] = 1

y = singleStock$LogReturn[2:251]

alpha=0.025

fit = esreg(y ~ x, alpha=alpha)

forecast = fit

z =  singleStock$LogReturn[251]

z[z > 0] = 0
z[z < 0] = 1

forecast$xq = c(1,z)
forecast$xe = c(1,z)

estimates =  fitted(forecast)

VaR = estimates[1]*-1
ES = estimates[2]*-1

N = length(X)


path= GetStockOutputPath(method,stockName)

DFHS = MultiPeriodES(singleStock = singleStock,method = 'historical',selected =stockName)

DF <- data.frame(X=rep(NA,N-T), ES=rep(NA,N-T), VAR=rep(NA,N-T), stringsAsFactors = FALSE )




######
# set.seed(0)
# x = rnorm(1000,sd = 10)
# r = rnorm(1000)
# y =   x+10+r
# 
# quantile(y,0.025)
# quantile(y,0.975)
# 
# 
# 
# x[6]
# y[6]
# 
# fit = esreg(y ~ x, alpha=0.025)
# 
# fitted(fit)[6,]
# 
# #VaR
# fit$coefficients_q[1] *  fit$xq[6,1] + fit$coefficients_q[2] *  fit$xq[6,2]
# 
# #ES
# fit$coefficients_e[1] *  fit$xe[6,1] + fit$coefficients_e[2] *  fit$xe[6,2]  

######


coef(fit)
summary(fit)
fit$xq = c(1,0)
fit$xe = c(1,0)
fit$loss
fit$formula
fit$y[1000]
y[1000]




fitted()
cov1 <- vcov(object=fit, sparsity="iid", cond_var="ind")
cov2 <- vcov(object=fit, sparsity="nid", cond_var="scl_N")
cov3 <- vcov(object=fit, sparsity="nid", cond_var="scl_sp")
