
library(rugarch)

#spec = ugarchspec()

#nrow(expand.grid(GARCH = 1:14, VEX = 0:1, VT = 0:1, Mean = 0:1, ARCHM = 0:2, ARFIMA = 0:1, MEX = 0:1, DISTR = 1:10))

#Valid choices are
#"norm", "snorm", "std", 
#"sstd" Skew Student-T Distribution
#, "ged" Generalized Error Distribution , "sged", "nig", "jsu
spec = ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(2, 2)), distribution = 'std')

#setstart(spec)<- list(shape = 5)
#setbounds <-spec

modelfit = ugarchfit(spec, x, solver = 'hybrid', out.sample=1)


modelfor = ugarchforecast(modelfit, data = NULL, n.ahead = 1, n.roll = 1, out.sample = 1)

plot(modelfor@forecast$seriesFor)

modelfit@fit$coef["mu"]
uncmean(modelfit)

modelfit@fit$coef

sigma(modelfor)
fitted(modelfor)


quantile(modelfor,0.05)

qnorm(0.05)*sigma(modelfor)+fitted(modelfor)


modelroll=ugarchroll (
  spec=model, data=x, n.ahead = 1, forecast.length = 100,
  n.start = NULL, refit.every = 50, refit.window = c("recursive"),
  window.size = NULL, solver = "hybrid", fit.control = list(),
  solver.control = list() ,  calculate.VaR = TRUE, VaR.alpha = c(0.01,
                                                               0.05),
  cluster = NULL, keep.coef = TRUE
)


data(sp500ret)

model=ugarchspec (
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)
modelfit=ugarchfit(model,data=sp500ret,out.sample=2)

modelfit@model$modeldata$T

modelfor=ugarchforecast(modelfit, data = NULL, n.ahead = 1, n.roll
                        = 2, out.sample = 2)


sigma(modelfor)
# 2009-01-28 2009-01-29 2009-01-30
# T+1 0.02413003 0.02512952 0.02492642
fitted(modelfor)
# 2009-01-28 2009-01-29 2009-01-30
# T+1 0.0005205228 0.0005205228 0.0005205228

coef(modelfit)["mu"]
residuals(modelfit)[5521]


#Getting the expected return from the estimated model
mu=coef(modelfit)["mu"]
#Getting the relevant observed return from last
# two periods of sp500ret
return=sp500ret[5522:5523,]
#Getting residual of period T=5521 from modelfit
e5521=as.vector(residuals(modelfit)[5521])
#Calculating residuals of period T=5522 and 5523
e5522=return[1]-mu
e5523=return[2]-mu
#Taking estimated parameters from modelfit
theta=coef(modelfit)

#Making function for forecast
.fgarch=function(e,sigma_0,theta) {
  omega=theta["omega"]
  alpha=theta["alpha1"]
  beta=theta["beta1"]
  sigma_1 = sqrt(omega + alpha*e^2 + beta*sigma_0^2)
  names(sigma_1)=NULL
  return(sigma_1)
}

#Getting estimated sigma for period T=5521 from modelfit
sigma5521=as.vector(sigma(modelfit)[5521])
#Forecast sigma_5522 and comparing with rugarch forecast
sigma5522=.fgarch(e5521,sigma_0=sigma5521,theta)
sigma5522
sigma(modelfor)[1]
#Forecast sigma_5523 and comparing with rugarch forecast
sigma5523=.fgarch(e5522,sigma_0=sigma5522,theta)
sigma5523
sigma(modelfor)[2]
#Forecast sigma_5523 and comparing with rugarch forecast
sigma5524=.fgarch(e5523,sigma_0=sigma5523,theta)
sigma5524
sigma(modelfor)[3]

coef(modelfit)["mu"]
sigma(modelfor)
fitted(modelfor)


quantile(modelfor,0.05)


####

data(dji30ret)
spec = ugarchspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  variance.model = list(model = "gjrGARCH"), distribution.model = "sstd")
fit = ugarchfit(spec, data = dji30ret[1:1000, 1, drop = FALSE])
spec2 = spec
setfixed(spec2)<-as.list(coef(fit))
filt = ugarchfilter(spec2, dji30ret[1001:2500, 1, drop = FALSE], n.old = 1000)
actual = dji30ret[1001:2500,1]
# location+scale invariance allows to use [mu + sigma*q(p,0,1,skew,shape)]
VaR = fitted(filt) + sigma(filt)*qdist("sstd", p=0.05, mu = 0, sigma = 1, 
                                       skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
# calculate ES
f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                      skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
ES = fitted(filt) + sigma(filt)*integrate(f, 0, 0.05)$value/0.05
print(ESTest(0.05, actual, ES, VaR, boot = TRUE))
# }


###########


spec = ugarchspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE),   variance.model = list(model = "gjrGARCH"), distribution.model = "sstd")

ret = x  * - 1

fit = ugarchfit(spec, data = ret , out.sample = 1)
show(fit)
conf = 0.025
modelfor=ugarchforecast(fit, data = NULL, n.ahead = 1, n.roll = 1, out.sample = 1)
modelfor@model$pars
mean(ret)

VaR = ((fitted(modelfor) + sigma(modelfor)*qdist("sstd", p=conf, mu = 0, sigma = 1, skew  = coef(fit)["skew"], shape=coef(fit)["shape"]) )*-1)[2]
# calculate ES
f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1,  skew  = coef(fit)["skew"], shape=coef(fit)["shape"])

ES = ((fitted(modelfor) + sigma(modelfor)*integrate(f, 0, conf)$value/conf) * -1)[2]

VaR
ES

####

class(dji30ret)

dji30ret = x*-1
spec = ugarchspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  variance.model = list(model = "gjrGARCH"), distribution.model = "sstd")
fit = ugarchfit(spec, data = dji30ret[1:200])
spec2 = spec
setfixed(spec2)<-as.list(coef(fit))
filt = ugarchfilter(spec2, dji30ret[201:250], n.old = 100)
actual = dji30ret[201:250]
# location+scale invariance allows to use [mu + sigma*q(p,0,1,skew,shape)]
VaR = fitted(filt) + sigma(filt)*qdist("sstd", p=0.025, mu = 0, sigma = 1, 
                                       skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
# calculate ES
f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                      skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
ES = fitted(filt) + sigma(filt)*integrate(f, 0, 0.025)$value/0.025
print(ESTest(0.025, actual, ES, VaR, boot = TRUE))

####
source("DALC.R")
source("constants.R")
source("A_Sz.R")
source("ESlib.R")
source("HistoricalSimulation.R")
tickers = get_tickers()
selected = 
singleStock = get_returns_for_single_stock(tickers[1],start_date)$LogReturn*-1


dji30ret <- data.frame("AAPL" = get_returns_for_single_stock(tickers[1],start_date)$LogReturn[1:7559]*-1)

rownames(dji30ret) <- get_returns_for_single_stock(tickers[1],start_date)$Date

spec = ugarchspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  variance.model = list(model = "gjrGARCH"), distribution.model = "sstd")

data = dji30ret[1:3500, 1, drop = TRUE]
fit = ugarchfit(spec, data =data,solver = 'hybrid')

spec2 = spec
setfixed(spec2)<-as.list(coef(fit))


actual = dji30ret[3501:7559, 1, drop = TRUE]

filt = ugarchfilter(spec2, dji30ret[3501:7559, 1, drop = TRUE], n.old = 3500 , solver = 'hybrid')

# location+scale invariance allows to use [mu + sigma*q(p,0,1,skew,shape)]
VaR = fitted(filt) + sigma(filt)*qdist("sstd", p=0.025, mu = 0, sigma = 1, 
                                       skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
# calculate ES
f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                      skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
ES = fitted(filt) + sigma(filt)*integrate(f, 0, 0.025)$value/0.025

length(ES)

print(ESTest(0.025, actual, ES, VaR, boot = TRUE))

################

####
source("DALC.R")
source("constants.R")
source("A_Sz.R")
source("ESlib.R")
source("HistoricalSimulation.R")
tickers = get_tickers()
selected = 
  singleStock = get_returns_for_single_stock(tickers[1],start_date)$LogReturn*-1

end = 1000
middle = 250

dji30ret <- data.frame("AAPL" = get_returns_for_single_stock(tickers[1],start_date)$LogReturn[1:end]*-1)

rownames(dji30ret) <- get_returns_for_single_stock(tickers[1],start_date)$Date[1:end]

spec = ugarchspec(mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  variance.model = list(model = "gjrGARCH"), distribution.model = "sstd")

data = dji30ret[1:middle, 1, drop = TRUE]
fit = ugarchfit(spec, data =data,solver = 'hybrid')

fit2 = ugarchfit(spec, data = data ,solver = 'hybrid', out.sample = 1)

params2<-ugarchforecast(fit2, data = NULL, n.ahead = 1, n.roll = 1, out.sample = 1)

VaR2 = fitted(params2) + sigma(params2)*qdist("sstd", p=0.025, mu = 0, sigma = 1, 
                                            skew  = coef(fit2)["skew"], shape=coef(fit2)["shape"])
f2 = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                      skew  = coef(fit2)["skew"], shape=coef(fit2)["shape"])

ES2 = fitted(params2) + sigma(params2)*integrate(f2, 0, 0.025)$value/0.025

conf = 0.025
skew = params2@model$pars[16]

shape = params2@model$pars[17]

f3 = function(x) qdist("sstd", p=x, mu = 0, sigma = 1,  skew  = skew, shape=shape)

modelfor = params2
ES3 = ((fitted(params2) + sigma(params2)*integrate(f3, 0, conf)$value/conf) * -1)



params<-ugarchforecast(fit, data = NULL, n.ahead = 1)
fitted(params)
VaR1 = fitted(params) + sigma(params)*qdist("sstd", p=0.025, mu = 0, sigma = 1, 
                                       skew  = coef(fit)["skew"], shape=coef(fit)["shape"])


spec2 = spec
setfixed(spec2)<-as.list(coef(fit))


actual = dji30ret[(middle+1):end, 1, drop = TRUE]

filt = ugarchfilter(spec2, dji30ret[(middle+1):end, 1], n.old = middle , solver = 'hybrid')

# location+scale invariance allows to use [mu + sigma*q(p,0,1,skew,shape)]
VaR = fitted(filt) + sigma(filt)*qdist("sstd", p=0.025, mu = 0, sigma = 1, 
                                       skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
# calculate ES
f = function(x) qdist("sstd", p=x, mu = 0, sigma = 1, 
                      skew  = coef(fit)["skew"], shape=coef(fit)["shape"])
ES = fitted(filt) + sigma(filt)*integrate(f, 0, 0.025)$value/0.025



print(ESTest(0.025, actual, ES, VaR, boot = TRUE))

length(ES)
ES[1]
VaR[1]
VaR1
VaR[which(VaR[,1] >=  -0.03526  ),1]
VaR[which(VaR[,1] >=  -0.036  ),1]

#???-0.05088472
################