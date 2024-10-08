library(distr)
library(VaRES)




sigmoid = function(x) {
  1 / (1 + exp(-x))
}

x <- seq(-5, 5, 0.01)

plot(x, sigmoid(x), col='blue')
abline(h = 0, v = 0, col = "gray60")

tanh = function(x) {
  (exp(x)-exp(-x))/(exp(x)+exp(-x))
}

plot(x, tanh(x), col='blue')
abline(h = 0, v = 0, col = "gray60")

Z_yearly = get_detailed_simulation_results_for_method("gjrgarch-arma-std",rewrite =  FALSE)

outpath= paste(getwd(),"/../Results/DFYearlySummary.txt",sep="")
write.table(Z_yearly, outpath ,sep="\t",col.names=NA,dec=",")