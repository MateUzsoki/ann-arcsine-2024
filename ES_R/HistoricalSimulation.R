


EShistorical <- function(x,conf, isLoss = FALSE) {
  if(!isLoss)
  {
    losses = x*-1
  }
  return (mean(losses[losses > quantile(losses, conf)]))
}

VaRhistorical <- function(x,conf, isLoss = FALSE) {
  if(!isLoss)
  {
    losses = x*-1
  }
  return(as.numeric(quantile(losses, conf)))
}