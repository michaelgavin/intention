#' Measure normalization threshold
#' 
#' @export
set_threshold = function(avgs, devs, mu = NULL) {
  x = 1:length(avgs)
  fita = lm(avgs~log(x))
  anew = predict(fita,newdata=list(x=x),interval="confidence")
  fitd = lm(devs~log(x))
  dnew = predict(fitd,newdata=list(x=x),interval="confidence")
  delta = anew[,1]
  psi = dnew[,1]
  if (any(mu) == F) {
    mu = (1-delta[1]) / psi[1]
  }
  theta = delta + mu * psi
  res = list()
  res$mu = mu
  res$avg_alpha = coefficients(fita)[2]
  res$avg_beta = coefficients(fita)[1]
  res$psi_alpha = coefficients(fitd)[2]
  res$psi_beta = coefficients(fitd)[1]
  res$delta = delta
  res$psi = psi
  res$theta = theta
  return(res)
}