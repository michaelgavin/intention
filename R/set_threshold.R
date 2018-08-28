#' Measure normalization threshold
#' 
#' @export
set_threshold = function(avgs, devs, mu = NA) {
  x = 1:length(avgs)
  fita = lm(avgs~log(x))
  anew = predict(fita,newdata=list(x=x),interval="confidence")
  fitd = lm(devs~log(x))
  dnew = predict(fitd,newdata=list(x=x),interval="confidence")
  delta = anew[,1]
  phi = dnew[,1]
  if (is.na(mu)) {
    mu = (1-delta[1]) / phi[1]
  }
  theta = delta + mu * phi
  res = list()
  res$mu = mu
  res$avg_alpha = coefficients(fita)[2]
  res$avg_beta = coefficients(fita)[1]
  res$phi_alpha = coefficients(fitd)[2]
  res$phi_beta = coefficients(fitd)[1]
  res$delta = delta
  res$phi = phi
  res$theta = theta
  return(res)
}