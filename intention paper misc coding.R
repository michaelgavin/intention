id = "A30616"
wk = work[,id]
d = distances[,id]
freqs = as.integer(wk / d)
names(freqs) = names(wk)
adj_dev = d - theta[freqs]
adj_wk = adj_dev * freqs
words = names(sort(wk, decreasing = T))[1:13]

plot(freqs,d, main = id, pch=20, col="gray", ylim = c(0,1))
lines(x=1:max(freqs, na.rm=T), theta[1:max(freqs, na.rm=T)], lwd=3, col="black")
text(freqs[words],d[words],labels = words)

# Adjusted work per work
n = length(which(freqs > 0))
ord = which(adj_dev > 0)
sum((adj_dev[ord] * freqs[ord]) / n)



set_threshold = function(avgs, devs, mu = NULL) {
  if (any(mu) == F) {
    mu = (1-avgs[1]) / devs[1]
  }
  x = 1:length(avgs)
  y = avgs + (mu * devs)
  fit = lm(y~log(x))
  ynew = predict(fit,newdata=list(x=x),interval="confidence")
  theta = ynew[,1]
  browser()
  return(theta)
}



# Comparing Locke to Locke Contexts
dists = c()
for (i in 1:nrow(m)) {
  print(i)
  dists = c(dists, semantic_distance(m[i,],locke_mat[i,]))
}
names(dists) = rownames(m)
wk = dists * deebo[,"A48901"]

locke_dists = dists
locke_wk = wk

dists = c()
for (i in 1:nrow(m)) {
  print(i)
  dists = c(dists, semantic_distance(m[i,],eebo[i,]))
}
names(dists) = rownames(m)
wk = dists * deebo[,"A48901"]

e_dists = dists
e_wk = wk

dists = c()
for (i in 1:nrow(m)) {
  print(i)
  dists = c(dists, semantic_distance(locke_mat[i,],eebo[i,]))
}
names(dists) = rownames(m)
wk = dists * deebo[,"A48901"]

