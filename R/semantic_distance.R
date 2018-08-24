#' Measure cosine distance between two vectors
#' 
#' @export
semantic_distance = function(x, y) {
  x[is.na(x)] = 0
  y[is.na(y)] = 0
  sim = x %*% y / (sqrt(x %*% x) * sqrt(y %*% y))
  return(1 - sim)
}