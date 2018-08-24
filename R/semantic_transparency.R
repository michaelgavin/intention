#' Measure semantic transparency
#' 
#' @export
semantic_transparency = function(work) {
  work = work[!is.na(work)]
  work = work[work > 0]
  n = length(work)
  p = work / sum(work)
  st = -1 * (p %*% log(p, base = n))
  return(st)
}