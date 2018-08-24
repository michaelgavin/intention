#' Measure normalized conceptual work
#' 
#' @export
conceptual_work_adj = function(freq, cos_dist, threshold) {
  hits = which(freq > 0)
  threshold_scores = rep(0, length(freq))
  threshold_scores[hits] = threshold[freq[hits]]
  adj_cos_dist = cos_dist - threshold_scores
  adj_cos_dist[is.na(adj_cos_dist)] = 0
  wk = freq * adj_cos_dist
  return(wk)
}