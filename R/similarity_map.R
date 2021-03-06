#' Create a semantic map of a keyword
#' 
#' Build a scatterplot that represents the conceptual structure of a keyword,
#' showing the most semantically similar terms.
#' 
#' @param mat A word-context matrix.
#' 
#' @param keyword A string.
#' 
#' @param margin Numeric value: 1 or 2. If 1, calculations are performed over the rows. If 2, over
#'              the columns.
#' 
#' @param numResults Numeric value. The number of words to be displayed in the 
#'                   graph. Default is 40.
#'  
#'       
#' @return A scatterplot showing structure of 40 most-similar words.
#' 
#' @section What it does:
#' This function runs \code{\link{similarity}} over a word-context matrix and looks for the
#' thirty most similar terms, then clusters them.
#' @export
similarity_map = function(mat, 
                          keyword, 
                          margin = 1,
                          numResults = 40) {
  # Get words most similar to keyword
  results = similarity(mat = mat, vec = keyword, margin = margin, fullResults = T)

  # Set correlation method
  corr_method = function(x, y) { x %*% y/(sqrt(x %*% x) * sqrt(y %*% y)) }
  
  words = names(sort(results, decreasing = T))[1:numResults]

  # Now build a correlations matrix among the words
  if (margin == 1) {
    mat = mat[words, ]
    correlations = matrix(0, length(words), length(words))
    for (i in 1:nrow(correlations)) {
      correlations[i, ] = apply(mat, 1, corr_method, mat[i, ])
    }
    rownames(correlations) = words
    xy <- data.frame(cmdscale(dist(correlations)), factor(words))
    names(xy) <- c("x", "y","words")
    
    xdiff = (max(xy$x) - min(xy$x)) / 4
    ydiff = (max(xy$y) - min(xy$y)) / 4
    p = ggplot(xy, aes(x, y, label = words))
    
    p = p +
      geom_text(colour = "black") +
      theme_bw() +
      xlim(min(xy$x) - xdiff, max(xy$x) + xdiff) +
      ylim(min(xy$y) - ydiff, max(xy$y) + ydiff) +
      ylab("") +
      xlab("") +
      ggtitle(keyword) +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            legend.position = 'none',
            text = element_text(size = 2),
            title = element_text(size=10, face='bold'))
    print(p)
  }
}