################################################################################
#' @name threshold_entropy
#' @title Entropy-based thresholding
#'
#' @description Entropy based thresholding based on Kapur et al. (1985).
#' @param x vector of (pixel) gray levels to be thresholded
#' @param range_constrain constrain the range from which to select the threshold
#' @param ... arguemnts passed to hist, e.g. breaks
#' @export
threshold_entropy <- function(x, range_constrain=NULL, ...) {
  
  #@references  'J. N. Kapur, P. K. Sahoo, and A. K. C. Wong, 1985, A new method for gray-level picture thresholding using the entropy of the histogram, Graph. Models Image Process.29, 273–285.'
  # also used in 
  # Patra, Swarnajyoti, und Lorenzo Bruzzone. „A Fast Cluster-Assumption Based Active-Learning Technique for Classification of Remote Sensing Images“. IEEE Transactions on Geoscience and Remote Sensing 49, Nr. 5 (Mai 2011): 1617–26. doi:10.1109/TGRS.2010.2083673.
  
  .get_hist <- 
    function(x, ...) {
      ### generate the histogram
      h <- hist(x, ...)
      g <- h$mids 
      h <- h$counts
      h[h==0] <- 1 # add at least one count for numerical stability
      
      return(list(g=g, h=h, pmf=h/sum(h), n=length(h)))
    }
  ### calculate the entropy of the positive and negative class
  Et <- function(t, p) {
    P.neg <- sum(p[1:t])/sum(p)
    P.pos <- 1 - P.neg
    E.neg <- -sum( ( p[1:t]/P.neg ) * log2( p[1:t]/P.neg ) )
    E.pos <- -sum( ( p[(t+1):length(p)]/P.pos ) * log2( p[(t+1):length(p)]/P.pos ) )
    return(c(Et.neg=E.neg, Et.pos=E.pos))
  }
  
  h <- .get_hist(x, plot=FALSE, ...) # ...
  x_eval=h$g
  ### get the sum of the entropies for all thresholds
  optimize <- colSums(sapply(1:length(h$pmf), Et, p=h$pmf))
  
  if (!is.null(range_constrain)) {
    sub_idx <- which(h$g >= range_constrain & range_constrain<=h$g)
    sub_optimze <- optimize[sub_idx]
    th_idx <- which.max(optimize)
    th_idx <- sub_idx[th_idx]
  } else {
    th_idx <- which.max(optimize)
  }
  
  threshold = h$g[th_idx]
  
  if (is.null(threshold))
    threshold <- NA
  
  
  h = hist(x, breaks="Scott", plot=FALSE)
  model=list(x = x_eval,
             density = NULL,
             optcrit = optimize,
             opt ='max',
             h = h,
             constrain = range_constrain)
  
  attr(threshold, "method") <- 'entropy'
  attr(threshold, "model") <- model
  
  threshold <- structure(threshold, class="threshold")
  
  return(threshold)
}
