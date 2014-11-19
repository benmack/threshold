#' @import logspline
#' @export
find_threshold <- function(x, method='rosin', x_eval=101, ...) {
  require(logspline)
  
  all_thresholds <- vector('list', length(methods))
  names(all_thresholds) <- method
  
  if (length(x_eval)==1)
    x_eval <- seq(min(x), max(x), length.out=x_eval)
  
  if (method=='rosin') {
    all_thresholds[['rosin']] <- threshold_rosin(x, x_eval=x_eval)
  }
  if (method=='valleys') {
    all_thresholds[['valleys']] <- threshold_valleys(x, x_eval=x_eval)
  }
  if (method == 'nmm') {
    all_thresholds[['nmm']] <- threshold_nmm(x, x_eval=x_eval, ...)
  }
  if (method == 'entropy') {
    all_thresholds[['entropy']] <- threshold_entropy(x)
  }
  
  if (length(all_thresholds)==1) {
    all_thresholds <- all_thresholds[[1]]
  } else {
    # create a class: 'list_of_thresholds' 
  }
  return(all_thresholds)
}

