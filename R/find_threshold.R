#' @name find_threshold
#' @title ...
#' @description ...
#' @param x ...
#' @param method ...
#' @param x_eval ...
#' @param ... ...
#' @export
find_threshold <- function(x, method='rosin', x_eval=101, ...) {

  all_thresholds <- vector('list', length(method))
  names(all_thresholds) <- method
  
  if (length(x_eval)==1)
    x_eval <- seq(min(x), max(x), length.out=x_eval)
  
  if (any(method=='rosin')) {
    all_thresholds[['rosin']] <- threshold_rosin(x, x_eval=x_eval)
  }
  if (any(method=='valleys')) {
    all_thresholds[['valleys']] <- threshold_valleys(x, x_eval=x_eval)
  }
  if (any(method == 'nmm')) {
    all_thresholds[['nmm']] <- threshold_nmm(x, x_eval=x_eval, ...)
  }
  if (any(method == 'entropy')) {
    all_thresholds[['entropy']] <- threshold_entropy(x)
  }
  
  if (length(all_thresholds)==1) {
    all_thresholds <- all_thresholds[[1]]
  } else {
    all_thresholds <- structure(all_thresholds, class='threshold_list')
  }
  return(all_thresholds)
}

