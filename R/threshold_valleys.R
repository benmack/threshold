################################################################################
#' @name threshold_valleys
#' @title Thresholding based on peak and valley analysis.
#'
#' @description A smooth density model is derived for the data with \code{\link[logspline]{logspline}}. Based on the density peaks and valleys are detected and the final threshold is defined as the valley  (if available) with the highest \code{x} value.
#'  
#' @param x vector of (pixel) gray levels to be thresholded
#' @param x_eval ...
#' @param ... arguemnts passed to peaks_and_valleys 
#' @export
threshold_valleys <- function(x, x_eval=101, ...) {
  
  if (length(x_eval)==1)
    x_eval <- seq(min(x), max(x), length.out=x_eval)
  
  pav <- try(peaks_and_valleys(x, x_eval=x_eval, ...))
  if (class(pav)=="try-error") {
    err <- pav
    pav <- list(density = rep(NA, length(x_eval)))
  } else {
    err <- NULL
  }
  
  if (is.null(pav$valleys)) {
    threshold <- NA
  } else {
    threshold <- pav$valleys
  }
  
  h = hist(x, breaks="Scott", plot=FALSE)
  model=list(x = x_eval,
             density = pav$density,
             logspline_fit=pav$logspline_fit,
             h = h,
             x_1st_deriv = pav$x_eval_deriv,
             deriv_1 = pav$derivative,
             x_2nd_deriv = pav$x_eval_deriv2,
             deriv_2 = pav$derivative2, 
             error = err)
  
  attr(threshold, "method") <- 'valleys'
  attr(threshold, "model") <- model
  
  threshold = structure(threshold, class="threshold")
  
  return(threshold)
}
