################################################################################
#' @title threshold_valleys
#' @title Thresholding based on peak and valley analysis.
#'
#' @description A smooth density model is derived for the data with \code{\link[logspline]{logspline}}. Based on the density peaks and valleys are detected and the final threshold is defined as the valley  (if available) with the highest \code{x} value.
#'  
#' @param x vector of (pixel) gray levels to be thresholded
#' @param ... arguemnts passed to peaks_and_valleys 
#' @references  ...
#' @export
threshold_valleys <- function(x, x_eval=101, ...) {
require(logspline)

pav <- peaks_and_valleys(x, x_eval=x_eval, ...)

if (is.null(pav$valleys)) {
  threshold <- NA
} else {
  threshold <- pav$valleys
}

h = hist(x, breaks="Scott", plot=FALSE)
model=list(x = pav$x_eval,
           density = pav$density,
           logspline_fit=pav$logspline_fit,
           h = h,
           x_1st_deriv = pav$x_eval_deriv,
           deriv_1 = pav$derivative,
           x_2nd_deriv = pav$x_eval_deriv2,
           deriv_2 = pav$derivative2)

attr(threshold, "method") <- 'valleys'
attr(threshold, "model") <- model

threshold = structure(threshold, class="threshold")

return(threshold)
}
