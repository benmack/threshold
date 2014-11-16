################################################################################
#' @title threshold_shape_pav
#' @title Thresholding based on peak and valley analysis.
#'
#' @description A smooth density model is derived for the data with \code{\link[logspline]{logspline}}. Based on the density peaks and valleys are detected and the final threshold is defined as the valley  (if available) with the highest \code{x} value.
#'  
#' @param x vector of (pixel) gray levels to be thresholded
#' @param ... arguemnts passed to peaks_and_valleys 
#' @references  ...
#' @export
threshold_shape_pav <- function(x, thresholds=100, ...) {
require(mclust)

pav <- peaks_and_valleys(x, ...)

threshold <- pav$valleys[length(pav$valleys)]

rtrn <- structure(list(threshold = threshold, 
                         method="shape_pav", 
                      model=pav),
                  class="threshold")

return(rtrn)
}
