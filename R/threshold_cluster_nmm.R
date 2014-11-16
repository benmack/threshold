################################################################################
#' @title threshold_cluster_nmm
#' @title Thresholding based on normal mixture modelling.
#'
#' @description It is assumed that the data can be descrived by two Gaussians. 
#' The parameters of the mixture components are estimated with \code{\link[mclust]{Mclust}}. 
#'  
#' @param x vector of (pixel) gray levels to be thresholded
#' @parm range_constrain constrain the range from which to select the threshold  
#' @param ... arguemnts passed to mclust 
#' @references  ...
#' @export
threshold_cluster_nmm <- function(x, ...) {
require(mclust)

fit <- Mclust(x, G=2)
# h=hist(x)
# plot(fit, what='density')

threshold=mean(c(max(x[fit$classification==1]), 
                 min(x[fit$classification==2])))

rtrn <- structure(list(threshold = threshold, 
                         method="cluster_nmm", 
                      nmm=fit),
                  class="threshold")

return(rtrn)
}
