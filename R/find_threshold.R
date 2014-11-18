#' @import logspline
#' @export
find_threshold <- function(x, method='rosin') {
  require(logspline)
  
  if (method=='rosin') {
    th <- threshold_shape_rosin(x, thresholds=100)
    
  }
  return(th)
}

#' @method plot threshold
#' @export
plot.threshold <- function(x) {
  
  method <- attr(x, 'method')
  if (method == 'rosin') {
    pnts <- attr(x, 'model')$points
    plot(attr(x, 'model')$h, freq=FALSE, 
         border="darkgrey", 
         main=paste0("Method: ", method))
    lines(attr(x, 'model')$x, attr(x, 'model')$density, 
          lwd=2)
    points(pnts$x, 
           pnts$y, lwd=2)
    # abline(lm(y~x, pnts), lwd=2)
    abline(v=x, lwd=2)
}
  
  
}

#' @method print threshold
#' @export
print.threshold <- function(x) {
  method <- attr(x, 'method')
  attributes(x) <- NULL
  print(paste0("Threshold(s): ", signif(x)))
  print(paste0("Method: ", method))
}