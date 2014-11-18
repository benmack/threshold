#' @method plot threshold
#' @export 
plot.threshold_deprecated <- 
  function(x, add=FALSE, y_range=NULL, ...) {
    
    plot_shape_pav <- function(x, add=FALSE, ...) {
      ylim=range(c(x$model$density, x$model$derivative))
      if (!add) {
      plot(x$model$thresholds, x$model$density, type="l", ylim=ylim, 
           xlab='threshold', ylab='density/derivative', ...)
      } else {
        lines(x$model$thresholds, x$model$density)
      }
      lines(x$model$thresholds_deriv, x$model$derivative, ...)
      points(x$model$peaks, x$model$peaks*0, pch=2)
      points(x$model$valleys, x$model$valleys*0, pch=6)
      abline(h=0)
    }
    
    
    if (x$method=="entropy_Kapur") {
      threshold <- x$g
      criteria <- x$optimize
      if (is.null(y_range)){
        plot(threshold, criteria, type='l', ...)
      } else{
        criteria_scaled <- approx(range(criteria, na.rm=TRUE), 
                                  y_range, xout=criteria)$y
        lines(threshold, criteria_scaled, ...)
      }
      abline(v=threshold[which(max(criteria, na.rm=TRUE)==criteria)])
    } else if (x$method=="cluster_nmm") {
      thresholds <- seq(min(x$nmm$z), max(x$nmm$z), length.out=100)
      dens <- predict(x$nmm, thresholds)
      if (is.null(y_range)) {
        h <- hist(x$nmm$data, breaks='Scott', freq=FALSE)
        y_range <- h$density
      }
      pred <- predict(x$nmm, thresholds)
      plot(x$nmm, what="density")
      hist(x$nmm$data, breaks="Scott", freq=FALSE, add=TRUE)
      abline( v=x$threshold )
    } else if (x$method=="shape_pav"){
      plot_shape_pav(x, add=add, ...)
    }
  }