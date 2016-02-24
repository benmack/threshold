#' @name plot.threshold 
#' @title ...
#' @description ...
#' @param x ...
#' @param add2hist ...
#' @param ylim ...
#' @param ... ...
#' @method plot threshold
#' @export
plot.threshold <- function(x, add2hist=FALSE, ylim=NULL, ...) {
  
  clrs.PN <- c(pos='#2166ac', neg='#d6604d')
  
  method <- attr(x, 'method')
  if (any(c('rosin', 'valleys', 'nmm', 'entropy')  == method)) {
    
    if (!add2hist)
      plot(attr(x, 'model')$h, freq=FALSE, 
           main=paste0("Method: ", method))
    abline(v=x, lwd=2)
    
    model <- attr(x, 'model')
    
    if (method=='rosin') {
      points(model$points$x, 
             model$points$y, lwd=2)
    }
    if (method=='entropy') {
      scaled_opt <- approx(range(model$optcrit, na.rm=TRUE), 
                           range(model$h$density), 
                           xout=model$optcrit)$y
      lines(model$x, scaled_opt, lwd=2)
    }
    if (method=='nmm') {
      distPos <- attr(x, 'distPos')
      # NOT AVAILABLE: polygon(x$forPlot$xs, x$forPlot$ys, col="gray")
      for (i in 1:model$nmm$G) {
        clr = ifelse(i==model$comp_pos, clrs.PN['pos'], clrs.PN['neg'])
        idx.sd <- ifelse(length(model$nmm$parameters$variance$sigmasq)==1, 1, i)
        lines(model$x, dnorm(model$x,
                             mean=model$nmm$parameters$mean[i],
                             sd=sqrt(model$nmm$parameters$variance$sigmasq[idx.sd]) )*
                model$nmm$parameters$pro[i], col=clr, lwd=2)
        
      }
#       if (!is.null(distPos$empi$mean))
#         lines(model$x, dnorm(model$x, 
#                              mean=distPos$empi$mean, 
#                              sd=distPos$empi$sd) * 
#                              model$nmm$parameters$pro[model$comp_pos],
#               lwd=2, lty=2, col=clrs.PN['pos'])
    }
  }
}
