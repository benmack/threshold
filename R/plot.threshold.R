#' @name plot.threshold 
#' @title ...
#' @description ...
#' @param x ...
#' @param add2hist ...
#' @param ylim ...
#' @param ... ...
#' @method plot threshold
#' @export
plot.threshold <- function(x, add2hist=FALSE, ths_add=NULL, 
                           names_add=NULL, ylim=NULL, xlim=NULL, 
                           right_leg=TRUE, ...) {
  
  clrs.PN <- c(pos='#2166ac', neg='#d6604d')
  
  h <- attr(x, "model")$h
  if (is.null(ylim)) {
    ylim <- range(h$density)
  }
  if (is.null(xlim)) {
    xlim <- range(h$mids)
  }
  
  method <- attr(x, 'method')
  if (any(c('rosin', 'valleys', 'nmm', 'entropy')  == method)) {
    
    if (!add2hist) {
      plot(attr(x, 'model')$h, freq=FALSE, 
           main=paste0("Method: ", method), xlim=xlim, ylim=ylim, ...)
    }
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
      
            if (!is.null(distPos$empi$mean))
              lines(model$x, dnorm(model$x, 
                                   mean=distPos$empi$mean, 
                                   sd=distPos$empi$sd) * 
                                   model$nmm$parameters$pro[model$comp_pos],
                    lwd=2, lty=2, col=clrs.PN['pos'])
    }
  }
  
  if (!is.null(ths_add)) {
    
    if (is.null(ylim)) {
      ylim <- range(h$density)
    }
    
    if ( class(ths_add)!='list' ) { #  | class(ths_add)!='threshold_list' 
      stop ("ths must be a list.") 
    } else {
      if (!is.null(names_add) & is.null(names(ths_add))) {
        names(ths_add) <- names_add
      } else if (is.null(names_add) & is.null(names(ths_add))) {
        names(ths_add) <- paste0("th_added_", 1:length(ths_add))
      }
      x <- c(list(x), ths_add)
      names(x)[1] <- attr(x[[1]], "method")
    }
    
    pos.y <- ylim[2]-(ylim[2]/15*1:length(x))
    
    if (right_leg) {
      pos.x <- rep(max(xlim)-diff(xlim)/10, 
                   length(x))
      pos_text <- 4
    } else {
      pos.x <- rep(min(h$mids)+diff(range(h$mids))/10, 
                   length(x))
      pos_text <- 2
    }
    
    for (i in 1:length(x)) {
      abline(v=x[[i]], lwd=2, lty="solid")
      text(pos.x[i], pos.y[i], names(x)[i], cex=.5, pos=pos_text)
      
      for (j in 1:length(x[[i]]))
        arrows(pos.x[i], pos.y[i], x[[i]][j], pos.y[i], lwd=2, length=.1)
    }
  }
}
