#' @name plot.threshold_list 
#' @title ...
#' @description ...
#' @param x ...
#' @param ths_add ...
#' @param names_add ...
#' @param add2hist ...
#' @param ylim ...
#' @param ... ...
#' @method plot threshold_list
#' @export
plot.threshold_list <- function(x, ths_add=NULL, names_add=NULL, add2hist=FALSE, ylim=NULL, xlim=NULL, ...) {
  
  clrs.PN <- c(pos='#2166ac', neg='#d6604d')
  
  if (!add2hist)
    plot(attr(x[[1]], 'model')$h, freq=FALSE, 
         border="darkgrey", 
         main=paste0("Threshold comparison"))
  lines(attr(x[[1]], 'model')$x, attr(x[[1]], 'model')$density, 
        lwd=2)
  
  
  if (!is.null(ths_add))
    if ( class(ths_add)!='list' ) { #  | class(ths_add)!='threshold_list' 
      stop ("ths must be a list.") 
    } else {
      if (!is.null(names_add) & is.null(names(ths_add))) {
        names(ths_add) <- names_add
      } else if (is.null(names_add) & is.null(names(ths_add))) {
        names(ths_add) <- paste0("th_added_", 1:length(ths_add))
      }
      x <- c(x, ths_add)
    }
  
  h <- attr(x[[1]], "model")$h
  if (is.null(ylim)) {
    ylim <- range(h$density)
  }
  if (is.null(ylim)) {
    xlim <- range(h$mids)
  }
  
  pos.y <- ylim[2]-(ylim[2]/15*1:length(x))
  
  pos.x <- rep(min(xlim)+diff(range(xlim))/10, 
               length(x))
  
  for (i in 1:length(x)) {
    abline(v=x[[i]], lwd=2, lty="solid")
    text(pos.x[i], pos.y[i], names(x)[i], cex=.75, pos=2)
    
    for (j in 1:length(x[[i]]))
      arrows(pos.x[i], pos.y[i], x[[i]][j], pos.y[i], lwd=2, length=.1)
  }
  
}
