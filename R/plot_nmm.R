plot_nmm <- function(x, plotHist=TRUE, add=FALSE, ...) {
  #' @param nmm must be of an object of class "densityMclust" "Mclust"
  
  clrs.PN <- c(pos='#2166ac', neg='#d6604d')
  
  if (plotHist) {
    hist(x$nmm$data, breaks="Scott", freq=FALSE, add=add, 
         border="darkgrey", main="", ...)
    lines(x$forPlot$thresholds, rowSums(x$forPlot$pred.scl), lwd=3)
  } else if (!plotHist & !add) {
    plot(x$forPlot$thresholds, rowSums(x$forPlot$pred.scl), lwd=3)
  } else {
    lines(x$forPlot$thresholds, rowSums(x$forPlot$pred.scl), lwd=3)
  }
  polygon(x$forPlot$xs, x$forPlot$ys, col="gray")
  for (i in 1:x$nmm$G) {
    clr = ifelse(i==x$forPlot$comp.pos, clrs.PN['pos'], clrs.PN['neg'])
    idx.sd <- ifelse(length(x$nmm$parameters$variance$sigmasq)==1, 1, i)
    lines(x$forPlot$thresholds, dnorm(x$forPlot$thresholds, 
                       mean=x$nmm$parameters$mean[i], 
                       sd=sqrt(x$nmm$parameters$variance$sigmasq[idx.sd]) )*
            x$nmm$parameters$pro[i], col=clr, lwd=2 )
  }
  # lines(c(x$threshold, x$threshold), c(0, x$forPlot$densAtTh), lwd=2)
  
}
