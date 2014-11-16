#' @export
peaks_and_valleys <- function(x, thresholds=100) {
  require(logspline)
  
  
  if (length(thresholds)==1)
    thresholds <- seq(min(x), max(x), length.out=thresholds)
  
  fit <- logspline(x)
  dens <- dlogspline(thresholds, fit)
  
  thresholds_deri <- thresholds[-1]-(diff(thresholds)/2)
  deri <- diff(dens)/diff(thresholds)
  
  thresholds_deri2 <- thresholds_deri[-1]-(diff(thresholds_deri)/2)
  deri2 <- diff(deri)/diff(thresholds_deri)
  
  thresholds_deri3 <- thresholds_deri2[-1]-(diff(thresholds_deri2)/2)
  deri3 <- diff(deri2)/diff(thresholds_deri2)

  thresholds_deri4 <- thresholds_deri3[-1]-(diff(thresholds_deri3)/2)
  deri4 <- diff(deri3)/diff(thresholds_deri3)
 
  peaks <- valleys <- c()
  for (i in 2:length(thresholds_deri)) {
    if (deri[i-1]>0 & deri[i]<0)
      peaks <- c( peaks, thresholds[i]  )
    if (deri[i-1]<0 & deri[i]>0)
      valleys <- c( valleys, thresholds[i]  )
  }
  return(list(peaks=peaks, 
              valleys=valleys, 
              thresholds = thresholds, 
              density = dens, 
              thresholds_deriv = thresholds_deri, 
              derivative = deri,              
              thresholds_deriv2 = thresholds_deri2, 
              derivative2 = deri2,              
              thresholds_deriv3 = thresholds_deri3, 
              derivative3 = deri3,              
              thresholds_deriv4 = thresholds_deri4, 
              derivative4 = deri4,              
              logspline_fit = fit))
}