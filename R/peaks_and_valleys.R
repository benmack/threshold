#' @export
peaks_and_valleys <- function(x, x_eval=101) {
  require(logspline)
  
  if (length(x_eval)==1)
    x_eval <- seq(min(x), max(x), length.out=x_eval)
  
  fit <- logspline(x)
  dens <- dlogspline(x_eval, fit)
  
  x_eval_deri <- x_eval[-1]-(diff(x_eval)/2)
  deri <- diff(dens)/diff(x_eval)
  
  x_eval_deri2 <- x_eval_deri[-1]-(diff(x_eval_deri)/2)
  deri2 <- diff(deri)/diff(x_eval_deri)
  
  x_eval_deri3 <- x_eval_deri2[-1]-(diff(x_eval_deri2)/2)
  deri3 <- diff(deri2)/diff(x_eval_deri2)

  x_eval_deri4 <- x_eval_deri3[-1]-(diff(x_eval_deri3)/2)
  deri4 <- diff(deri3)/diff(x_eval_deri3)
 
  peaks <- valleys <- valleys_density <- c()
  for (i in 2:length(x_eval_deri)) {
    if (deri[i-1]>0 & deri[i]<0)
      peaks <- c( peaks, x_eval[i]  )
    if (deri[i-1]<0 & deri[i]>0)
      valleys <- c( valleys, x_eval[i]  )
      # valleys_density <- c( valleys_density, mean(dens[i:i+1])  )
  }
  
  # points <- 
  
  return(list(peaks=peaks, 
              valleys=valleys, 
              x_eval = x_eval, 
              density = dens, 
              x_eval_deriv = x_eval_deri, 
              derivative = deri,              
              x_eval_deriv2 = x_eval_deri2, 
              derivative2 = deri2,              
#               x_eval_deriv3 = x_eval_deri3, 
#               derivative3 = deri3,              
#               x_eval_deriv4 = x_eval_deri4, 
#               derivative4 = deri4,              
              logspline_fit = fit))
}