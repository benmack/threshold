threshold_shape_rosin <- function (x, thresholds=100) {
  # translated from 
  # http://clickdamage.com/sourcecode/code/rosinThreshold.m
  
  picknonempty = NULL # ???
  
  if (length(thresholds)==1)
    thresholds <- seq(min(x), max(x), length.out=thresholds)
  
  if (is.null(picknonempty))
    picknonempty = 0
  
  fit <- logspline(x)
  dens <- dlogspline(thresholds, fit)
  
  mmax2 <- max(dens)
  mpos <- thresholds[which.max(dens)]
  
  p1 = c(mpos, mmax2)
  
  p2 = c(max(x), y=0)
  
  DD = sqrt((p2[1]-p1[1])^2 + (p2[2]-p1[2])^2);
  
  
  from = which(thresholds==p1[1])
  to = which(thresholds==p2[1])
  
  if (DD != 0) {
    best = -999
    found = -999
    for (i in from:to) {
      p0 = c(thresholds[i], dens[i])
      d = abs((p2[1]-p1[1])*(p1[2]-p0[2]) - (p1[1]-p0[1])*(p2[2]-p1[2]));
      d = d / DD;
      
      if ((d > best)) { #  & ((imhist(i)>0) | (picknonempty==0)) ???
        best=d
        found = thresholds[i]
      }
    }
  }
  
  if (found == -999) {
    found = max(x);
  }
  
rtrn <- structure(list(threshold = found, 
                         method="shape_rosin", 
                      model=list(thresholds = thresholds, 
                                 density = dens, 
                                 logspline_fit=fit,
                                 p1=p1, p2=p2),
                  class="threshold"))
}