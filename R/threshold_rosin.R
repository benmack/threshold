#' @name threshold_rosin
#' @title ...
#' @description ...
#' @param x ...
#' @param x_eval ...
#' @importFrom logspline logspline
#' @importFrom logspline dlogspline
#' @export
threshold_rosin <- function (x, x_eval=101) {
  # translated from 
  # http://clickdamage.com/sourcecode/code/rosinThreshold.m
  
  picknonempty = NULL # ???
  
  if (length(x_eval)==1)
    x_eval <- seq(min(x), max(x), length.out=x_eval)
  
  if (is.null(picknonempty))
    picknonempty = 0
  
  fit <- try({logspline(x)})
  dens <- try({dlogspline(x_eval, fit)})
  
  check_dens <- try(any(is.infinite(dens)))

  if (class(fit)=="try-error" | check_dens) {
    err <- fit
    dens <- rep(NA, length(x_eval))
    p1 <- p2 <- c(NA, NA)
    found <- -999
  } else {
    err <- NULL
  
  mmax2 <- max(dens)
  mpos <- x_eval[which.max(dens)]
  
  p1 = c(mpos, mmax2)
  
  p2 = c(max(x), y=0)
  
  DD = sqrt((p2[1]-p1[1])^2 + (p2[2]-p1[2])^2);
  
  
  from = which(x_eval==p1[1])
  to = which(x_eval==p2[1])
  
  if (DD != 0) {
    best = -999
    found = -999
    for (i in from:to) {
      p0 = c(x_eval[i], dens[i])
      d = abs((p2[1]-p1[1])*(p1[2]-p0[2]) - 
                (p1[1]-p0[1])*(p2[2]-p1[2]));
      d = d / DD;
      
      if ( is.logical(d > best) )
        if (d > best) { # ...
          best=d
          found = x_eval[i]
        }
    }
  }
  }

  if (found!=-999) {
    found = max(x);
  } else {
    found = NA
  }

  threshold = found

  if (is.null(threshold))
    threshold <- NA
    
  h = hist(x, breaks="Scott", plot=FALSE)
  model=list(x = x_eval,
             density = dens,
             logspline_fit=fit,
             h = h,
             points=data.frame(x=c(p1[1], p2[1]),
                               y=c(p1[2], p2[2])), 
             error = err)

  attr(threshold, "method") <- 'rosin'
  attr(threshold, "model") <- model
  
  threshold <- structure(threshold, class="threshold")
  
  return(threshold)
}