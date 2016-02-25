#' @name threshold_rosin
#' @title ...
#' @description ...
#' @param x ...
#' @param x_eval ...
#' @export
threshold_rosin <- function (x, x_eval="FD") {
  # translated from 
  # http://clickdamage.com/sourcecode/code/rosinThreshold.m
  debug=FALSE
  
  picknonempty = NULL # ???
  
  if (class(x) != "histogram") {
    x <- hist(x, breaks=x_eval, plot=F)
  }
  x_eval <- x$mids
  
  if (is.null(picknonempty))
    picknonempty = 0
  
  dens <- x$density
  p1 <- p2 <- c(NA, NA)
  found <- -999
  
  mmax2 <- max(dens)
  mpos <- x_eval[which.max(dens)]
  
  p1 = c(mpos, mmax2)
  
  p2 = c(max(x_eval), y=dens[length(dens)])
  
  DD = sqrt((p2[1]-p1[1])^2 + (p2[2]-p1[2])^2)
  
  from = which(x_eval==p1[1])
  to = which(x_eval==p2[1])
  ds <- x_eval*NA
  if (DD != 0) {
    best = -999
    found = -999
    for (i in from:to) {
      p0 = c(x_eval[i], dens[i])
      d = abs((p2[1]-p1[1])*(p1[2]-p0[2]) - 
                (p1[1]-p0[1])*(p2[2]-p1[2]));
      d = d / DD;
      ds[i] <- d
      if (debug)
        cat(sprintf("i / d: %d / %.4f", i, d))
      if (is.logical(d > best) & !is.na(d)) {
        if (d > best) { # ...
          best=d
          found = x_eval[i]
          if (debug)
            cat(" => found: ", x_eval[i])
        }
      }
      if (debug)
        cat("\n")
    }
  }
  
  if (found==-999) {
    found = max(x);
  }
  
  threshold = found
  
  if (is.null(threshold))
    threshold <- NA
  
  # h = hist(x, breaks="Scott", plot=FALSE)
  model=list(x = x_eval,
             density = dens,
             h = x,
             points=data.frame(x=c(p1[1], p2[1]),
                               y=c(p1[2], p2[2])))
  
  attr(threshold, "method") <- 'rosin'
  attr(threshold, "model") <- model
  
  threshold <- structure(threshold, class="threshold")
  
  return(threshold)
}