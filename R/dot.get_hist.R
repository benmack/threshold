.get_hist <- function(x, ...) {
  ### generate the histogram
  h <- hist(x, ...)
  g <- h$mids 
  h <- h$counts
  h[h==0] <- 1 # add at least one count for numerical stability
  
  return(list(g=g, h=h, pmf=h/sum(h), n=length(h)))
}