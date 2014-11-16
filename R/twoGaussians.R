#' @export 
twoGaussians <- function(n = c(1000, 5000), m.pos=2, m.neg=-2, 
                         sd.pos=1, sd.neg=1, seed=NULL) {
  if (!is.null(seed))
    set.seed(seed)
  pos <- rnorm(n[1], m.pos, sd.pos)
  neg <- rnorm(n[2], m.neg, sd.neg)
  x <- c(pos, neg)
  y <- rep(c(1, -1), n)
  return(data.frame(y=y, x=x))
}