#' @name threeGaussians
#' @title ...
#' @description ...
#' @param n ...
#' @param m.pos ...
#' @param m.neg ...
#' @param sd.pos ...
#' @param sd.neg ...
#' @param seed ...
#' @export 
threeGaussians <- function(n = c(1000, 1000, 4000), m.pos=2, m.neg=c(-10,-4), 
                         sd.pos=1, sd.neg=c(1,2), seed=NULL) {
  if (!is.null(seed))
    set.seed(seed)
  pos <- rnorm(n[1], m.pos, sd.pos)
  neg1 <- rnorm(n[3], m.neg[1], sd.neg[1])
  neg2 <- rnorm(n[2], m.neg[2], sd.neg[2])
  x <- c(pos, c(neg1, neg2))
  y <- rep(c(1, -1, -1), n)
  return(data.frame(y=y, x=x))
}