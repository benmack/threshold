################################################################################
#' @name threshold_nmm
#' @title Thresholding based on normal mixture modelling.
#'
#' @description It is assumed that the data can be descrived by two Gaussians. 
#' The parameters of the mixture components are estimated with \code{\link[mclust]{Mclust}}. 
#'  
#' @param x vector of (pixel) gray levels to be thresholded
#' @param x_eval points which    
#' @param ... arguemnts passed to mclust 
#' @references  ...
#' @importFrom mclust densityMclust
#' @importFrom mclust predict.densityMclust
#' @export
threshold_nmm <- function(x, x_eval=101, ...) {
  
  if (length(x_eval)==1)
    x_eval <- seq(min(x), max(x), length.out=x_eval)
  
  ### --------------------------------------------------------------------------
  ### error/overlap between the (assumed) positive and all other components
  ### returns the minimum densitiy 
  min_f1f2 <- function(newdata, model) {
    idx <- which.max(nmm$parameters$mean)
    d.mat <- predict(model, newdata, what="cdens")
    pro.mat <- matrix(rep(model$parameters$pro), length(newdata), model$G, byrow=TRUE)
    scaled.d <- d.mat*pro.mat
    rtrn <- pmin(rowSums(scaled.d[, -idx, drop=FALSE]), scaled.d[, idx])
    return(rtrn)
  }
  getOverlap <- function(nmm, theo) {
    rng <- range(qnorm(c(.01, .99), theo$mean, theo$sd))
    integrate(min_f1f2, lower=rng[1], upper=rng[2], model=nmm, subdivisions=100L)
  }
  
  ### --------------------------------------------------------------------------
  ### error/overlap between the theoretical distribution and the empirical
  min_f1f2_PP <- function(newdata, theo, empi) {
    pmin(dnorm(newdata, theo$mean, theo$sd), 
         dnorm(newdata, empi$mean, empi$sd))
  }
  getOverlap_PP <- function(theo, empi) {
    rng <- range(c(qnorm(c(.01, .99), theo$mean, theo$sd), 
                   qnorm(c(.01, .99), empi$mean, empi$sd)))
    integrate(min_f1f2_PP, lower=rng[1], upper=rng[2], theo=theo, empi=empi, subdivisions=1000L)
  }
  
  nmm <- densityMclust(x, ...)
  
  
  rng <- nmm$range
  ys <- min_f1f2(x_eval, nmm)
  xs <- c(x_eval, x_eval[1])
  ys <- c(ys, ys[1])
  pred <- predict(nmm, x_eval, what="cdens")
  pred.scl <- pred*NA
  for (i in 1:nmm$G)
    pred.scl[,i] <- pred[,i]*nmm$parameters$pro[i]
  
  ### assuming the component with the highest mean belongs 
  # to the positive class, which is the best threshold
  comp.pos <- which.max(nmm$parameters$mean)
  # we need to exclude numerically non meaningful behaviour at the tails
  ans <- pred.scl[, comp.pos]>rowSums(pred.scl[, -comp.pos, drop=FALSE])
  ans <- x_eval[ans]
  threshold <- min(ans[ans>min(nmm$parameters$mean)])
  idx_th <- which(x_eval == threshold)
  
  densAtTh <- sum(pred.scl[idx_th,])
  
  rtrn <- structure(list(threshold = threshold, 
                         method="cluster_nmm", 
                         nmm=nmm, 
                         forPlot=list(rng=rng,
                                      xs=xs,
                                      ys=ys,
                                      x_eval=x_eval,
                                      pred.scl=pred.scl,
                                      comp.pos=comp.pos, 
                                      densAtTh=densAtTh)),
                    class="threshold")
  
  
  
  ### do not store all the data !
  nmm$data <- NULL
  
  ###
  threshold <- threshold
  if (is.null(threshold))
    threshold <- NA
  
  h = hist(x, breaks="Scott", plot=FALSE)
  model=list(x = x_eval,
             density = rowSums(pred.scl),
             densities_comp = pred.scl,
             density_at_th = densAtTh,
             comp_pos = comp.pos[[1]],
             h = h,
             nmm = nmm)
  
  attr(threshold, "method") <- 'nmm'
  attr(threshold, "model") <- model
  
  threshold <- structure(threshold, class="threshold")
  
  
  
  
  
  return(threshold)
}
