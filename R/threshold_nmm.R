################################################################################
#' @name threshold_nmm
#' @title Thresholding based on normal mixture modelling.
#'
#' @description It is assumed that the data can be descrived by two Gaussians. 
#' The parameters of the mixture components are estimated with \code{\link[mclust]{Mclust}}. 
#'  
#' @param x vector of (pixel) gray levels to be thresholded, or an object of class densityMclust Mclust
#' @param x_eval ...    
#' @param x_pos A sample of the positive class. If given the overlap between the theoretical positive distribution and the empirical distribution is calculated.
#' @param ... arguemnts passed to mclust 
#' @references  ...
#' @importFrom mclust densityMclust
#' @importFrom mclust predict.densityMclust
#' @importFrom caret confusionMatrix
#' @export
threshold_nmm <- function(x, x_eval=101, x_pos=NULL, ...) {
  
  calcNmm <- TRUE
  if (class(x)[1]=="densityMclust") {
    nmm <- x
    x <- nmm$data
    calcNmm <- FALSE
  }
  
  if (length(x_eval)==1) {
    x_eval <- seq(min(x), max(x), length.out=x_eval)
  }
  ### --------------------------------------------------------------------------
  ### error/overlap between the (assumed) positive and all other components
  scaled_density_PN <- function(nmm, newdata, comp_pos, returnWhat="PN") {
    idx <- which.max(nmm$parameters$mean)
    d.mat <- predict(nmm, newdata, what="cdens")
    pro.mat <- matrix(rep(nmm$parameters$pro), length(newdata), nmm$G, byrow=TRUE)
    scaled.d <- d.mat*pro.mat
    scaled.d <- data.frame(neg=rowSums(scaled.d[, -comp_pos, drop=FALSE]),
                           pos=rowSums(scaled.d[, comp_pos, drop=FALSE]))
    if (returnWhat=="P") {
      return(scaled.d[, "pos"])
    } else if (returnWhat=="N") {
      return(scaled.d[, "neg"])
    } else {
      return(scaled.d)
    }
  }
  
  
  ### returns the minimum densitiy 
  min_f1f2 <- function(newdata, model) {
    idx <- which.max(nmm$parameters$mean)
    d.mat <- predict(model, newdata, what="cdens")
    pro.mat <- matrix(rep(model$parameters$pro), length(newdata), model$G, byrow=TRUE)
    scaled.d <- d.mat*pro.mat
    rtrn <- pmin(rowSums(scaled.d[, -idx, drop=FALSE]), scaled.d[, idx])
    return(rtrn)
  }
  getOverlap <- function(nmm, theo, rng=NULL) {
    if (is.null(rng))
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
  
  if (calcNmm)
    nmm <- densityMclust(x, ...)
  
  rng <- nmm$range
  ys <- min_f1f2(x_eval, nmm)
  xs <- c(x_eval, x_eval[1])
  ys <- c(ys, ys[1])
  pred <- predict(nmm, x_eval, what="cdens")
  pred.scl <- pred*NA
  for (i in 1:nmm$G) {
    pred.scl[,i] <- pred[,i]*nmm$parameters$pro[i]
  }
  ### assuming the component with the highest mean belongs 
  # to the positive class, which is the best threshold
  comp.pos <- which.max(nmm$parameters$mean)
  prior.pos <- nmm$parameters$pro[comp.pos]
  # we need to exclude numerically non meaningful behaviour at the tails
  ans <- pred.scl[, comp.pos]>=rowSums(pred.scl[, -comp.pos, drop=FALSE])  
  if(all(!ans))
    warning("Could not identify a threshold.\nCould not find any threshold for which the posterior of the positive class ( p(+|x) ) is larger than 0.5.")
  
  ans[x_eval<min(nmm$parameters$mean)] <- FALSE
  if (length(which(ans))==0) {
    threshold = NA
  } else {
    threshold <- mean(x_eval[which(ans)[1]+c(-1,0)])
  }
  # ans <- x_eval[ans]
  # threshold <- min(ans[ans>min(nmm$parameters$mean)])
  # idx_th <- which(x_eval == threshold)
  
  rng <- nmm$range
  # cat(rng)
  err <- integrate(min_f1f2, lower=rng[1], upper=rng[2], model=nmm, subdivisions=100L)
  distPos <- list()
  if (!is.null(x_pos)) {
    x_pos_noOut <- x_pos[!(x_pos %in% boxplot.stats(x_pos)$out)]
    theo_std_pos <- sqrt(ifelse(nmm$parameters$variance$modelName=="E",
                           nmm$parameters$variance$sigmasq[1], 
                           nmm$parameters$variance$sigmasq[comp.pos]))
    distPos$theo = list(mean=nmm$parameters$mean[comp.pos], 
                sd=theo_std_pos)
    distPos$empi = list(mean=mean(x_pos_noOut), 
                sd=sd(x_pos_noOut))
    distPos$overlap <- getOverlap_PP(theo=distPos$theo, empi=distPos$empi)
  } else {
    distPos$overlap <- distPos$theo <- distPos$empi <- NA
  }
  #   # ALL. Should be ~1  
  #   integrate(predict, lower=rng[1], upper=rng[2],
  #             object=nmm, comp_pos=comp.pos, returnWhat="P", 
  #             subdivisions=100L)
  #   # P(y+)  
  #   integrate(scaled_density_PN, lower=rng[1], upper=rng[2],
  #             nmm=nmm, comp_pos=comp.pos, returnWhat="P", 
  #             subdivisions=100L)
  if (!is.na(threshold)) {
    TP <- integrate(scaled_density_PN, lower=threshold, upper=rng[2],
                    nmm=nmm, comp_pos=comp.pos, returnWhat="P",
                    subdivisions=100L)$value
    FN <- integrate(scaled_density_PN, lower=rng[1], upper=threshold,
                    nmm=nmm, comp_pos=comp.pos, returnWhat="P",
                    subdivisions=100L)$value
    FP <- integrate(scaled_density_PN, lower=threshold, upper=rng[2],
                    nmm=nmm, comp_pos=comp.pos, returnWhat="N",
                    subdivisions=100L)$value
    TN <- integrate(scaled_density_PN, lower=rng[1], upper=threshold,
                    nmm=nmm, comp_pos=comp.pos, returnWhat="N",
                    subdivisions=100L)$value
    densAtTh <- predict(nmm, threshold)
  } else {
    TP <- FN <- FP <- TN <- NA
    densAtTh <- NA
  }
  # TP+FN+FP+TN
  
  confmat <- matrix(signif(c(TP, FN, FP, TN), 3), 2, 2)
  rownames(confmat) <- c("P", "N")
  colnames(confmat) <- c("P", "N")
  if (!is.na(threshold)) {
    acc = confusionMatrix(as.table(round(confmat*1000, 0)))
  } else {
    acc <- NA
  }
#   confmat <- matrix(signif(c(TP/(TP+FN), FN/(TP+FN), 
#                              FP/(FP+TN), TN/(FP+TN)), 3), 2, 2)
#   rownames(confmat) <- c("refP", "refN")
#   colnames(confmat) <- c("clP", "clN")

#   rtrn <- structure(list(threshold = threshold, 
#                          method="cluster_nmm", 
#                          nmm=nmm, 
#                          error_rate=err,
#                          confmat=confmat,
#                          forPlot=list(rng=rng,
#                                       xs=xs,
#                                       ys=ys,
#                                       x_eval=x_eval,
#                                       pred.scl=pred.scl,
#                                       comp.pos=comp.pos, 
#                                       densAtTh=densAtTh)),
#                     class="threshold")
  
  
  
  ### do not store all the data !
  nmm$data <- NULL
  
  ###
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
  attr(threshold, "distPos") <- distPos
  attr(threshold, "confmat") <- confmat
  attr(threshold, "acc") <- acc
  attr(threshold, "error_rate") <- err$value
  
  threshold <- structure(threshold, class="threshold")
  
  return(threshold)
}
