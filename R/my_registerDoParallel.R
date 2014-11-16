#' @export
my_registerDoParallel <- function(cl=NULL, 
                                  nCores=NULL) {
  require(foreach)
  require(doParallel)
  
  if (is.null(nCores))
    nCores <- detectCores()
    try(stopCluster(cl))
    cl <- makeCluster(nCores)
    registerDoParallel(cl)
    return(cl)
}