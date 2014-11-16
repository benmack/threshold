### two-Gaussian test case for histogram thresholding

breaks <- 50
ds <- twoGaussians(c(2000, 10000), m.pos=1, sd.pos=2, seed=123)
x=ds$x
h=hist(ds$x, breaks=breaks, freq=FALSE)

### entropy 
th_Kapur <- threshold_entropy_Kapur(ds$x, breaks=breaks)
plot(th_Kapur, y_range=range(th_Kapur$h))

### cluster
th_Nmm <- threshold_cluster_nmm(ds$x, breaks=breaks)
h <- hist(x, breaks=50, freq=FALSE)
plot(th_Nmm, y_range=range(h$density))

### peak and valley thresholding
th_pav <- threshold_shape_pav(ds$x)
plot(th_pav)

if (is.null(th_pav$tresholds))
  th_rosin <- threshold_shape_rosin(ds$x)

h <- hist(x, breaks=50, freq=FALSE)
lines(th_pav$model$thresholds, th_pav$model$density, lwd=2)
abline(v=th_pav$model$valleys, lwd=2)
abline(v=th_rosin$threshold, lwd=2)
