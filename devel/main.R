require(oneClass)
# source("http://bioconductor.org/biocLite.R")
# biocLite("CRImage")
require(CRImage)
source("functions/get_hist.R")

data(bananas)
tr.x <- bananas$tr[,-1]
tr.y <- bananas$tr[,1]
te.x <- bananas$x[]
te.y <- bananas$y[]
u <- bananas$x

fit <- trainOcc(tr.x, tr.y)
z.img <- predict(fit, u)
z <- z.img[]
e <- evaluateOcc(fit, te.x, te.y, 1)

### Entropy-based histogram thresholding
source("functions/threshold_entropy_Kapur.R")
th.entropy_Kapur <- threshold_entropy_Kapur(x, plot=FALSE, breaks="FD")

### visualize
th <- th.entropy_Kapur
h <- hist(fit, z)
lines(attr(th, "thresholds"), 
      approx(range(attr(th, "optimize"), na.rm=TRUE), h$ylim, attr(th, "optimize"))$y )
lines(e@t, approx(range(e@kappa, na.rm=TRUE), h$ylim, e@kappa)$y, lwd=2 )
