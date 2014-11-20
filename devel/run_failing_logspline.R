x <- rep(0, 100)
th <- find_threshold(x, method="valleys")
th; plot(th)

rm(th); th <- find_threshold(x, method="nmm")
th; plot(th)

rm(th); th <- find_threshold(x, method="entropy")
th; plot(th)

rm(th); th <- find_threshold(x, method="rosin")
th; plot(th)
