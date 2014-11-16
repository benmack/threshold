### convex hull with chull
h <- .get_hist(x, breaks="FD")
ch <- chull(h$g, h$pmf)
plot(h$g, h$pmf, type="h")
lines(h$g[sort(ch)], h$pmf[sort(ch)])
### convex hull with alphahull
require(alphahull)
alphashape <- ashape(h$g*100, h$h, alpha = .9)
