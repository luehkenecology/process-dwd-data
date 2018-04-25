# distance function ----------------------------------------------------------------------
minDist <- function(points, p) which.min(colSums((t(points) - p)^2))