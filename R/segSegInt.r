# simple 2- segment intersection test.  See material at 
# https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r


# This is taken from the derivations at 
# http://paulbourke.net/geometry/pointlineplane/
#bourke notes "The equations apply to lines, if the intersection of line segments is required then it is only necessary to test if ua and ub lie between 0 and 1. Whichever one lies within that range then the corresponding line segment contains the intersection point. If both lie within the range of 0 to 1 then the intersection point is within both line segments."

# seg1 must be either 2x2 or 4x2; if the latter seg2 is ignored
# Column 1 is X Column2 is Y . If 4x2, upper and lower are the two segments
segSegInt <- function(seg1, seg2=NULL, plotit=FALSE, probParallel = 1e-10,  ...){
#TODO
#	2) 
# 1) need more input validation...
if (nrow(seg1) == 4) {
	seg2 <- seg1[3:4,] 
	seg1 <- seg1[1:2,]
}
x <- c(seg1[,1], seg2[,1])
y <- c(seg1[,2], seg2[,2])
# some reused items - more memory, fewer calcs
denom <- ((y[4] - y[3])*(x[2] - x[1]) - (x[4] - x[3])*(y[2] - y[1]))
denom[abs(denom) < probParallel] <- NA # parallel lines, most likely
ua <- ((x[4] - x[3])*(y[1] - y[3]) - (y[4] - y[3])*(x[1] - x[3])) / denom
ub <- ((x[2] - x[1])*(y[1] - y[3]) - (y[2] - y[1])*(x[1] - x[3])) / denom

xout <- x[1] + ua * (x[2] - x[1])
yout <- y[1] + ua * (y[2] - y[1])
# this will be NA if anything above (like denom) is NA
inside <- (ua >= 0) && (ua <= 1) && (ub >= 0) && (ub <= 1)
# gotta be a better way to test for NA and for TRUE
if (is.na(inside)) inside <-FALSE
#browser()
if (plotit) {
	plot(seg1[,1],seg1[,2], t='line', lty = 2, xlim = c(-1.1,1.1)*max(abs(c(seg1[,1],seg2[,1]))), ylim =c(-1.1,1.1)*max(abs(c(seg1[,2],seg2[,2]))) ,asp=TRUE, xlab = '', ylab = '')
# passing '...' to lines() call only. Not particularly helpful. Oh, well. 
	lines(seg2[,1],seg2[,2],lty = 3, ...)
	points(xout,yout, pch ='*',col='red') #won't plot if NA, i.e. no intersection
}
# don't use ifelse because incest between size of "test" and the return values
  return(invisible( if(inside) c(xout,yout) else c(NA,NA)) )
}


# This function uses segSegInt to determine if & where two polygons intersect.
# poly1,poly2 must be Nx2 arrays & I'll check if lastrow == firstrow
#TODO
#	2) DUN allow 2xN and transpose.  (2x2 aren't polygons)
polyInt <- function(poly1,poly2, stopAtFirst = FALSE, plotit = FALSE, roundPrecision = 10, ...) {
if(ncol(poly1)>2) {
	#transpose it
	poly1 <- t(poly1)
}
if(ncol(poly2)>2) {
	#transpose it
	poly2 <- t(poly2)
}
nrow1 <-nrow(poly1)
nrow2 <- nrow(poly2)
#check if lastrow == firstrow . If not, add it so all segments are included
if (!(poly1[1,1] == poly1[nrow1,1]) || !(poly1[1,2] == poly1[nrow1,2]) ){
	poly1 <- rbind(poly1,poly1[1,])
	nrow1 <- nrow1+1
}
if (!(poly2[1,1] == poly2[nrow2,1]) || !(poly2[1,2] == poly2[nrow2,2]) ){
	poly2 <- rbind(poly2,poly2[1,])
	nrow2 <- nrow2 +1 
}
allint <- NULL # start just saving intersection coords

breakout <- FALSE
for (jone in 1:(nrow1-1) ) {
	for(jtwo in 1:(nrow2-1 ) ) {
		thisint <- segSegInt(poly1[c(jone,jone+1),], poly2[c(jtwo,jtwo+1),])
		if (!is.na(prod(thisint)) ) {
			allint <- rbind(allint,thisint) 
			if(stopAtFirst) breakout <- TRUE
		}
		if( breakout) break
	}
	if(breakout) break
}
#OK, now let's check for dupes, since vertices will get multiple counts
if (length(allint)) {
	allint <- round(allint, roundPrecision)
	allint <- unique(allint) 
}
if(plotit) {
# plot both polygons and then the intersection points, if any
	plot(poly1[,1],poly1[,2], t='l', lty = 2, xlim = c(-1.1,1.1)*max(abs(c(poly1[,1],poly2[,1]))), ylim =c(-1.1,1.1)*max(abs(c(poly1[,2],poly2[,2]))) ,asp=TRUE, xlab = '', ylab = '')
	lines(poly2[,1],poly2[,2],lty = 3)
	points(allint[,1],allint[,2], pch = '*', col='red')
}
return(invisible(allint))
}











