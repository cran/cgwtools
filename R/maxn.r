#
#TODO

maxn <-function(x,nth=1){
foo <- rev(sort(unique(as.vector(x))))
if (nth > length(foo)) {
	warning(" 'nth' greater than number of unique values, will truncate ")
	nth <- length(foo)
}
return(invisible(foo[nth]))
}


minn <-function(x,nth = 1){
 themin <- -maxn(-x,nth)
 return(invisible(themin))
}


#  emulate  which(x, arr.ind = FALSE, useNames = TRUE)

which.maxn <-function(x, nth = 1, arr.ind = FALSE, useNames = TRUE) {
themax <- maxn(x,nth)
foo <- which(x == themax, arr.ind, useNames)
return(invisible(foo))
}

which.minn <-function(x, nth = 1, arr.ind = FALSE, useNames = TRUE) {
themin <- minn(x,nth)
foo <- which(x == themin, arr.ind, useNames)
return(invisible(foo))
}