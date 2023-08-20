
#Sept 2013: per anando mahto request: return NONinvisible

# new: figure out if increment !=0 makes any sense for character inputs, seeing
# as rle() supports char vectors.
# let's just say that all non-numeric 'x' are chars and default to base::rle

seqle <- function(x, incr=1L, prec=.Machine$double.eps ^ 0.5){ 

 #   if(!is.numeric(x)) x <- as.numeric(x) 
   if(!is.numeric(x)) {
   	foo <- rle(x)
   	return(foo)
   } 
    n <- length(x)  
    y <- abs(x[-1L] - x[-n] - incr) > prec
    ii <- c(which(y|is.na(y)),n) 
    foo<- list( lengths = diff(c(0L,ii)),  values = x[head(c(0L,ii)+1L,-1L)])
 # The only method for  class 'rle' is "print", so use it here as well.
   class(foo)<-'rle'
   return(foo)
} 


# and if I allow chars into seqle, I have to allow them here as well.  

inverse.seqle<-function(x,incr=1L) {
#error checker stolen from inverse.rle:
if (is.null(le <- x$lengths) || is.null(v <- x$values) ||  length(le) != length(v)) {
	stop("invalid 'seqle' structure")
}
if (!is.numeric(x$values)) {
	theseq <- inverse.seqle(x)
	return(theseq)
}
theseq<-vector()
# be aware that, for floats, this may not be an exact reconstruction.
for(jj in 1: length(le) ) {
	theseq <- c(theseq, seq(from=x$values[jj],by=incr,length=x$lengths[jj]) )
}
return(theseq)
}
