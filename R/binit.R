binit <- function(samps,roundPrec=NULL){
if ( length(roundPrec)  ){
	roundPrec <-floor(roundPrec)
	samps <- round(samps, roundPrec)
}
binned <- rle(sort(as.vector(samps)))
return(invisible(binned))
}
