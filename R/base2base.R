#TODO: 
# 
#	5 whatever the latest CRAN rule is about 'require' vs gmp::gmp::is.bigz and so on
base2base <- function(x,frombase=10, tobase=2) {
allChar <- c(0:9,letters)# validate by checking allChar[1:frombase]
theans <-list()
# presumably numeric x is decimal, but whatever
# check for gmp::is.bigz() and convert that to char as well
# easiest to make a non-list x into a length-1 list
x <- list(x)
for (jl in 1:length(x) ) {
	if (is.numeric(x[[jl]]) || gmp::is.bigz(x[[jl]])){
		x[[jl]] <- as.character(x[[jl]])
		}
	if (length(x[[jl]]) > 1) {
		xchar <- paste0(x[[jl]],sep='',collapse='')
		xtmp <- x[[jl]] #check this
		} else {	
			xchar <- x[[jl]]
			xtmp <- unlist(strsplit(x[[jl]],''))
			}
	xtmp <- tolower(xtmp)
	if (frombase == 16) {
		xtmp <- gsub('^0x','',xtmp) # just in case
	}
	if(!(2<=frombase && 36 >= frombase && 2<=tobase && 36>= tobase)){
		stop('Both bases must be in range 2:36')
		}
	if(frombase < 36 && any(xtmp%in%allChar[(frombase+1):36] ) ) {
		warning( c('At least one element of', xtmp,' not allowed in selected frombase') )
		next # i.e. skip this item
		}
	# gmp  in here for really large integers. First check whether
	# incoming is > max integer
	# use length(xtmp)  to determine max power invoked for xtmp in base(frombase)
	maxpow <-frombase^length(xtmp) 
	if(maxpow < 2147483647) {   #2^31-1)
		foo <- strtoi(xchar,frombase)
		# powM is numeric even when foo is bigz
		powM <- floor(log(foo,tobase)) # how many places needed
	#TODO: change mout here and below to numeric? 
		mout <- rep('0',powM+1) 
		#remember leftmost element of vector is index 1
		for (jp in powM:1) {
			divtmp <- tobase^jp
			mout[jp+1] <- foo%/%divtmp 
			foo <- foo%%divtmp
			}
		# whatever's left is final digit
		mout[1] <- foo
		} else {
		#goto gmp and do things the hard way,loop over x terms
			foo <- gmp::as.bigz(0)  # length(x) or len+1 ? 
			for (jg in length(xtmp):1){
		#convert a:z into a number
		# get the index order right!
				xrev <- rev(xtmp)
				tmpval <- which(allChar == xrev[jg]) - 1 
				if (!length(tmpval))  stop("Can't convert. Check for illegal symbol in input x")
				foo <- foo + tmpval * (gmp::as.bigz(frombase)^(jg-1) )
			    }
			# powM is numeric even when foo is bigz
			powM <- floor(log(foo,tobase)) # how many places will I need
			mout <- rep('0',powM+1) 
			#remember leftmost element of vector is index 1!
			tobase <- gmp::as.bigz(tobase)
	#maybe do mout <- as.numeric here? 
			for (jp in powM:1) {
				divtmp <- tobase^jp
				mout[jp+1] <- as.character(foo%/%divtmp )
				foo <- foo%%divtmp
				}
			# whatever's left is final digit
			mout[1] <- as.character(foo)
			}

	theans[[jl]] <- allChar[rev(as.numeric(mout)+1)]
} #end of for jl
return(invisible(theans))
}
