#  a generic cumulative function function
# example:
 # foo <- 'abcdefcghicklmno'
 # grepfoo <-function(x,y) grep(y,x)
 # cumfun(unlist(strsplit(foo,'')),grepfoo,'c')
 #
 # compare also, from cumstat package, implementation of cumkurtosis:
#   sapply(seq_along(x), function(k, z) kurtosis(z[1:k]), z = x)

#
#TODO
# 
cumfun <- function(frist, FUN = sum, ...){
xlen = length(frist)
ycumresult <- vector('list',length=xlen ) #in case FUN() returns lots of crapola
inargs <- list(...)
# make extra args same length as frist
# and skip if there's no such args
if (length(inargs)) {
	innames <- names(inargs)
	for (jarg in 1:length(inargs)) {
		if (length(inargs[[jarg]]) > xlen ) {
			warning( c('length of ',innames[jarg],' is > length(frist); will truncate') ) 
			inargs[[jarg]] <- inargs[[jarg]][1:xlen]
			} else {
	# some called funcs may recycle, but many won't so do this
				if( (length(inargs[[jarg]]) > 1) && (length(inargs[[jarg]]) < xlen) ){
					warning( c('length of ',innames[jarg],' is < length(frist); will recycle') )
					inargs[[jarg]] <- rep(inargs[[jarg]],length = xlen)
					}
				}
		}

	truncargs <-list()
	for (jy in 1: xlen){
		for (jarg in 1:length(inargs)){
			truncargs[[jarg]] <- inargs[[jarg]][1:min(jy,length(inargs[[jarg]])) ]
			}
	#make sure NOT to "name" frist or FUN will probably reject or "place" it wrong.  
		ycumresult[[jy]] <- do.call(FUN, c(list(frist[1:jy]), truncargs) )
		}
	} else {
		for (jy in 1:xlen) {
			ycumresult[[jy]] <- FUN(frist[1:jy])
		}
	}

return(invisible(ycumresult))
}
 

