# I love jumping to strings - since it converts all inputs to char, can enter mixed-class
# data. FOr reference,this approach is much much slower than findpat
# findpats <-function(datavec, pattern) {
# str <- paste(datavec, collapse = '-')
# pattern <- paste0('\\b', paste(pattern, collapse = '-'), '\\b')
# inds <- c(1, unlist(gregexpr(pattern, str)))
# m <- substring(str, head(inds,-1), tail(inds,-1))
# len <- lengths(strsplit(m, '-'))
# return(invisible( cumsum(c(len[1], len[-1]-1)) ))
# }

# In some cases, can have differnt classes because  `==` coerces one
# to the other when atomic 

findpat <- function(datavec, pattern, roundit = NULL) {
if (length(roundit) && is.numeric(datavec)) {
	roundit = roundit[1] # just to be dead sure. NB round() rounds the second argument
	datavec = round(datavec,roundit)
	pattern = round(pattern, roundit)
	}
  idx <- which(datavec == pattern[1L])
  if (!length(idx)) {
	message('Initial pattern value not found in data.')
	return(invisible( integer(0) ))
	}
  xl <- length(pattern) - 1L
 return(invisible( idx[sapply(idx, function(i) all(datavec[i:(i+xl)] == pattern))] ))
}
