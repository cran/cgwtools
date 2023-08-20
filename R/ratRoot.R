# use rational root theorem to determine existence of (rational) roots.
# for AnX^n .... + A0 = 0, any rational solution X = p/q must satisfy both
#  p is factor of A0 and q is factor of An  (pos or neg)
# So follow the Medium article but test all combos of p, q after reducing {An}
# there is a %% for bigz as well as a lcm, gcd 


ratRoot <- function(Anum, Adenom = rep(1,times=length(Anum)) ){
# If user inputs floats, all calcs will be in floats; if in bigz in bigz or bigq
Anum <- floor(Anum)
Adenom <- floor(Adenom)
# modify numbers() funcs for streamlining since they'll be internal only
lcmrr <- function(x,y){
	if(x == 0 && y == 0) return(0) 
	return( x *y /gcfrr(x,y))
}
gcfrr <- function (x,y){
	if(x == 0 && y == 0) return (0)
	x <- abs(x)
	y <- abs(y)
	if (x < y){
		tmp <- x
		x <- y
		y <- tmp
	}
	while(y >0){
		tmp <- x
		x <- y
		y <- tmp %% y
	}
	return(x)
}
intA <- Anum * Reduce(lcmrr, Adenom) /Adenom 
lenA <- length(Anum)
# get factors:
# factorize won't operate on '1' or '0'
if (abs(intA[lenA] ) > 1){
	pfac <- unique(gmp::factorize(intA[lenA]))
} else pfac <- as.bigz(intA[lenA])
if (abs(intA[1] ) > 1){
qfac <- unique(gmp::factorize(intA[1]))
} else qfac <- as.bigz(intA[1])
pfac <- c(pfac,-pfac)
qfac <- c(qfac,-qfac)
theroots <- gmp::as.bigq(NULL)
#  browser()
for(jp in 1:length(pfac)){
	for(jq in 1:length(qfac)){
# do the test: stick p[jp]/q[jq] into polynomial and see if get zero
	this <- pfac[jp]/qfac[jq]
	that <- sum(rev(intA) * this^(0:(lenA-1)))
	if(that == 0) theroots <- c(theroots, this)
	}
}
return( unique(theroots) )
}



