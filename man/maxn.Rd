\name{maxn}
\alias{maxn}
\alias{minn}
\alias{which.maxn}
\alias{which.minn}

\title{
Functions to find the n-th maximum or minimum of a vector or array. 
}
\description{
These functions behave similarly to \code{min, max} and \code{which(x == max/min (x)} to find  the n-th greatest max or min of a dataset. 
}
\usage{
maxn(x,nth=1)
minn(x,nth=1)
which.maxn(x, nth = 1, arr.ind = FALSE, useNames = TRUE)
which.minn(x, nth = 1, arr.ind = FALSE, useNames = TRUE) 
}

\arguments{
  \item{ x}{
  A vector or array of numerical or string values.  Note: ordering of string values may not be what is expected. See Details.
}
  \item{nth}{	
Which lesser(greater, for min functions) to find, with default nth==1 being identical to \code{max, min}. 
}
  \item{arr.ind}{
Same meaning as for \code{which}, a logical value indicating whether to return the array indices if \code{x} is an array. 
}
   \item{useNames}{
Same meaning as for \code{which}, a logical value indicating if the value of arrayInd() should have (non-null) dimnames at all
}
}
\details{

Quoting the help page for \code{max} :
 Character versions are sorted lexicographically, and this depends on the collating sequence of the locale in use: the help for Comparison gives details. The max/min of an empty character vector is defined to be character NA. (One could argue that as "" is the smallest character element, the maximum should be "", but there is no obvious candidate for the minimum.) 
}
\value{
For \code{maxn, minn}, a single value which is the nth- max or min. 

For the \code{which.min,max} functions, quoting from \code{which}:
If arr.ind == FALSE (the default), an integer vector, or a double vector if x is a long vector, with length equal to sum(x), i.e., to the number of TRUEs in x. 
If arr.ind == TRUE and x is an array (has a dim attribute), the result is arrayInd(which(x), dim(x), dimnames(x)), namely a matrix whose rows each are the indices of one element of x

}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
\code{\link{which}} , \code{\link[base]{max}}
}

\examples{
set.seed(17) # for repeatability
foo <- matrix(sample(1:10,20,replace=TRUE),5,4)
maxn(foo,3)
which.minn(foo,4)
}
