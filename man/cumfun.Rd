\name{cumfun}
\alias{cumfun}

\title{
Function calculate the cumulative result of any function along an input vector.  
}
\description{
Calculates the cumulative value of almost any function in the same manner as \code{\link{cumsum} }
}
\usage{
cumfun(frist, FUN = sum, ...)
}

\arguments{
  \item{ frist}{
  A vector of numerical or string values
}
  \item{FUN}{	
The function to use. Can be either the function name (as is usually done) or a single character string naming the function.  Default is \code{sum}. 
}
  \item{...}{
 Additional arguments, if any, required for the chosen FUN function. See Details.
}
 
}
\details{
If additional arguments are of length one, they are applied to each value in \code{frist}. If they are longer but not equal in length to \code{length(frist)} they will either be truncated or recycled to match. 
The builtin functions \code{cumsum} and \code{cumprod} generally are much faster than applying \code{sum} or \code{prod} to this function. 
 
}
\value{
A list of the cumulative calculations generated.
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
\code{\link{cumsum}} , \code{\link[dplyr]{cumall}}, \code{\link[matrixStats]{rowCumsums}} 
}

\examples{
foo <- 'abcdefcghicklmno'
grepfoo <-function(x,y) grep(y,x)
cumfoo <- cumfun(unlist(strsplit(foo,'')), FUN=grepfoo, y = 'c')
bar <- rnorm(1000,1)
cumsd <- cumfun(bar,FUN=sd)
plot(unlist(cumsd),type='l')
# compare with input std dev
lines(c(0,1000),c(1,1),lty=2,col='red')
}
