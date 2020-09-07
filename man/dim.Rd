\name{dim}
\alias{dim}
\title{
Function to return dimensions of arguments, or lengths if dim==NULL.
}
\description{
Simple overload to return \code{\link{dim}} when it's sensible and \code{\link{length}} otherwise

}
\usage{
dim(item)
}
\arguments{
  \item{item}{
  The object whose dimensions are to be determined
}
}

\value{
Either a single value as returned by \code{\link{length}}  or  a vector of integers indicating the magnitude of each dimension as returned by \code{\link{dim}}
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\seealso{
\code{\link{dim}} , \code{\link{length}}, 
}
\examples{
x1<-1:10
x2<-matrix(1,3,4)
dim(x1)
dim(x2)
}