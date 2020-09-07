\name{findpat}
\alias{findpat}

\title{
Function to locate patterns ( sequences of strings or numerical values) in data vectors. 
}
\description{
Finds the location of a specified sequence either of numbers or strings in the source data item.  If desired, for numerical data, both the source and the named pattern can be rounded to specified number of digits. 
}
\usage{
findpat(datavec, pattern, roundit = NULL)
}

\arguments{
  \item{ datavec}{
  A vector of numerical or string values
}
  \item{pattern}{	
A vector containing the sequence to search for
}
  \item{roundit}{
  If not NULL, sets the precision for rounding numbers.  Ignored if \code{datavec} are strings
}
 
}
\details{
If \code{datavec}  and \code{pattern} are of different types, R will automagically convert to a common type and then compare the values. This is a result of the coercion rules identified in the man page for \code{\link{Comparison}}, "If the two arguments are atomic vectors of different types, one is coerced to the type of the other, the (decreasing) order of precedence being character, complex, numeric, integer, logical and raw."  
Use of the \code{roundit} argument is recommended whenever working with  doubles (floats) to avoid the well-known and often overlooked pain of binary precision errors. 
}
\value{
If the first element of \code{pattern} isn't found, a message is posted and an empty integer vector is returned.
Otherwise, a vector of the indices of datavec where the desired pattern is located is returned.  These are the indices of the start of the pattern. 
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
\code{\link{which}} , \code{\link[filesstrings]{nth_number_after_mth}}, \code{\link[pracma]{strfind}}
}

\examples{
fooc <- letters[c(1:15,4:9,12:26)]
 findpat(fooc,c('d','e','f'))
# 4 16
fooi <- c(1:50,5:9,60:80)
findpat(fooi,6:8)
# 6 52
findpat(fooi,c('6','7','8'))
# also 6 52
}
