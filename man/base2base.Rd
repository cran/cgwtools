\name{base2base}
\alias{base2base}

\title{
Function to convert any base to any other base (up to 36). 
}
\description{
For a named vector or value, specify the input and output bases and convert.  
}
\usage{
base2base(x,frombase=10, tobase=2)
}

\arguments{
  \item{ x}{
  A value of class numeric, bigz, or character. Alternatively, a list containing such values.   See Details for various limitations  
}
  \item{frombase}{	
An integer from 2 thru 36 indicating the base of \code{x}.
}
  \item{tobase}{	
An integer from 2 thru 36 indicating the desired base of the returned value.
}
 
}
\details{
If \code{x} has more than one value (e.g. a vector of hex characters) they will be collapsed into a single value. If \code{x} is a list, then this is done to each list entry in turn. 
For hexadecimals, i.e. when \code{frombase} is set to 16, any leading "0x" will be stripped for you. 
The standard alpha "encoding" for digits in bases 11 thru 36 is assumed, i.e. 0-9,a-z .

Keep in mind that builtin functions such as \code{\link[base]{strtoi}} will be much faster for base conversions supported by those functions. 
 
}
\value{
The equivalent value of \code{x} in the new base.  If \code{x} is a list, a matching list is returned. 
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
\code{\link[base]{strtoi}}
}

\examples{
base2base('101011',2,10)
base2base(101011,2,10)
base2base('4r8s',30,10)
base2base('101011',12,5)


}
