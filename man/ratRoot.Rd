\name{ratRoot}
\alias{ratRoot}

\title{
Function to find the rational roots of any polynomial (when they exist).  
}
\description{
.   
}

\usage{
ratRoot(Anum, Adenom = rep(1,times=length(Anum)) )

}

\arguments{
  \item{ Anum}{
A vector of the polynomial coefficients' numerators, starting with the highest power.  Values can be numeric or bigz.
}
  \item{ Adenom}{
 A vector of the polynomial coefficients' denominators, starting with the highest power.  Values can be numeric or bigz. Default is all ones, indicating that all input coefficients are integers. 
}

}


\details{
The code makes use of the 'Rational Root Theorem,' which states that all real, rational roots p/q meet two criteria. Given a polynomial of the form \code{An *x^n + A_(n-1) * x^(n-1) + ... + A0 = 0}, all coefficients adjusted to be integers,  'p' must be a factor of the zero-power polynomial coefficient A0, and 'q' must be a factor of the maximum-power polynomial coefficient An. 
If any of the input \code{Adenom} are not '1', the function will make this adjustment.  

}
\value{
A vector of \code{bigq} fractions representing all real rational roots found.  If empty, all roots are irrational (or perhaps complex). 
  
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\references{

 \url{https://en.wikipedia.org/wiki/Rational_root_theorem} 
 
}

\examples{
ratRoot(c(6, -15, 261), c(1,1,49))
ratRoot(c(1,0,0,0,-1))
ratRoot(c(1,-2,1))

}

