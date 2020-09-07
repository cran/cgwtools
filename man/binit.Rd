\name{binit}
\alias{binit}

\title{
Create histogram bins for each unique value in a sample. 
}
\description{
This is a Q&D way to create Pareto / histogram bins of a dataset when you want a separate bin for each value and don't want to deal with the 'breaks' or equivalent arguments in \code{\link{hist}} or other histogram functions in R packages. 
}
\usage{
binit(samps,roundPrec=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samps}{
  A vector or array of data to be binned.
}
\item{roundPrec}{
The number of digits to round \code{samps} to. Highly recommended when the data are floats
}
}

\details{
\code{binit} sorts the input data and feeds the result to \code{\link{rle}}.  This effectively produces histogram-like results. If you want a strict Pareto order (most common first), just sort the list elements \code{lengths} and \code{values} by the magnitudes in \code{lengths}.
}
\value{
A list containing two elements
lengths: the number of items in each bin
values: the data value associated with each bin
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\seealso{
\code{\link{rle}} , \code{\link{hist}}, 
%% ~~objects to  ~~~
}
\examples{
x <- sample(1:100, 1000, rep=TRUE)
xbin <- binit(x)
plot(xbin$values,xbin$lengths, type = 'h')
# without rounding, will just be grass
x <- rnorm(1000)
 xbin <- binit(x,2)
plot(xbin$values,xbin$lengths, type = 'h')
}