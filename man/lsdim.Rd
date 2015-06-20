\name{lsdim}
\alias{lsdim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return dimensions of arguments, or lengths if dim==NULL.
%%  ~~function to do ... ~~
}
\description{
Just a toy to return dim() when it's sensible and length() elsewise
# items must be collection or list of character strings, e.g. output of ls()
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lsdim(items)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{
  A vector of character strings identifying the objects of interest as would be returned by, e.g. \code{ls(pattern="foo")} or \code{lstype("double")} .
%%     ~~Describe \code{items} here~~
}
 
}

\value{A list, each element of which gives the object dimensions if there are any, and the length if not. 

}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\seealso{
\code{\link{dim}} , \code{\link{length}}, 
%% ~~objects to  ~~~
}
\examples{
x1<-1:10
x2<-matrix(1,3,4)
lsdim(c('x1','x2'))
}