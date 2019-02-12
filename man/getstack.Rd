\name{getstack}
\alias{getstack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns the current directory stack that pushd and popd manipulate
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 \code{getstack}  goes into the separate environment where \code{pushd} and 
 \code{popd} operate and returns the current stack of directories.
}
\usage{
getstack()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  none
}
\details{
Allowing a function to modify an object in the GlobalEnvironment is frowned upon by CRAN (and most programmers), so to maintain a directory stack a separate environment is established by \code{pushd}. Since this environment is not visible at the console level,  \code{getstack} allows the user to check on the current status of the stack.
}
\value{
The current directory stack is returned as a vector of strings.
}

\author{
Carl Witthoft   \email{carl@witthoft.com}
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{popd}} , \code{\link{pushd}}, \code{\link{setwd}}
}
\examples{
## depends on your local directory structure and permissions
getwd()
getstack() #empty, probably
pushd('..')
getstack()
pushd('.')
getstack()
popd()
getstack()
popd()
getstack()
getwd() #back where we started
}
