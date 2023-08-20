\name{resave}
\alias{resave}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add some objects to an existing \code{.Rdata} - type file.
%%  ~~function to do ... ~~
}
\description{
Take an existing \code{myfile.Rdata} data file and add the specified objects to it.  This is achieved by opening the data file in a local environment, "dumping" the new objects into that environment, and re-saving everything to the same file name.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resave(..., list = character(), file, overwrite = TRUE, loadverbose = FALSE )
}

\arguments{
  \item{\dots}{
  Names of objects to save. Same usage as in \code{base::save}

}
  \item{list}{
  A list of names of the objects to save.  Can be used with or without any named arguments in \code{...} . Same usage as in \code{base::save}
}
  \item{file}{
  The name of the file to open and add items to.
}
  \item{overwrite}{
  If TRUE (the default), items currently in the selected file will be overwritten with items of the same name in the console environment.  If FALSE, they will not be overwritten; a warning message will be issued. 
}
  \item{loadverbose}{
  Default is FALSE. If TRUE, then the names of all objects currently in the file are echoed to the console
}
}

\value{Nothing is returned. This function is used solely to put objects into the file.
}

\author{
Carl Witthoft  \email{carl@witthoft.com}
%%  ~~who you are~~
}
\note{
Code is essentially the same as that provided by  "flodel" on StackOverflow, with some enhancements based on  "save_append" by "nehartj" on GitHub  
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lsdata}} , \code{\link{save}}, \code{\link{load}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## not run to avoid creating detritus
# foo<-1:4
# bar<-5:8
# save(foo,file='foo.Rdata')
# resave(bar,file='foo.Rdata')
# #check your work
# lsdata('foo.Rdata')
}