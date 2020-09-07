\name{dirdir}
\alias{dirdir}

\title{
Wrapper function around dir() which returns only directories found in the specified location(s).
}
\description{
For those times when you only want to know the local directories available, use this instead of struggling through myriad arguments to \code{\link{dir} }.  All arguments are the same as for plain old "dir" and are passed to \code{\link{dir} }. 
}
\usage{
dirdir(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE,
 recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
}

\arguments{
  \item{ path}{	
a character vector of full path names; the default corresponds to the working directory, getwd(). Tilde expansion (see path.expand) is performed. Missing values will be ignored. Elements with a marked encoding will be converted to the native encoding (and if that fails, considered non-existent).
}
  \item{pattern	}{
an optional regular expression. Only file names which match the regular expression will be returned.
}
  \item{all.files}{	
a logical value. If FALSE, only the names of visible files are returned (following Unix-style visibility, that is files whose name does not start with a dot). If TRUE, all file names will be returned.
}
  \item{full.names	}{
a logical value. If TRUE, the directory path is prepended to the file names to give a relative file path. If FALSE, the file names (rather than paths) are returned.
}
  \item{recursive	}{
logical. Should the listing recurse into directories?
}
  \item{ignore.case	}{
logical. Should pattern-matching be case-insensitive?
}
  \item{include.dirs	}{
logical. Should subdirectory names be included in recursive listings? (They always are in non-recursive ones).
}
  \item{no..}{	
logical. Should both "." and ".." be excluded also from non-recursive listings?
}

}

\value{
Note: this is directly quoted from the man page for \code{dir}.
A character vector containing the names of the files in the specified directories (empty if there were no files). If a path does not exist or is not a directory or is unreadable it is skipped.

The files are sorted in alphabetical order, on the full path if full.names = TRUE.

}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\seealso{
\code{\link{dir}} 
}
