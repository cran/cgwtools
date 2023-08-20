\name{banvax}
\alias{banvax}

\title{
Function print a text banner in the style of the original DEC VAX "banner" function.  
}
\description{
This function takes a character string and writes the same characters as large block-letters into a text file.   
}

\usage{ 
banvax(msg, file = 'banner.txt',  linewid = 80, bandat = cgwtools::ascarr)
}

\arguments{
\item{msg}{
A character string. See the Details section for the behavior for nonmatching characters. 
  }
\item{file}{
A character string identifying an output file, or a \code{\link[base]{connection} } as described in the help page for \code{\link[base]{cat}} .
  }
\item{linewid}{
Defines the max number of characters per line. Change from the default value of 80 depending on the display or paper size in use. 
}
\item{bandat}{
The data array to use for the banner elements. In general, must be a 3-dimensional array with dimensions [x,y,N] where each n-th layer contains the banner form of one character. See the Details section.
}
}
\details{
The  supplied data file \code{ascarr} contains all letters and numerals and a bunch of punctuation marks such as " ;" , "," "\\" , etc. If a character in the input is not found in the file, a "box" is used in its place.  If a user-supplied file is specified, note that each layer of the [x,y,N] array must have a name equal to the character to be invoked. Here \code{x} specifies the number of columns and \code{y} the number of rows in each banner-element. 

The function \code{\link[base]{cat}} generates the output, so setting the argument \code{file} to "" will direct the output to the console.  Quoting from the help page for \code{\link[base]{cat}}, ' If "" (the default), cat prints to the standard output connection, the console unless redirected by \code{\link[base]{sink}}. If it is "|cmd", the output is piped to the command given by  cmd , by opening a pipe connection. '
  }
\value{
Nothing is returned from the function. The output is a file or whatever \code{\link[base]{connection} } is specified by the \code{file} argument. 

}

\author{
Author and Maintainer:Carl Witthoft \email{carl@witthoft.com}

}

\references{
\url{https://www0.mi.infn.it/~calcolo/OpenVMS/ssb71/6015/6017p041.htm#index_x_2757}
}