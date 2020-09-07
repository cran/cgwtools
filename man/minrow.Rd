\name{minRow}
\alias{minRow}
\alias{maxRow}
\alias{minCol}
\alias{maxCol}

\title{
Functions which mimic \code{\link{max.col}} to find for minimum or maximum of rows or columns. 
}
\description{
These are Q&D wrappers around \code{max.col} to make it easy to get the positions of  max or the min of either rows or columns of an array. The description of the base function is, for comparison, "Find the maximum position for each row of a matrix".
}

\usage{
maxRow(mat,ties.method = c("random", "first", "last") )
minRow(mat,ties.method = c("random", "first", "last") )
minCol(mat,ties.method = c("random", "first", "last") )
maxCol(mat,ties.method = c("random", "first", "last") )
}

\arguments{
\item{mat}{
A 2-D matrix, same rules as for \code{max.col}
}
\item{ties.method}{
Specify how to deal with ties, using same internal rules as \code{max.col}
}
}
 
\value{
For each of these functions, same as for \code{\link{max.col} }:
index of a {maximal, minimal} value for each {row, column}, an integer vector of length {nrow(mat), ncol(mat)}

}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
\code{\link{max.col}} 
}
