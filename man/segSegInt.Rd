\name{segSegInt}
\alias{segSegInt}

\title{
Function to find intersection point between two line segments (NOT lines). 
}
\description{
This function finds the intersection of the two lines containing the provided line segments, then checks whether the intersection is contained within both segments.  
This is an implementation of the equations presented, among many other sources, at   \url{http://paulbourke.net/geometry/pointlineplane/} .  Bourke comments there, that  "The equations apply to lines, if the intersection of line segments is required then it is only necessary to test if ua and ub lie between 0 and 1. Whichever one lies within that range then the corresponding line segment contains the intersection point. If both lie within the range of 0 to 1 then the intersection point is within both line segments."

}
\usage{
segSegInt(seg1, seg2=NULL, plotit=FALSE, probParallel = 1e-10,  ...)
}

\arguments{

  \item{ seg1}{
  An 2x2 or 4x2 matrix with X-values in the first column and Y-values in the second column. If 4x2, the lower 2 rows are assigned to \code{seg2} and the input argument \code{seg2} is ignored. 
}
  \item{ seg2}{
  An 2x2 or matrix with X-values in the first column and Y-values in the second column.  Ignored if \code{seg1} is 4x2.
}
  \item{plotit}{
 Boolean: if TRUE, (and stopAtFirst is FALSE), the two line segments are plotted and, if one exists, the intersection point marked. 
}

\item{probParallel}{
 A numeric value, typically quite small, used to identify line segments which probably are parallel. This basically is checking for identical slopes. 
 }
 \item{...}{
 Not used at present
 }

}
\details{
The function runs a check for parallelism so as not to return an infinity of intersection points, then basically checks for intersection of the lines to which the line segments belong, and finally verifies said intersection belongs to both line segments. This is an implementation of the equations presented at, among many other sources, \url{http://paulbourke.net/geometry/pointlineplane/} .  Bourke comments there, that  "The equations apply to lines, if the intersection of line segments is required then it is only necessary to test if ua and ub lie between 0 and 1. Whichever one lies within that range then the corresponding line segment contains the intersection point. If both lie within the range of 0 to 1 then the intersection point is within both line segments."
}

\value{
A matrix of the x and y coordinates of the intersection points. If no intersection exists, \code{c(NA,NA)} is returned.  This is done so the return value is always a 2-element vector. 
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\seealso{

\code{\link{polyInt}}  
}


