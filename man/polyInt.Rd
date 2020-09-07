\name{polyInt}
\alias{polyInt}

\title{
Function to find intersection points of two polygons. 
}
\description{
This is a Q&D tool to find the locations where two polygons, in a plane only (not 3D space), intersect.   
}

\usage{
polyInt(poly1,poly2, stopAtFirst = FALSE, plotit = FALSE, roundPrecision = 10, ...)
}

\arguments{
  \item{ poly1}{
  An Nx2 or 2xN matrix with X-values in the first row/column and Y-values in the second.
}
  \item{ poly2}{
  An Nx2 or 2xN matrix with X-values in the first row/column and Y-values in the second.
}
  \item{stopAtFirst}{	
Boolean: if TRUE, then just return the first intersection point found. Useful time-saver when the user only needs to know if the polygons intersect. 
}
  \item{plotit}{
 Boolean: if TRUE, (and stopAtFirst is FALSE), the two polygons are plotted and the intersection points marked on the graph. 
}
\item{roundPrecision}{
Number of digits that data should be rounded to.  This is necessary to avoid the usual floating-point precision problems when checking for possible duplicated intersection points. 
}
 \item{...}{
 Arguments to be passed along to the internal \code{line} call when \code{plotit} is TRUE.  

}
}

\details{
The function loops over all pairs of segments (one from \code{poly1} and one from \code{poly2}), calling \code{\link{segSegInt} } to see if they intersect. After all pair-combinations are tested, the collected intersection points, if any, are reduced to the unique collection. This avoids repetition when an intersection point is a vertex of one (or both) of the polygons. 
It is not necessary to "close" the supplied set of vertices, i.e. repeat the initial vertex at the end of the array as is needed to generate a complete line-plot of a polygon. The function will add that repeated vertex if it's not present in the input polygon(s). 

Note: The supporting function \code{\link{segSegInt} } returns NA when two segments are parallel. However, when two polygons in fact have an overlapping (and thus parallel) couple of  edges, the adjoining edges of one or both polygons will not be parallel to these parallel edges and will intersect one or both, so the vertex which lies on the other polygon's edge will be reported. 
}
\value{
A matrix of the x and y coordinates of all intersection points, or, if \code{stopAtFirst} is TRUE, the first intersection point found.  If no intersections exist, NULL is returned.  
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\seealso{  
There are many tools which are far faster and more flexible. I wrote this one because it uses only base functions and doesn't require converting polygon vertices into a special class variable. Here are three common packages.  \code{\link{intersect}} , \code{\link[sf]{st_intersection}}, \code{\link[rgeos]{gIntersection}}
}

\examples{
sqone <- cbind(c(0,1,1,0),c(0,0,1,1))
sqtwo <- sqone + 0.5 
foo <- polyInt(sqone, sqtwo, plotit = TRUE)

}

