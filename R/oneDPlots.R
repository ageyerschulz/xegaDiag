
#' Parobola2D 
#'
#' See package \code{xega}
#'
#' @family problem environments
#'
#' @importFrom xega Parabola2D
#' @export
Parabola2D<-xega::Parabola2D

#' 1-D plot of function f of problem environment penv.
#'
#' @description \code{envPlot1D} shows a 
#'    1-D plot of a high-dimensional function by fixing all
#'    coordinates but one.
#'    
#' @details
#' The parameter vectors \code{point} and \code{dim} allow to specify 
#' the fixed coordinates (by point) and the free variable (dim).  
#'
#' For all problem environments of package \code{envRealFunctions}
#' unconstrained real functions.
#'
#' @references Murrell, Paul (2006):
#'       \emph{R-Graphics}.
#'       Chapman & Hall/CRC, Boca Raton.
#' 
#' @param penv    A problem environment. E.g. \code{DeJongF1}
#'
#' The following 2 parameters specify the point and the free variable:
#'
#' @param point Coordinates at which the 2D-subspace 
#'                           will be evaluated. Default: NULL
#' @param dim   An integer between 1 and length(point). Default: NULL
#'
#' The last 2 parameters control the resolution of the plot and the
#' interactivity: 
#'
#' @param n           The number of points for 
#'                    the x and y coordinate of the plot. Default: \code{45}  
#' @param ask       The default is interactive. Default: \code{TRUE}
#' 
#' @section Comment:
#'   Suggestions for an improved interface are welcome. 
#'
#' @family visualization
#' @family real functions
#'
#' @examples
#' envPlot1D(Parabola2D)
#' envPlot1D(Parabola2D, point=c(2, -2), dim=2, n=200)
#' @importFrom graphics grid
#' @export
envPlot1D<-function(penv, point=NULL, dim=1, n=100, ask=TRUE)
{
coord<-function(c, point, dim) {point[dim]<-c; return(point)} 
if (is.null(point)) {point<- penv$lb()+ ((penv$ub() - penv$lb())/2) }
c<-seq(penv$lb()[dim], penv$ub()[dim], length=n) 
z<-lapply(lapply(c, FUN=coord, point=point, dim=dim), FUN=penv$f)
np<-point
np[dim]<-NA
title<-paste(penv$name(), " at (", toString(np), ")", sep="") 
plot(c, z, type="l", main=title)
grid()
}
