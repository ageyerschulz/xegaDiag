
#' Interactive perspective plot of function f of problem environment penv.
#'
#' @description \code{plotPersp} shows a series of perspective plots
#'    of a 2 dimensional (sub)space of the function \code{penv$f}
#'    which are rotated in 15 degree steps for theta and phi. 
#'    
#' @details
#' For higher dimensional functions, we can only plot 2D-subspaces.
#' The parameter vectors \code{point} and \code{mask} allow to specify 
#' the subspace which will be plotted. Both vectors have the same 
#' length as \code{penv$lb()}.
#'
#' @references Murrell, Paul (2006):
#'       \emph{R-Graphics}.
#'       Chapman & Hall/CRC, Boca Raton.
#' 
#' @param penv    A problem environment. E.g. \code{DeJongF1}
#'
#' The following 2 parameters specify which 2D-subspace is 
#' evaluated at which ``point''.
#'
#' @param point Coordinates at which the 2D-subspace 
#'                           will be evaluated. Default: NULL
#' @param mask   A vector with exactly two 1s, and the rest 0s. Default: NULL
#'
#' The last 2 parameters control the resolution of the plot and the
#' interactivity. 
#'
#' @param n           The number of points for 
#'                    the x and y coordinate of the plot. Default: 45 
#' @param ask          The default is interactive. Default: TRUE
#' 
#' @section Comment:
#'   Suggestions for an improved interface are welcome. 
#'
#' @family visualization
#' @family real functions
#'
#' @examples
#' envPlotPersp(Parabola2D)
#' envPlotPersp(Parabola2D, point=c(2, -2), mask=c(1, 1))
#' @importFrom graphics par
#' @importFrom graphics persp
#' @export
envPlotPersp<-function(penv, point=NULL, mask=NULL, n=45, ask=TRUE)
{

grid2d<-function(x, y, penv) 
{       m<-matrix(0, nrow=length(x), ncol=length(y))	
	for (i in (1:length(x)))
	{  for (j in (1:length(y)))
	  { m[i,j]<-penv$f(c(x[i],y[j])) }
	}
       return(m) } 

gridhd<-function(x, y, penv, ixy, point) 
{       m<-matrix(0, nrow=length(x), ncol=length(y))	
	for (i in (1:length(x)))
	{  for (j in (1:length(y)))
	  { 
		  p<-point
		  p[ixy]<-c(x[i], y[j]) 
		  m[i,j]<-penv$f(p) 
	  }
	}
       return(m) } 

if (length(penv$lb())>2)
{
len<-length(penv$lb())
if (is.null(mask)) {  mask<-c(1, 1, rep(0, (len-2)) ) }
if (is.null(point)) {point<- penv$lb()+ ((penv$ub() - penv$lb())/2) }
ixy<-seq(1:len)[as.logical(mask)]
x<-seq(penv$lb()[ixy[1]], penv$ub()[ixy[1]], length=n)
y<-seq(penv$lb()[ixy[2]], penv$ub()[ixy[2]], length=n)
z<-gridhd(x, y, penv, ixy, point)
}

if (length(penv$lb())==2)
{
	x<-seq(penv$lb()[1], penv$ub()[1], length=n)
	y<-seq(penv$lb()[2], penv$ub()[2], length=n)
	z<-grid2d(x, y, penv)
}

par(ask=ask)

for (i in (1:6))
{
for (j in (1:6))
{
thet<-(i*15)%%360	
ph<-(j*15)%%360
t<-paste(penv$name(), " theta=", toString(thet), "phi", toString(ph), seq="")
if (length(penv$lb())>2)
{ t<-paste(t, " point=(", toString(point), ") xy=(", toString(ixy),")", seq="")
}
persp(x, y, z, theta=thet, phi=ph, main=t, box=TRUE, xlab="X", ylab="Y", 
      zlab="f(x, y)")
}}

}
