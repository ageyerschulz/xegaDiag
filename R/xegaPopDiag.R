
#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Programming Algorithm in R. V 0.1
#          Layer: Population-level diagnostics.
#          Package: xegaPopDiag
#
#          Experimental version.

#' Plot population fitness statistics
#'
#' @description \code{xegaPlotPopStats} plots the population 
#'              fitness statistics (min, Q1, mean, median, Q3, max)
#'              of a run of a genetic algorithm.
#'
#' For an introduction to graphics in R, see 
#' Paul Murrell (2006) R-Graphics, Chapman & Hall/CRC, Boca Raton 
#'
#' @references Murrell, Paul (2006):
#'       \emph{R-Graphics}. 
#'       Chapman & Hall/CRC, Boca Raton.
#' 
#' TODO: Automatic file names for printing several runs ... 
#'
#' @param xegaResult the result of \code{RunSGA} (or \code{RunSGP}).
#' @param epsfile  If \code{epsfile} is \code{FALSE}, then the 
#'                        plot is shown on an X11-device else an epsfile
#'                       with the name of the problem environment is printed.
#' @param filename If specified, overrides filename of epsfile.
#' @param description Additional text.
#'
#' @return the return code of device.off()
#'
#' @family genetic algorithm visualization
#' @family visualization
#'
#' @examples
#' library(xega)
#' a<-xegaRun(Parabola2D)
#' xegaPlotPopStats(a)
#' \dontrun{xegaPlotPopStats(a, epsfile=TRUE)}
#'
#' @importFrom grDevices postscript
#' @importFrom grDevices dev.off
#' @importFrom graphics  lines
#' @importFrom graphics  mtext
#' @importFrom graphics  title
#' @export
xegaPlotPopStats<-function(xegaResult, epsfile=FALSE, filename=NULL, description="")
{

	name<-unlist(xegaResult$solution$name)
        if (xegaResult$GAenv$max) {max<-"Maximize"} else {max<-"Minimize"}

if (epsfile) 
{
	if (is.null(filename))
	{
	n1<-paste0(name, ".eps", seq="")
	rc<-postscript(file=n1)
	} 
	else
	{
	rc<-postscript(file=filename)
	} 
}

d<-xegaResult$popStat
config<-xegaResult$GAconfig
miny<-min(as.vector(d[,(2:6)]))
maxy<-max(as.vector(d[,(2:6)]))
d<-cbind((1:nrow(d)), d)
xlen<-nrow(d)
plot(d[,1], d[,2], ylim=c(miny, maxy), 
     xlab="Generations", ylab="Fitness",
     col="red", lty="solid", type="l", 
     sub=description)
lines(d[,1], d[,3], lty="dashed", type="l")
lines(d[,1], d[,4], col="blue", lty="dotted", type="l")
lines(d[,1], d[,5], col="green", lty="solid", type="l")
lines(d[,1], d[,6], col="blue", lty="dotted", type="l")
lines(d[,1], d[,7], lty="dashed", type="l")
# for the following we can use vector notation!
tv<-c("Min -----", "Q1 ......", "Mean", "Median", "Q3 ........", "Max -----")
cv<-c("black", "blue", "red", "green", "blue", "black")
pv<-floor(xlen*cumsum(c((1/14),rep((1/7), 5))))
mtext(tv, col=cv, at=pv)
# mtext(config, side=1, line=4) # string must be formatted and sizes set!
# mtext("Min ---", at=4, col="black")
# mtext("Q1 ....", at=13, col="blue")
# mtext("Mean   ", at=22, col="red")
# mtext("Median ", at=29, col="grey")
# mtext("Q3 ....", at=38, col="blue")
# mtext("Max ---", at=48, col="black")
title(paste(max," Population Fitness:", name, sep=" "))

if (epsfile) 
{
     rc<-dev.off()	
}

rc<-0
}

