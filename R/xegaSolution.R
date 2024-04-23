
#
# (c) 2021 Andreas Geyer-Schulz
#          Simple Genetic Programming Algorithm in R. V 0.1
#          Layer: Population-level diagnostics.
#          Package: xegaPopDiag
#
#          Experimental version.

#' Print solution
#'
#' @param s   GA solution.
#'
#' @family genetic algorithm solution
#'
#'@examples
#' library(xega)
#'   a<-xegaRun(Parabola2D, algorithm="sga", generations=10, popsize=100, profile=TRUE) 
#'   xegaPrintSolution(a)
#'@export
xegaPrintSolution<-function(s)
{ 
opt<-c("Min", "Max")
cat( opt[1+s$GAenv$max], " ", s$GAenv$penv$name(), "!\n")
if (s$solution$numberOfSolutions>1)
{cat(s$solution$numberOfSolutions, "solutions with equal fitness found.\n")}
cat("Fitness:", s$solution$fitness, 
    "f(Parameters):", s$solution$value$fit, "\n")
if ((s$GAenv$algorithm %in% c("sga", "sgde")) && 
    ("pnames" %in% names(s$GAenv$penv)))
      {
      Parameter<-s$GAenv$penv$pnames() 
      Value<-s$solution$phenotype
      df<-data.frame(Parameter, Value)
      print(df) 
      }
else 
      {cat("Parameters:", s$solution$phenotype, "\n")}
cat("Time used:", s$timer$tMainLoop, "seconds.\n") 
}

#' Profile of run time spent in the main blocks of a genetic algorithm.
#'
#' @param s   GA solution.
#' 
#' @family genetic algorithm
#' @family performance measurements
#'
#'@examples
#' library(xega)
#'   a<-xegaRun(Parabola2D, algorithm="sga", generations=10, popsize=100, profile=TRUE) 
#'   xegaPrintSolution(a)
#' @export
xegaProfile<-function(s)
{ 
	defaultOptions<-options()
	options(digits=3)
        options(scipen=999)
opt<-c("Min", "Max")
cat("Run time profile of:", opt[1+s$GAenv$max], " ", s$GAenv$penv$name(), "!\n")
if (s$GAenv$profile==FALSE) {
cat("Time used in Mainloop (in seconds):", s$timer$tMainLoop, "\n")
cat("To measure run time profile, rerun GA with option profile=TRUE.\n") 
InFunction<-list()
TimeSpent<-vector()
PercentageSpent<-vector()
R<-data.frame(InFunction, TimeSpent, PercentageSpent)
options(defaultOptions)
invisible(R)
}
else
{ TimeUsed<-names(s$timer)
Label<-list()
TimeSpent<-vector()
for (i in (2:length(TimeUsed)))
{ if (startsWith(TimeUsed[[i]], "t")==TRUE)
{ Label<-c(Label,substr(TimeUsed[[i]],2, nchar(TimeUsed[[i]])))
TimeSpent<-c(TimeSpent, s$timer[[i]]) }}
PercentageSpent<-100*(TimeSpent/sum(TimeSpent))
InFunction<-unlist(Label)
R<-data.frame(InFunction, TimeSpent, PercentageSpent)
print(R) 
options(defaultOptions)
invisible(R)
}
}

#' Compare run time and counts of main GA blocks of two runs.
#' 
#' @description \code{xegaCompareProfiles} takes the results of two runs 
#'              of genetic algorithms and builds a data frame with
#'              the time (names: t<variable>) and count (names: c<variable>)
#'              of the timers of the main GA blocks and functions.
#'                           
#' @details  The data frame has the following columns:
#'           \enumerate{
#'           \item \code{names} the names of the functions and code blocks.
#'                 Time variable names start with t, count variable names
#'                 start with c.
#'           \item Times in seconds and counts of first result.
#'           \item Times in seconds and counts of second result.
#'           \item Ratio first/second. Should be 1 for counts if SG algorithms
#                  are deterministic.
#'           \item Ratio second/first.
#'            }
#'
#' @param GaResultA the result of \code{RunSGA} (or \code{RunSGP}).
#' @param GaResultB the result of \code{RunSGA} (or \code{RunSGP}).
#'
#' @return a data frame 
#'           (<names>, time/count 1, time/count 2, ratio 1/2, ratio 2/1) 
#' 
#' @family genetic algorithm
#' @family performance measurements
#'
#'@examples
#' library(xega)
#'   a<-xegaRun(Parabola2D, algorithm="sga", generations=10, popsize=100, profile=TRUE) 
#'   b<-xegaRun(Parabola2D, algorithm="sga", generations=10, popsize=200, profile=TRUE) 
#'   xegaCompareProfiles(a, b)
#'@export
xegaCompareProfiles<-function(GaResultA, GaResultB)
{
	defaultOptions<-options()
        on.exit(options(defaultOptions))
	options(digits=3)
        options(scipen=999)
        cat("Comparison of A=", 
             GaResultA$GAenv$penv$name(),
             "and B=",
             GaResultB$GAenv$penv$name(),
             "\n")
        TimeSpentIn<-names(GaResultA$timer)
        RunA<-as.vector(unlist(GaResultA$timer))
        RunB<-as.vector(unlist(GaResultB$timer))
        AdivB<-RunA/RunB
        BdivA<-RunB/RunA
	ix<-unlist(lapply(names(GaResultA$timer), FUN=startsWith, prefix="t"))
        R<-data.frame(TimeSpentIn, RunA, RunB, AdivB, BdivA)
	colnames(R)<-c("Time spent in", "Run A", "Run B", "A/B", "B/A")
	print(R[ix,])
	GAoptions<-names(GaResultA$GAenv)
	sgaParameters<-list()
	RunA<-list()
	RunB<-list()
# skip penv and grammar!!
	for (i in (3:length(GAoptions)))
	{
#             cat("Option:", GAoptions[i],"\n")
#             cat("a:","\n")
#             print(GaResultA$GAenv[[i]])
#             cat("b:","\n")
#             print(GaResultB$GAenv[[i]])
        # case 1 both NULL
        if (is.null(GaResultA$GAenv[[i]]) &&
            is.null(GaResultB$GAenv[[i]]))
        {   next}
       
        # case 2 one NULL
        if (is.null(GaResultA$GAenv[[i]]) ||
            is.null(GaResultB$GAenv[[i]]))
        {   sgaParameters<-c(sgaParameters, GAoptions[i])
		RunA<-c(RunA, GaResultA$GAenv[[i]])
		RunB<-c(RunB, GaResultB$GAenv[[i]]) 
             next}
        # case 3 both NA
        if (is.na(GaResultA$GAenv[[i]]) &&
            is.na(GaResultB$GAenv[[i]]))
        {   next}
        # case 4 one NA
        if (is.na(GaResultA$GAenv[[i]]) ||
            is.na(GaResultB$GAenv[[i]]))
        {   sgaParameters<-c(sgaParameters, GAoptions[i])
		RunA<-c(RunA, GaResultA$GAenv[[i]])
		RunB<-c(RunB, GaResultB$GAenv[[i]]) 
             next}
        # case 5 differ?
	if (!GaResultA$GAenv[[i]]==GaResultB$GAenv[[i]])
	{ sgaParameters<-c(sgaParameters, GAoptions[i])
		RunA<-c(RunA, GaResultA$GAenv[[i]])
		RunB<-c(RunB, GaResultB$GAenv[[i]]) 
                 next}
        }
	if (!0==length(sgaParameters))
	{ P<-data.frame(unlist(sgaParameters), unlist(RunA), unlist(RunB))
	cat("\n")
	colnames(P)<-c("Different Parameters", "Run A", "Run B")
	print(P)}
        invisible(R[ix,])
}

