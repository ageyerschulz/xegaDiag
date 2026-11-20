


#' Convert a named parameter list into a dataframe.
#'
#' @description For island solutions (configuration lists), spefify the process id. 
#'
#' @details 
#'
#'  Some parameters need special handling:
#'  \enumerate{
#'     \item \code{$penv}: Replace penv by name of penv.
#'     \item \code{$grammar}: Replace grammar by its name.
#'     \item \code{$uParApply}: Replace object by hint.
#'     \item \code{$Cluster}: Replace object by hint.
#'     \item \code{$RmpiFNS}: Replace object by name list.
#'     }
#'
#' @param solution  A xega-solution object
#' @param ...       For a configuration list, the process id 
#'                  must be specified to index the configuration.
#'                  E.g. To select the 2nd of a list of 5 configurations,
#'                  specify \code{pid=2}.
#'
#' @return A dataframe with one column: The parameter values.
#'         The rownames are the (raw) parameter names.
#'
#' @family Reflection
#'
#' @examples
#' a<-xega::xegaRun(Parabola2D, max=FALSE, algorithm="sga", generations=10, popsize=20, verbose=1)
#' xegaParameters(a) 
#'
#' @export
xegaParameters<-function(solution, ...)
{
iSolution<-solution

#### solution is a configuration list.
if (is.null(names(solution)))
{
newArgList<-eval(substitute(alist(...)))
#### we pick the solution for the pid.
iSolution<-solution[[1+newArgList$pid]]
}

pl<-iSolution$GAenv
npl<-pl
n<-names(pl)
if ("penv" %in% names(pl)) {npl$penv<-pl$penv$name()}
if ("grammar" %in% names(pl)) {npl$grammar<-pl$grammar$name}
if ("uParApply" %in% names(pl)) 
    {if (!is.null(pl$uParApply))
      npl$uParApply<-"Function body in <o>$GAenv$uParApply"}
if ("RmpiFNS" %in% names(pl)) 
    {if (!identical(pl$RmpiFNS, "unused"))
      npl$RmpiFNS<-Reduce(paste, pl$RmpiFNS)}
if ("Cluster" %in% names(pl)) 
    {if (!is.null(pl$Cluster))
      npl$Cluster<-"Cluster definition in <o>$GAenv$Cluster"}
nulls<-lapply(npl, FUN=is.null)
npl[unlist(nulls)]<-rep("NULL", sum(unlist(nulls)))
df<-as.data.frame(npl)
df<-t(df)
colnames(df)<-c("Parameter Values")
return(df)}

