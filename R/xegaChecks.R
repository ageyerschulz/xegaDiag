
#
# (c) 2023 Andreas Geyer-Schulz
#          Simple Genetic Programming Algorithm in R. V 0.1
#          Layer: Population-level diagnostics.
#          Package: xegaPopDiag
#
#          Experimental version.

#' Check existence of function in problem environment.
#'
#' @param name   name of function
#' @param penv   problem environment
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @return stateOfCheck
#'
#' @family interface check
#'
#' @export
xegaCheckFunctionExists<-function(name, 
				penv, 
				stateOfCheck=list(Passed=0, Failed=0))
{ if (!name %in% names(penv)) 
   { cat("[Error in penv] Function [",  name, "] is missing!\n") 
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
else   
   { stateOfCheck$Passed<-stateOfCheck$Passed+1
	   invisible(stateOfCheck) }}

#' Check header of functions without arguments.
#'
#' @param name   name of function
#' @param penv   problem environment
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @return stateOfCheck
#'
#' @family interface check
#'
#' @export
xegaCheckFunctionWithoutArguments<-function(name, 
				penv, 
				stateOfCheck=list(Passed=0, Failed=0))
{ if (!name %in% names(penv)) 
   { cat("[Error in penv] Function [",  name, "] is missing!\n") 
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
else   
   { 
	 fn<-penv[names(penv) %in% name] 
if (!identical(NULL,formals(fn[[1]]))) 
   {
    cat("[Error in penv,", name, "] Expected function header:\n")
    cat(name, "name, <-function() # Function name has no arguments!\n")
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
   else
   {	   
	   stateOfCheck$Passed<-stateOfCheck$Passed+1
	   invisible(stateOfCheck) }
   }}

#' Check return value of function is a non-empty character string
#'
#' @param name   name of function
#' @param penv   problem environment
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @return stateOfCheck
#'
#' @family interface check
#'
#' @export
xegaCheckFunctionValueIsACharacterString<-function(name, 
				penv, 
				stateOfCheck=list(Passed=0, Failed=0))
{ if (!name %in% names(penv)) 
   { cat("[Error in penv] Function [",  name, "] is missing!\n") 
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
else   
   { 
	 fn<-penv[names(penv) %in% name] 
if (!("character"==class(fn[[1]]()))) 
  { cat("[Error in penv,", name, "] ")
    cat("Return value of name must be a character string!\n")
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
if (!(1==length(fn[[1]]()))) 
  { cat("[Error in penv,", name, "] ")
cat("Expected return value is exactly one character string!\n")
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
if (!identical("character", class(penv$name)) && !(0<nchar(penv$name()))) 
  { cat("[Error in penv,", name, "] ")
    cat("Expected length of character string returned > 0!\n")
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
   else
   {	   
	   stateOfCheck$Passed<-stateOfCheck$Passed+1
	   invisible(stateOfCheck) }
   }}

#' Check return value of function is numeric.
#'
#' @param name   name of function
#' @param penv   problem environment
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @returns stateOfCheck
#'
#' @family interface check
#'
#' @export
xegaCheckFunctionValueIsNumeric<-function(name, 
				penv, 
				stateOfCheck=list(Passed=0, Failed=0))
{ if (!name %in% names(penv)) 
   { cat("[Error in penv] Function [",  name, "] is missing!\n") 
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
else   
   { 
	 fn<-penv[names(penv) %in% name] 
if (!("numeric"==class(fn[[1]]())))  #### integer not yet tested! 
  { cat("[Error in penv,", name, "]")
    cat("Return value of function [", name, "] must be an integer vector!\n")
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
   else
   {	   
	   stateOfCheck$Passed<-stateOfCheck$Passed+1
	   invisible(stateOfCheck) }
   }}

#' Check return value of function has a length >0
#'
#' @param name   name of function
#' @param penv   problem environment
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @returns stateOfCheck
#'
#' @family interface check
#'
#' @export
xegaCheckFunctionValueLengthNotZero<-function(name, 
				penv, 
				stateOfCheck=list(Passed=0, Failed=0))
{ if (!name %in% names(penv)) 
   { cat("[Error in penv] Function [",  name, "] is missing!\n") 
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
else   
   { 
	 fn<-penv[names(penv) %in% name] 
if (! 0<length(fn[[1]]())) 
  { cat("[Error in penv,", name, "]")
    cat("Length of return value of function [", name, "] must be greater zero!\n")
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
   else
   {	   
	   stateOfCheck$Passed<-stateOfCheck$Passed+1
	   invisible(stateOfCheck) }
   }}

####
# For consistency tests:

#' Get return value of function \code{name}
#'
#' @param name   name of function
#' @param penv   problem environment
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @returns stateOfCheck
#'
#' @family interface check
#'
#' @export
xegaGetPenvVal<-function(name, 
				penv, 
				stateOfCheck=list(Passed=0, Failed=0))
{ 
	if (!name %in% names(penv)) 
   { cat("[Error in GetPenVal] Function [",  name, "] is missing!\n") 
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(list(value=0, fail=TRUE, stateOfCheck=stateOfCheck)) }
else   
   { 
   fn<-penv[names(penv) %in% name] 
   stateOfCheck$Passed<-stateOfCheck$passed+1
   invisible(list(value=fn[[1]](), fail=FALSE, stateOfCheck=stateOfCheck)) }
}

#' Check condition
#'
#' @param description  description of condition
#' @param cond   conditional expression (TRUE or FALSE)
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @returns stateOfCheck
#'
#' @family interface check
#'
#' @export
xegaCheckCondition<-function(description, 
				cond, 
				stateOfCheck=list(Passed=0, Failed=0))
{ 
	if (!cond) 
   { cat("Failed:", description, "\n") 
	   stateOfCheck$Failed<-stateOfCheck$Failed+1
	   invisible(stateOfCheck) }
else   
   { 
	   stateOfCheck$Passed<-stateOfCheck$Passed+1
	   invisible(stateOfCheck) }
}

#' Check fitness function
#'
#' @param penv           problem environment
#' @param stateOfCheck  state of check. Default: Passed=0, Failed=0. 
#'
#' @family interface check
#'
#' @export
xegaCheckFitnessFunction<-function(penv,
				stateOfCheck=list(Passed=0, Failed=0))
{
if (!"f" %in% names(penv)) 
   { cat("[Error in GetPenVal] Function [ f ] is missing!\n") 
   stateOfCheck$Failed<-stateOfCheck$Failed+1
   invisible(stateOfCheck) }
else {
parm<-xegaGetPenvVal("ub", penv, stateOfCheck)
if (parm$fail==TRUE) 
{
 cat("[Error] ub() did not return values for evaluation of fitness function!\n")
 invisible(parm$stateOfCheck)     
}
else
{
fit<-tryCatch(
      penv$f(parm$value),
      error = function(e) 
      {cat("f(parm) failed:\n")     
       print(e, "\n")
       NA})
if (is.na(fit)) 
{ stateOfCheck$Failed<-stateOfCheck$Failed+1
  invisible(stateOfCheck) }

fit<-tryCatch(
      penv$f(parm$value, gene=7, lF=NA),
      error = function(e) 
      {cat("f(parm, gene, lF) failed. \n")     
       print(e, "\n")
       NA})
if (is.na(fit)) 
{ stateOfCheck$Failed<-stateOfCheck$Failed+1
  invisible(stateOfCheck) }

stateOfCheck$Passed<-stateOfCheck$Passed+1
invisible(stateOfCheck) 

}}}

####

#' Check problem environment for xegaRun (algorithm="sga")
#' 
#' 
#'
#' @param penv   problem environment
#'
#' @family interface tests
#'
#'@export
xegaCheckProblemEnvironment<-function(penv)
{
stateOfCheck<-list(Passed=0, Failed=0)	
### function names.
stateOfCheck<-xegaCheckFunctionExists("name", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionWithoutArguments("name", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionValueIsACharacterString("name", penv, stateOfCheck) 

### function bitlength.
stateOfCheck<-xegaCheckFunctionExists("bitlength", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionWithoutArguments("bitlength", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionValueIsNumeric("bitlength", penv, stateOfCheck) 
stateOfCheck<-xegaCheckFunctionValueLengthNotZero("bitlength", penv, stateOfCheck) 

### function genelength.
stateOfCheck<-xegaCheckFunctionExists("genelength", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionWithoutArguments("genelength", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionValueIsNumeric("genelength", penv, stateOfCheck) 

t1<-xegaGetPenvVal("bitlength", penv, stateOfCheck)
t2<-xegaGetPenvVal("genelength", penv, stateOfCheck)
stateOfCheck<-xegaCheckCondition("genelength==sum(bitlength)",
                                (sum(t1$value)==t2$value), 
                                stateOfCheck) 

### function lb.
stateOfCheck<-xegaCheckFunctionExists("lb", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionWithoutArguments("lb", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionValueIsNumeric("lb", penv, stateOfCheck) 
stateOfCheck<-xegaCheckFunctionValueLengthNotZero("lb", penv, stateOfCheck) 

t1<-xegaGetPenvVal("bitlength", penv, stateOfCheck)
t2<-xegaGetPenvVal("lb", penv, stateOfCheck)
stateOfCheck<-xegaCheckCondition("length(lb)==length(bitlength)",
                                (length(t1$value)==length(t2$value)), 
                                stateOfCheck) 

### function ub.
stateOfCheck<-xegaCheckFunctionExists("ub", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionWithoutArguments("ub", penv, stateOfCheck)
stateOfCheck<-xegaCheckFunctionValueIsNumeric("ub", penv, stateOfCheck) 
stateOfCheck<-xegaCheckFunctionValueLengthNotZero("ub", penv, stateOfCheck) 

t2<-xegaGetPenvVal("ub", penv, stateOfCheck)
stateOfCheck<-xegaCheckCondition("length(ub)==length(bitlength)",
                                (length(t1$value)==length(t2$value)), 
                                stateOfCheck) 

t1<-xegaGetPenvVal("lb", penv, stateOfCheck)

stateOfCheck<-xegaCheckCondition("all(ub >lb)",
                                (all(t2$value>t1$value)), 
                                stateOfCheck) 

### function f: The fitness function
stateOfCheck<-xegaCheckFunctionExists("f", penv, stateOfCheck)

fit<-formals(penv$f)
if (!all(names(penv$f) %in% c("parm", "gene", "lF")))
   {
    cat("f<-function(parm, gene=0, lF=0) #  \n")
    cat("# A fitness function for xegaRun needs three arguments: \n")  
    cat("# parms: The parameter vector. \n")  
    cat("# gene=0: Not used in xegaRun(algorithm=\"sga\"). \n")  
    cat("# lF=0: Not used in xegaRun(algorithm=\"sga\"). \n")  
    stateOfCheck$Fail<-stateOfCheck$Fail+1
    }
else
{
    stateOfCheck$Passed<-stateOfCheck$Passed+1
}

stateOfCheck<-xegaCheckFitnessFunction(penv, stateOfCheck)

###
if (0==stateOfCheck$Failed) 
  {cat("Problem environment passed ", stateOfCheck$Passed, "tests.\n")}
else
  {cat("Interface check for penv failed.\n") 
   cat("Failed:", stateOfCheck$Failed, "Passed:", stateOfCheck$Passed, ".\n") }
}

