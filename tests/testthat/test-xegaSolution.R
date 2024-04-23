
library(testthat)
library(xega)
library(xegaDiag)

a<-xegaRun(Parabola2D,
           algorithm="sga", 
           generations=10, 
           profile=TRUE, 
           verbose=0, 
           replay=5)

test_that("xegaPrintSolution OK", 
{
expect_output(xegaPrintSolution(a), "Max   Parabola2D")
expect_output(xegaPrintSolution(a), "Fitness:")
expect_output(xegaPrintSolution(a), "Parameters:")
expect_output(xegaPrintSolution(a), "Time used:")
}
)

