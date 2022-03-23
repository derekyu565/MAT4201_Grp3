library(NLRoot)
library(spuRs)
library("SciViews")

func <- function(x){
  #x^3-(3*x^2)+10*x+#number 1
  #x^2+(x/(x+1))-3#number 2
  #(x^4/(x+1))-6#number 3
  #sin(((2*x)/3)+1)+x#number 4
  #cos(((2*x)/3)+1)+x#number 5
  tan(((2*x)/3)+1)+x#number 6
  #exp(x)+x-6#number 7
  #exp(x^2)-2*x-5#number 8
}

func1 <- function(x) {
  #(-x^3+(3*x^2)-5)/10#number 1
  #sqrt(-x/(x+1)+3)#number 2
  #((6*x)+6)^(1/4)#number 3
  #-sin(((2*x)/3)+1)#number 4
  #-cos(((2*x)/3)+1)#number 5
  -tan(((2*x)/3)+1)#number 6
  #ln(6-x)#number 7
  #number 8
}

graphing <- function(someFunction,neg,pos){
  curve(someFunction, xlim=c(neg,pos), col='blue', lwd=1.5, lty=2)
  abline(h=0)
  abline(v=0)
}
graphing(func,-2.5,2.5)

uniroot(func,c(-3,3))
fixedpoint2 <- function(fun, x0, tol=1e-07, niter=500){
  ## fixed-point algorithm to find x such that fun(x) == x
  ## assume that fun is a function of a single variable
  ## x0 is the initial guess at the fixed point
  
  xold <- x0
  xnew <- fun(xold)
  for (i in 1:niter) {
    xold <- xnew
    xnew <- fun(xold)
    if ( abs((xnew-xold)) < tol )
      return(xnew)
  }
  stop("exceeded allowed number of iterations")
}

fixedpoint(func1, -2.5, tol = 1e-6)
fixedpoint2(func1,-1)
func(fixedpoint2(func1,1))
