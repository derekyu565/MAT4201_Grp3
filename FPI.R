library(NLRoot)

func <- function(x) {
  (exp(1))^x + x - 6
}

curve(func, xlim=c(-3,3), col='blue', lwd=1.5, lty=2)
abline(h=0)
abline(v=0)
uniroot(func,c(1,2))

fixedpoint <- function(fun, x, tol=1e-07, n=500){
  ## fixed-point algorithm to find x such that fun(x) == x
  ## assume that fun is a function of a single variable
  ## x0 is the initial guess at the fixed point
  
  xold <- x
  xnew <- fun(xold)
  for (i in 1:n) {
    xold <- xnew
    xnew <- fun(xold)
    if ( abs((xnew-xold)) < tol ){
      return(xnew) 
    }
  }
  stop("exceeded allowed number of iterations")
}

fixedpoint(func, 1)
