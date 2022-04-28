library(NLRoot)
library(spuRs)
library("SciViews")
library(pracma)

poly.neville <- function(x, y, x0) {
  
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- ((x0 - x[j-i+1]) * q[j,i-1] - (x0 - x[j]) * q[j-1,i-1]) / (x[j] - x[j-i+1])
    }
  }
  
  res <- list('Approximated value'=q[n,n], 'Neville iterations table'=q)
  return(res)
}

x <- c(2, -2, -3, 1)
y <- c(-2, 5, 7, 1)

poly.neville(x, y, 1.7)
neville(x, y, 1.7)