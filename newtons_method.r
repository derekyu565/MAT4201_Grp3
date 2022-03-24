require('calculus')
library(spuRs)
library(Ryacas)

E <- function (...) {eval(parse(text=paste(...,collapse=" ")))}
wrapper <- function(s){E("function(x){",s,"}")}


#y=parse(text=expression(x^2-2))
y=parse(text=expression(x^7-500))
#y=parse(text=expression(x^2-pi))
#y=parse(text=expression(x^9-1000000))
#y=parse(text=expression(x^6-900000))
#y=parse(text=expression(x^5-25))
#y=parse(text=expression(x^3-(3*x^2)+(10*x)+5))
#y=parse(text=expression(x^3-exp(1)))
#y=parse(text=expression(exp(x)+x-6))

#fx is now a function
fx=wrapper(y)

parsedDerivative=parse(text=derivative(y,"x"))
#dfx now becomes a function
dfx=wrapper(parsedDerivative)

graphing <- function(someFunction,neg,pos){
  curve(someFunction, xlim=c(neg,pos), col='blue', lwd=1.5, lty=2)
  abline(h=0)
  abline(v=0)
}

graphing(fx,-12,12)
uniroot(fx,c(-12,5))

newtons_method <- function(x) {
  threshold <- 0.0000005
  # just to make sure that it wont be greater than or equal to x
  prev_result <- x - 1
  ctr <- 1
  while(TRUE){
    x <- x - (fx(x) / dfx(x))
    print(paste("Previous result= ",prev_result))
    print(paste("x= ",x))
    if (abs(x - prev_result) <=  threshold){
      break
    }
    prev_result <- x
    ctr <- ctr + 1
  }
  print(paste("FINAL ANSWER: ",x))
  return(x)
}

fx(newtons_method(10000000000000))

