library(NLRoot)

func <- function(x) {
  #(x^3)-(3*x^2)+10*x+5#number 1 [-1,6]
  #x^2+(x/x+1)-3#number 2 [-2,0] number 3 [0,2]
  x^5+(4*x^4)-(2*x^3)+(3*x^2)-x+6 #number 4 [-10,0]
  #x^4-x^2 #number 5 [-2,0] number 6 [0,2]
}
func(-4)

graphing <- function(someFunction,neg,pos){
  curve(someFunction, xlim=c(neg,pos), col='blue', lwd=1.5, lty=2)
  abline(h=0)
  abline(v=0)
}


graphing(func,-10,0)

uniroot(func,c(-10,0))

bisection <- function(f, a, b, n = 1000, tol = 1e-7) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.
  if (!(f(a) < 0) && (f(b) > 0)) {
    stop('signs of f(a) and f(b) differ')
  } else if ((f(a) > 0) && (f(b) < 0)) {
    stop('signs of f(a) and f(b) differ')
  }
  
  for (i in 1:n) {
    c <- (a + b) / 2 # Finding midpoint between intervals A and B
    var <- (a+b)/2
    print(paste("Iteration= ",i))
    print(paste("Interval A= ",a))
    print(paste("Interval B= ",b))
    print(paste("Midpoint= ",var))
    print(var)
    
   
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
    # function and return the root.
    if ((f(c) == 0) || ((b - a) / 2) < tol) {
      return(c)
    }
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
    
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  print('Too many iterations')
}

bisection(func,-5,-4)


