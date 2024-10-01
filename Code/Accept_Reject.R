# here::i_am("Code/Accept_Reject.R")
# library(here)
require(magrittr)
require(here)

# target function
f <- function(x) {
  # if (all((x<1) && (x>0))) {
    s = dbeta(x, 5, 20) + dbeta(x, 30, 5)
    return(s)
  # }
  # else{return(0)}
}

p <- function(x){
  x %>% 
    f() %>% 
    exp() %>% 
    return()
}

# proposed function
g <- function(x) {
  return(dunif(x))
}

# optimize over
h <- function(x) {
  return(p(x)/g(x))
}

#A-R
sim.fun <- function(n) {
  # insert error checking
  if ( (!is.numeric(n)) | (!length(n) == 1L) |  (!all(n>1)) ) {
    stop("Input `n` should be a numeric scalar > 1")
  }
  n = floor(n)
  
  M = optimize(h, interval=c(0, 1), maximum=TRUE)
  M = M$objective
  
  y = vector("numeric", n)
  numIter = 0
  for (i in 1:n) {
    success = F
    while (!success) {
      numIter = numIter + 1
      u = runif(1)
      x = runif(1)
      
      condition = p(x)/ {M*g(x)}
      if (condition > u) {
        success <- TRUE
      }
    }
    y[i] <- x
  }
  return(list(realizations=y, obj=M, avg.Iter=numIter/n))
}

testAR <- function(n) {
  y = sim.fun(n)
  x = seq(0, 1, length.out=n)
  
  
  # Histogram of P(x)
  hist(y$realizations, prob=T, breaks=50, 
       main = "Histogram of p(x): (Blue is AR)")
  lines(x, log(p(x)), col = "blue", lwd=2)
  
  newx = seq(0, 1, length = 100)
  
  d = density(y$realizations, from=0, to=1, bw=0.02)
  fd = approx(d$x, d$y, xout=newx)
  fhat = fd$y %>% log()
  
  f0 = f(newx)
  
  # Plot of after converting p(x) to f(x)
  plot(fd$x, fhat-mean(fhat), col = "black", type="l")
  lines(newx, f0 - mean(f0), lwd=2, col = "blue")
  title("Plot of f(x): (Blue is AR)")
}




