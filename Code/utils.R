## Util Functions for other codes

## Utils
f <- glue::glue


## Bernoulli Kernel
b0 <- function(x) {
  return(rep(1, n))
}
b1 <- function(x) {
  return(x - 1/2)
}
b2 <- function(x) {
  return(x^2 - x + 1/6)
}
b3 <- function(x) {
  return(x^3 - 3/2*x^2 + 1/2*x)
}
b4 <- function(x) {
  return(x^4 - 2*x^3 + x^2 - 1/30)
}


bernoulliKernel <- function(x, y, m=2) {
  # Check
  if (length(x) != length(y)) {
    stop("x and y should be of same length")
  }
  if (m > 2) {
    stop("Bernoulli Kernel defined only upto 4: (4=m*2)")
  }
  rKernel = 1
  for (v in 1:m) { 
    textToEval <- paste0("b", v) # create the function name
    rKernel = rKernel + 
      ((textToEval %>%
          call2(., x = x) %>% eval()) + # call2 creates a function call, eval evaluates
         (textToEval %>%
            call2(., x = y) %>% eval())) / (factorial(v) ^ 2)
  } 
  
  lastCall = paste0("b", 2*m) %>% call2(., x=abs(x-y)) 
  rKernel = rKernel + ((-1) ^ (m - 1)) * ((lastCall %>% eval()) / (factorial(2 * m)))
  return(rKernel)
}