## Util Functions for other codes
suppressPackageStartupMessages(require(magrittr, quietly = TRUE))
suppressPackageStartupMessages(require(rlang, quietly = TRUE))

## Utils
GInv <- MASS::ginv
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

## Kernels
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


## Data Generation Process

SIN <- function(x, power = 8, numXArgs = 1) {
  n = max(dim(x))
  x1 = x[1, ]  %>% unlist()
  # x2 = x[2, ] |> unlist()
  y = sin(x1*(power)) # + sin(x2*(power/2))
  return(y)
}

DGP1 <- function(x, q, beta, b = 1, numXArgs = 2) { 
  # we can ignore q, beta, b for now because of lazy eval. 
  # It won't be evaluated (since it's not needed) and hence 
  # no question if you don't pass it in to the function
  # check
  if(!is.matrix(x)){
    stop("Input X must be a matrix [2x100]")
  }
  if (min(dim(x)) != 2) {
    stop(f("Input Matrix not of correct Dimension: [2x100]. Input dim: {dim(x)}"))
  }
  if (dim(x)[1] != 2) {
    x = t(x)
  }
  
  numXArgs = min(dim(x))
  for (i in 1:numXArgs) {
    assign(paste0("x", i), x[i, ]  %>% unlist())
  }
  
  g01 <- function(x1) {
    return(exp(-4*(1-2*((x1)^2)))*(1-(2*(x1))))
  }
  g02 <- function(x2) {
    return(sin(8*x2) + cos(8*x2) + log(4/3 + x2))
  }
  g03 <- function(x1, x2) {
    return(1.5*exp(x1 + x2))
  }
  
  val = g01(x1) + g02(x2) + b*g03(x1, x2)
  return(val)
}

DGP2 <- function(x, q, beta, b = 1, numXArgs = 5) {
  # we can ignore q, beta, b for now because of lazy eval. 
  # It won't be evaluated (since it's not needed) and hence 
  # no question if you don't pass it in to the function
  # check
  if(!is.matrix(x)){
    stop("Input X must be a matrix [2x100]")
  }
  if (min(dim(x)) != 5) {
    stop(f("Input Matrix not of correct Dimension: [2x100]. Input dim: {dim(x)}"))
  }
  if (dim(x)[1] != 5) {
    x = t(x)
  }
  
  numXArgs = min(dim(x))
  for (i in 1:numXArgs) {
    assign(paste0("x", i), x[i, ]  %>% unlist())
  }
  
  g01 <- function(x1) {
    return(5*x1)
  }
  g02 <- function(x2) {
    return(3*((2*x2 - 1)^2))
  }
  g03 <- function(x3) {
    return({4 * sin(2*pi*x3)} / {2 - sin(2*pi*x3)})
  }
  g04 <- function(x4) {
    return(2*x4^3 + min(x4, 0.2) + max(x4, 0.8))
  }
  g05 <- function(x5) {
    return({0.6*sin(2*pi*x5)} + {1.2*cos(2*pi*x5)} +
           {1.8*(sin(2*pi*x5)^2)} + {2.4*(cos(2*pi*x5)^3)} + 
           {3*(sin(2*pi*x5)^3)})
  }
  val = b*g01(x1) + g02(x2) + g03(x3) + g04(x4) + g05(x5)
  return(val)
}


## Pull data into code with
modelSp <- function(functionName, n, ...) {
  # get the function name from input and determine the x1, x2,,, dimension on its own
  # and creates x by runif within function, and passes to determinstic function
  # returns input x, y as matrix and num of x needed for the specified function 
  # so that it can be broken down later after output
  
  x <- list()
  xargs = formals(functionName)$numXArgs #we specify numXargs when making function in utils. And then pull that value here via formals
  for (arg in 1:xargs) {
    x <- append(x, runif(n))
  }
  
  x <- matrix(x, ncol = n) # num_x_args x n matrix
  fx <- rlang::call2(functionName, x, ...)  %>% 
    eval
  fx <- fx
  return(list(x=x, fx=fx, xargs=xargs))
}

mprod <- function(kernel, xdim, name, I) {
  R <- matrix(rep(1, n*n), nrow = n)
  for (i in 1:xdim) {
    R = R * getElement(kernel, f("{name}{i}"))
  }
  return(R)
}


evalHere <- function(x) {  # since we're going to be using this heavily. A wrapper
  # This function force evaluate the glued content
  if(!is.character(x)) stop("x should be a string")
  return(base::eval(base::parse(text=x)))
}


















