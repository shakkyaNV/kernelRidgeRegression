## Util Functions for other codes
suppressPackageStartupMessages(require(magrittr, quietly = TRUE))
suppressPackageStartupMessages(require(rlang, quietly = TRUE))
# suppressPackageStartupMessages(require(numbers, quietly = TRUE))

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
          call2(., x = x) %>% eval()) * # call2 creates a function call, eval evaluates
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
    stop("Input X must be a matrix.")
  }
  if (min(dim(x)) != 2) {
    stop(f("Input Matrix not of correct Dimensiom. Input dim: {dim(x)}"))
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


DBETA <- function(x, b = 0, numXArgs = 1) {
  if(!is.matrix(x)){
    stop("Input X must be a matrix")
  }
  
  numXArgs = min(dim(x))
  for (i in 1:numXArgs) {
    assign(paste0("x", i), x[i, ]  %>% unlist())
  }
  
  g01 <- function(x1){
    return(dbeta(x1, 5, 20))
  }
  g02 <- function(x1) {
    return(dbeta(x1, 30, 5))
  }
  
  val = g01(x1) + g02(x1) + b*(g01(x1) * g02(x1))
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
  x = sort(unlist(x))
  x <- matrix(x, nrow = xargs, byrow = TRUE) # num_x_args x n matrix
  fx <- rlang::call2(functionName, x, ...)  %>% 
    eval()
  
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

## zip equivalent for python

zip <- function(...) {
  return(mapply(list, ..., SIMPLIFY = FALSE))
}



generate_binary_combinations <- function(n) {
  n = evalHere(f("{n}L"))
  
  # Checks
  if ((!is.integer(n)) | (length(n) != 1)) {
    stop("n should be a vector of length 1. Integer values only")
  }
  
  if (n == 0) {
    return(matrix(0, 1, 1))
  }
  else if (n == 1) {
    return(matrix(c(0, 1), 2, 1))
  }
  else {
    previous_combinations <- generate_binary_combinations(n - 1)
    heads <- cbind(previous_combinations, 0)
    tails <- cbind(previous_combinations, 1)
    return(rbind(heads, tails))
  }
}




polyDegree <- function(xargs, npoly) {
  # This function should output a vector in accordance with
  # input xargs.
  # Eg: if xargs = 2
  # polynomial would be (1 + x1 + x2 + x1x2)
  # respective degrees are (0 degree, 1st degree, 1st degree, 2nd degree)
  # output (1, 1, 1, 0) ==> means we have silenced 2nd order degree interactions
  xargs = evalHere(f("{xargs}L"))
  
  # Checks
  if ((!is.integer(xargs)) | (length(xargs) != 1)) {
    stop("xargs should be a vector of length 1. Integer values only")
  }
  polyTerms <- numbers::pascal_triangle(xargs)[xargs + 1, ] ## pascal triangle row for xargs degree of polyn. Eg: xargs = 2.... 1 2 1
  nTerms    <- sum(polyTerms)            ## Number of all terms is equal to sum Eg: xargs = 2... Total terms = 4 [1 + x1 + x2 + x1x2]
  nDegree <- length(polyTerms)           ## number of degree. For above example this should be 3. 0th order (1), 1st order( x1 and x2) , 2nd order(x1x2)
  
  if (missing(npoly)) {
    npoly = rep(1, nDegree)
  }
  
  if (!is.vector(npoly) | (length(npoly) != nDegree) | any(!(unique(npoly) %in% c(0, 1))) ) {
    stop("npoly should either be a vector of respective length for xargs. Each element should be zero or one")
  }
  
  # Calc
  zipped <- zip(polyTerms, npoly)
  res = c()
  
  for (i in 1:nDegree) {
    res = append(res, rep(zipped[[i]][[2]], zipped[[i]][[1]]))
  }
  
  return(list(res=res, poly=polyTerms))
}












