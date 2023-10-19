here::i_am("Code/testInteractions.R")
library(here)
source(here("Code", "utils.R"))
pascalT <- numbers::pascal_triangle

## Model Specifications
functionName = "DGP1"
n = 100
sd = 0.02

model <- modelSp(functionName, n = n)
xargs = model$xargs
x = model$x
fx = model$fx
y = fx + rnorm(n, mean = 0, sd = sd)

xDim = as.integer(model$xargs)
for (i in 1:xDim) { # assign each row to x1, x2 ...>
  assign(f("x{i}"), x[i, ] %>% unlist())
}


## Kernel Specification

bernoulliKernel <- bernoulliKernel
lambda = 1e-5 # optimized according to GCV function
I = diag(1, nrow = n)
R = c()

for (i in 1:xDim) { # R = list(R1, R2)
  
  xi = evalHere(f("x{i}")) 
  assign(f("Rkernel{i}"), outer(xi, xi, bernoulliKernel))
  R[f("Rkernel{i}")] = evalHere(f("Rkernel{i}")) %>% list()
}


polyTerms <- pascalT(xargs)[xargs+1, ] 


polyDegree <- function(xargs, npoly) {
  # This function should output a vector in accordance with
  # input xargs.
  # Eg: if xargs = 2
  # polynomial would be (1 + x1 + x2 + x1x2)
  # respective degrees are (0 degree, 1st degree, 1st degree, 2nd degree)
  # output (1, 1, 1, 0) ==> means we have silenced 2nd order degree interactions
  require(numbers)
  xargs = evalHere(f("{xargs}L"))
  
  # Checks
  if ((!is.integer(xargs)) | (length(xargs) != 1)) {
    stop("xargs should be a vector of length 1. Integer values only")
  }
  polyTerms <- pascal_triangle(xargs)[xargs + 1, ] ## pascal triangle row for xargs degree of polyn. Eg: xargs = 2.... 1 2 1
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

# out = polyDegree(5, c(1, 1, 1, 0, 0, 0)) #0th, 1st order and 2nd order interactions only
out = polyDegree(2, c(1, 1, 0))
res = out$res
poly = out$poly 


df = generate_binary_combinations(xargs) %>% 
  as.data.frame()
df = cbind(df, degree = rowSums(df), res = res)
df = df[order(df$degree, decreasing = FALSE), ]
df = df[, c(1,2,4)]

len = length(poly)

# OUTER FOR -- zipped(each row of df, (x1, x2))
# assign power to each and add them all up
######## WORKING
val1 = 0
R1 = matrix(seq(1, 9), 3) ;R2 = diag(1, 3)
for (i in 1:sum(poly)) {
  degrees = df[i, c(1,2)] %>% unlist()
  ifsilence = df[i, 3] %>% unlist()
  zipped = zip(degrees, c(R1, R2))
  val2 = 1
  for (j in 1:xargs) {
    val2 = ifsilence * val2 * evalHere(f("R{j}"))^zipped[[j]][[1]]
  }
  val1 = val1 + val2
  print(val1)
}

################# WORKING

##### HOW TO GENERATE EACH INPUT COMBINATIONS OF RES

# let's assume we have list as 
# [R1, R2]
# we need to make: [1-R1_tilda, 1-R2_tilda]
# We ultimately need [1 + R1_tilda + R2_tilda + R1_tilda x R2_tilda]
# let's assume first list we enter [R1, R2] are in order 1, 2
# then

R = R #(input list of matrix arrays to R)
str = "1 +"


df = generate_binary_combinations(3)

for (i in 1:nrow(df)) {
  poly = df[i, ] %>% as.numeric();print(poly);
  res = polyDegree(2, poly)
  print(res$poly)   ## pascal triangle row
  print(res$res)    ## each element 1, 0
  print("---")
}































