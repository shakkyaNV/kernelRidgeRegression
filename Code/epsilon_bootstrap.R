rm(list = ls())
library(here)
suppressMessages(source(here("Code", "utils.R")))
suppressMessages(source(here("Code", "gcv.R")))
config <- config::get()


##################### UNSCALED ORIGINAL PROCEDURE ##############################
n <- config$sample_size
sd <- config$sd
b <- config$b
B <- config$B
seed <- config$seed
alpha <- config$alpha
# B = 100

set.seed(Sys.time())

functionName <- "DGP1"

modelVals = modelSp(functionName, n = n)
xargs = modelVals$xargs
x = modelVals$x
fx = modelVals$fx
y = {b*fx} + rnorm(n, mean = 0, sd = sd)


bernoulliKernel <- bernoulliKernel

mprodId <- function(kernel) {
  xdim = length(kernel)
  n = dim(kernel[[1]])[1]
  R <- matrix(rep(1, n*n), nrow = n)
  for (i in 1:xdim) {
    R = R * kernel[[i]]
  }
  return(R)
}

calcFHat <- function(x, fx, kernel, wmat=0) {
  n = max(dim(x))
  xDim = min(dim(x))
  if (missing(wmat)) {
    wmat = diag(1, nrow = n)
  }
  
  xi = c()
  
  for (i in 1:xDim) { # assign each row to x1, x2 ...>
    xi[[i]] = x[i, ] %>% unlist()
  }
  
  lambda = gcvMain(x = x, fx = fx) # optimized according to GCV function
  # log_info("Best fit is given with lambda value: {lambda}")
  I = diag(1, nrow = n)
  
  R <- c()
  for (i in 1:xDim) { # R = list(R1, R2)
    # log_info(f("Calculating outer R_tilda kernel for <x{i}, x{i}>"))
    R[i] = outer(xi[[i]], xi[[i]], bernoulliKernel) %>% list()
  }
  
  
  R <- mprodId(kernel = R)
  coef <- { (wmat %*% R) + n*lambda*I} %>% GInv()
  coef <- coef %*% matrix(wmat %*% y)
  
  phi <- R
  
  fHat <- {t(phi) %*% coef } %>% c()
  return(fHat)
}

fHat_original <- calcFHat(x, fx, bernoulliKernel)

phi_n_original = max(abs(fHat_original))

####################### BOOSTRAP PROCEDURE (WITH W) ############################

phi_n_star = c()
w = c()
fHat = c()
fHat_weighted = c()

for (i in 1:B) {
  message(paste0("Starting Iteration ", i, "/", B))
  ## Not sure about calc from here
  modelVals_i = modelSp(functionName, n = n)
  xargs_i = modelVals_i$xargs
  xip = modelVals_i$x
  fxip = modelVals_i$fx
  yi = {b*fxip} + rnorm(n, mean = 0, sd = sd)
  
  # to here (why is this necessary? What is P? Is it different from n?)
  
  wi = rexp(n, rate = 1)
  w[[i]] <- wi
  wmat = diag(wi)
  
  fHat[[i]] = calcFHat(xip, fxip, bernoulliKernel)
  fHat_weighted[[i]] = calcFHat(xip, fxip, bernoulliKernel, wmat)
  
  phi_n_star[[i]] = max(abs( fHat_weighted[[i]] - fHat[[i]] ))
  
}


ptile.t = quantile(x=unlist(phi_n_star), probs=1-(alpha/2), names=FALSE)
p.value = sum(unlist(phi_n_star) > phi_n_original) / B

message(paste0("ptile.t value: ", ptile.t))
message(paste0("p.value is: ", p.value))

message(paste0("Compare inequality: ", phi_n_original, " (phi_original)", "  ??  ", ptile.t, " (upper alpha t-quantile)"))
message(paste0("H0: b=0 is: ", if(phi_n_original > ptile.t) "Rejected" else "Not Rejected"))






########## H0 didn't get rejection in B=100, 10 (ran about 5 times in this variation)
















