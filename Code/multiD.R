## ----setup, include=FALSE-----------------------
# knitr::opts_chunk$set(echo = TRUE)

rm(list = ls())
# require(tidyverse)
library(here)

i_am("Code/multiD.Rmd")
source(here("Code", "utils.R"))
config <- config::get()

args <- commandArgs(TRUE)
parameters <- as.numeric(args)
n = parameters[1]
sd = parameters[2]
seed = parameters[3]

evalHere <- function(x) {  # since we're going to be using this heavily. A wrapper
  if(!is.character(x)) stop("x should be a string")
  return(base::eval(base::parse(text=x)))
}


## ----model--------------------------------------
set.seed(seed)

n = n
functionName <- "DGP1"
print(formals(functionName))

modelVals = modelSp(functionName, n = n, sd = sd) # from utils # x returning as matrix, fx as list
xargs = modelVals$xargs
x = modelVals$x
fx = modelVals$fx

xDim = min(dim(x))
for (i in 1:xDim) { # assign each row to x1, x2 ...>
  assign(f("x{i}"), x[i, ] |> unlist())
}



## -----------------------------------------------
# plot(x1, fx)


## ----kernelBuilder------------------------------

bernoulliKernel <- bernoulliKernel
lambda = 1e-5 # we will give a constant for now. Later we'll optimize
I = diag(1, nrow = n)
Rkernel = c()

for (i in 1:xDim) { # R = list(R1, R2)
  xi = evalHere(f("x{i}"))
  print(f("Calculating outer Rkernerl for <x{i}, x{i}>"))
  assign(f("Rkernel{i}"), outer(xi, xi, bernoulliKernel))
  Rkernel[f("Rkernel{i}")] = evalHere(f("Rkernel{i}")) |> list()
}

print(names(Rkernel))


## ----kernel and R-------------------------------
mprod <- function(kernel, xdim, name, I) {
  R <- matrix(rep(1, n*n), nrow = n)
  for (i in 1:xdim) {
    R = R * getElement(kernel, f("{name}{i}"))
  }
  return(R)
}

R = mprod(kernel = Rkernel, xdim = xDim, name = "Rkernel", I = I)


## ----fHat---------------------------------------
coef <- (R + n*lambda*I) |> GInv()
coef <- coef %*% matrix(fx)

phi <- R

fHat <- {t(phi) %*% coef } |> c()


## ----metrics------------------------------------
fHat_rmse = Metrics::rmse(fHat, fx)
print(f("Calculated RMSE: {round(fHat_rmse, 2)}"))

# write the metrics to a file
valsList = list(n = n, sd = sd, seed = seed, rmse = fHat_rmse)
file_name = format(Sys.time(), "%b_%d_%Y") |> as.character()
readr::write_csv(as.data.frame(valsList), file = here("Data", "multiDimension_{file_name}.csv"), append = TRUE, col_names = TRUE)

print(f("File saved: {config$path_to_data}{.Platform$file.sep}multiDimension_{file_name}.csv"))


## ----visualizex1--------------------------------
# require(dplyr)
# require(ggplot2)
# 
# x <- x1
# dfOri <- tibble(x, fx)
# dfFit <- tibble(x, fHat)
# dfOri %>% 
#   left_join(dfFit, by='x') %>%  # get the corresponding fitted value to x
#   reshape2::melt(id='x') %>% # flatten the table so that we can plot as one variable but two groups
#   ggplot() + 
#   geom_point(aes(x=x, y=value, col=variable)) + 
#   labs(title = "(x1,fx) and (x1, FHat)")
# 
# 
# ## ----visualizex2--------------------------------
# x <- x2
# dfOri <- tibble(x, fx)
# dfFit <- tibble(x, fHat)
# dfOri %>% 
#   left_join(dfFit, by='x') %>%  # get the corresponding fitted value to x
#   reshape2::melt(id='x') %>% # flatten the table so that we can plot as one variable but two groups
#   ggplot() + 
#   geom_point(aes(x=x, y=value, col=variable)) + 
#   labs(title = "(x2, fx) and (x2, FHat)")
# 

## -----------------------------------------------
print("done with this")

