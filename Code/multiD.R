## ----setup, include=FALSE-----------------------
# knitr::opts_chunk$set(echo = TRUE)

rm(list = ls())
# n=100;sd=0.01;seed=12;jobid=010;jobname="testAll"
args <- commandArgs(TRUE)
parameters <- as.numeric(args[1:5])
n = parameters[1]
sd = parameters[2]
seed = parameters[3]
jobid = parameters[4]
runTimeName = parameters[5]
jobname = args[6]

here::i_am("Code/multiD.R")
library(here, quietly = TRUE)
library(logger, quietly = TRUE)
source(here("Code", "utils.R"))
source(here("Code", "gcv.R"))
config <- config::get()

date_time = format(Sys.time(), "%H_%M_%b_%d_%Y")
# file_name = f("{jobname}_{jobid}_{date_time}")
file_name = f("{runTimeName}")
log_appender(appender_tee(here("Logs", f("{file_name}.log"))))


## ----model--------------------------------------
log_info("-----------------------------------------------------")
log_info(f("Process Starting: multiDimensional Kernel Regression Metrics Evaluation"))
log_info(f("Parameters Received: {parameters}") %>% skip_formatter())

set.seed(seed)

n = n
functionName <- "DGP1"

modelVals = modelSp(functionName, n = n) # from utils # x returning as matrix, fx as list
xargs = modelVals$xargs
x = modelVals$x
fx = modelVals$fx
y = fx + rnorm(n, mean = 0, sd = sd)

xDim = min(dim(x))
for (i in 1:xDim) { # assign each row to x1, x2 ...>
  assign(f("x{i}"), x[i, ] %>% unlist())
}



## -----------------------------------------------
# plot(x1, y)


## ----kernelBuilder------------------------------

bernoulliKernel <- bernoulliKernel
lambda = gcvMain(x = x, fx = fx) # optimized according to GCV function
log_info("Best fit is given with lambda value: {round(lambda, 3)}")
I = diag(1, nrow = n)
Rkernel = c()

for (i in 1:xDim) { # R = list(R1, R2)
  xi = evalHere(f("x{i}")) 
  log_info(f("Calculating outer Rkernerl for <x{i}, x{i}>"))
  assign(f("Rkernel{i}"), outer(xi, xi, bernoulliKernel))
  Rkernel[f("Rkernel{i}")] = evalHere(f("Rkernel{i}")) %>% list()
}

## ----kernel and R-------------------------------

R = mprod(kernel = Rkernel, xdim = xDim, name = "Rkernel", I = I)


## ----fHat---------------------------------------
coef <- (R + n*lambda*I) %>% GInv()
coef <- coef %*% matrix(y)

phi <- R

fHat <- {t(phi) %*% coef } %>% c()


## ----metrics------------------------------------
fHat_rmse = Metrics::rmse(fHat, y)
log_info(f("Calculated RMSE: {round(fHat_rmse, 2)}"))

# write the metrics to a file
valsList = list(n = n, sd = sd, seed = seed, rmse = fHat_rmse)
readr::write_csv(as.data.frame(valsList), file = here("Data", f("{file_name}.csv")), append = TRUE, col_names = FALSE)

log_info(f("File saved: {as.character(here('Data'))}{.Platform$file.sep}{file_name}.csv"))
log_info(f("File has colnames: {names(valsList)}") %>% logger::skip_formatter())

## ----visualizex1--------------------------------
# require(dplyr)
# require(ggplot2)
# 
# x <- x1
# dfOri <- tibble(x, y)
# dfFit <- tibble(x, fHat)
# dfOri %>% 
#   left_join(dfFit, by='x') %>%  # get the corresponding fitted value to x
#   reshape2::melt(id='x') %>% # flatten the table so that we can plot as one variable but two groups
#   ggplot() + 
#   geom_point(aes(x=x, y=value, col=variable)) + 
#   labs(title = "(x1,y) and (x1, FHat)")
# 
# 
# ## ----visualizex2--------------------------------
# x <- x2
# dfOri <- tibble(x, y)
# dfFit <- tibble(x, fHat)
# dfOri %>% 
#   left_join(dfFit, by='x') %>%  # get the corresponding fitted value to x
#   reshape2::melt(id='x') %>% # flatten the table so that we can plot as one variable but two groups
#   ggplot() + 
#   geom_point(aes(x=x, y=value, col=variable)) + 
#   labs(title = "(x2, y) and (x2, FHat)")
# 

## -----------------------------------------------
log_info("Code Finalized")

