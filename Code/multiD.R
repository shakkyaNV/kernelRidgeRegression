## ----setup, include=FALSE-----------------------
# knitr::opts_chunk$set(echo = TRUE)

rm(list = ls())
n=100;sd=0.01;seed=12;jobid=010;jobname="testAll";runTimeName="all_test"
# args <- commandArgs(TRUE)
# parameters <- as.numeric(args[1:4])
# n = parameters[1]
# sd = parameters[2]
# seed = parameters[3]
# jobid = parameters[4]
# runTimeName = args[5]
# jobname = args[6]

here::i_am("Code/multiD.R")
suppressPackageStartupMessages(library(here, quietly = TRUE))
suppressPackageStartupMessages(library(logger, quietly = TRUE))
source(here("Code", "utils.R"))
source(here("Code", "gcv.R"))
config <- config::get()

date_time = format(Sys.time(), "%H_%M_%b_%d_%Y")
# file_name = f("{jobname}_{jobid}_{date_time}")
file_name = f("{runTimeName}")
if (.Platform$GUI == "RStudio") {
  appender = appender_tee(here("Logs", f("{file_name}.log")))
} else {
  appender = appnder_file(here("Logs", f("{file_name}.log")))
}
log_appender(appender)


## ----model--------------------------------------
log_info("-----------------------------------------------------")
log_info(f("Process Starting: multiDimensional Kernel Regression Metrics Evaluation"))
# log_info(f("Parameters Received: {parameters}") %>% skip_formatter())

set.seed(seed)

n = n
functionName <- "DGP2"

modelVals = modelSp(functionName, n = n, b = 1) # from utils # x returning as matrix, fx as list
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
# lambda = 0.0001544701 ## DGP2
# lambda = 0.0001544701 ## DGP1

bernoulliKernel <- bernoulliKernel
lambda = 0.0001544701 # gcvMain(x = x, fx = fx) # optimized according to GCV function
log_info("Best fit is given with lambda value: {round(lambda, 3)}")
I = diag(1, nrow = n)
R_tilda = c()

for (i in 1:xDim) { # R = list(R1, R2)
  
  xi = evalHere(f("x{i}")) 
  log_info(f("Calculating outer R_tilda kernel for <x{i}, x{i}>"))
  R_tilda[i] = { 1- outer(xi, xi, bernoulliKernel) } %>% list()
}

## ----kernel and R-------------------------------

generate_combs <- function(n) {
  df = generate_binary_combinations(n) %>% as.data.frame()
  df = cbind(df, sumDegree = rowSums(df))
  df = df[order(df$sumDegree, decreasing = FALSE), ]
  df = df[, -ncol(df)]
  return(df)
}


# Rkernel = c()
# R1 = matrix(seq(1, 9), 3) ;R2 = diag(1, 3)
# R_tilda = list(R1, R2)
# R_tilda = c(2, 3, 10)

degree_df = generate_combs(xargs + 1) ## degree combinations
term_df   = generate_combs(xargs)     ## term combinations

eqn = c()
rmse_list = c()


####### ADD Explanation --- LOOKS LIKE WORKING

for (i in 1:nrow(degree_df)) {                ## Iterate through over all combinations
  comb = degree_df[i, ] %>% as.numeric()      ## eg: 1 1 0
  log_info(f("Calculating Interactional RMSE for term_{i}: {comb}") %>% skip_formatter())
  log_info("-----------------------")
  poly = polyDegree(xargs, comb)                 ##
  degree = poly$poly                             ## eg: 1 2 1
  termSilence = poly$res                         ## eg: 1 1 1 0
  
  kernel = 0
  big_eqn = ""
  for (j in 1:sum(degree)) {
    t_df = cbind(term_df, res = termSilence)
    dg = t_df[j, -ncol(t_df)] %>% unlist()
    silence = t_df[j, ncol(t_df)] %>% unlist()
    val = 1
    little_eqn = ""
    for (k in 1:xargs) {
      val = silence * val * R_tilda[[k]]^dg[[k]]
      little_eqn = paste0(little_eqn, f("{silence}xR_tilda{k}^{dg[[k]]} x "))
    }
    little_eqn = substr(little_eqn, 1, nchar(little_eqn) - 2)
    
    big_eqn = paste0(big_eqn, little_eqn, "+ ")
    kernel = kernel + val
    
  }
  big_eqn = substr(big_eqn, 1, nchar(big_eqn) - 2)
  log_info(f("Calculated Rkernel for: {big_eqn}"))
  R <- kernel
  eqn[[i]] <- big_eqn
  # R_list[[i]] <- R
  
  # ----fHat---------------------------------------
  coef <- (R + n*lambda*I) %>% GInv()
  coef <- coef %*% matrix(y)

  phi <- R

  fHat <- {t(phi) %*% coef } %>% c()


  ## ----metrics------------------------------------
  fHat_rmse = Metrics::rmse(fHat, y)
  rmse_list[[i]] = fHat_rmse
  log_info(f("Calculated RMSE: {round(fHat_rmse, 2)}"))

}

min_rmse_index = which.min(rmse_list)
min_rmse_eqn = eqn[min_rmse_index]

log_info(f("Minimum RMSE Reported: {rmse_list[min_rmse_index]}"))
log_info(f("For element degrees: {degree_df[min_rmse_index, ]}") %>% skip_formatter())
log_info(f("For equation: {min_rmse_eqn}"))


# write the metrics to a file
valsList = list(n = n, sd = sd, seed = seed, rmse = rmse_list)
readr::write_csv(as.data.frame(valsList), file = here("Data", f("{runTimeName}.csv")), append = TRUE, col_names = FALSE)

log_info(f("File saved: {as.character(here('Data'))}{.Platform$file.sep}{file_name}.csv"))
log_info(f("File has colnames: {names(valsList)}") %>% logger::skip_formatter())
log_info("Code Finalized")




