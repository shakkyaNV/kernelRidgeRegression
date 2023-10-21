## ----setup, include=FALSE-----------------------
# knitr::opts_chunk$set(echo = TRUE)

rm(list = ls())
# n=100;sd=0.01;seed=12;jobid=010;jobname="testAll";runTimeName="newLocalRun";parameters = c(1, 2)
args <- commandArgs(TRUE)
parameters <- as.numeric(args[1:4])
n = parameters[1]
sd = parameters[2]
seed = parameters[3]
jobid = parameters[4]
runTimeName = args[5]
jobname = args[6]

here::i_am("Code/multiD.R")
suppressPackageStartupMessages(library(here, quietly = TRUE))
suppressPackageStartupMessages(library(logger, quietly = TRUE))
source(here("Code", "utils.R"))
source(here("Code", "gcv.R"))
config <- config::get()

date_time = format(Sys.time(), "%H_%M_%b_%d_%Y")
file_name = f("{runTimeName}")
if (.Platform$GUI == "RStudio") {
  appender = appender_tee(here("Logs", f("{file_name}.log")))
} else {
  appender = appender_file(here("Logs", f("{file_name}.log")))
}
log_appender(appender)


## ----model--------------------------------------
log_info("-----------------------------------------------------")
log_info(f("Process Starting: multiDimensional Kernel Regression Metrics Evaluation"))
log_info(f("Parameters Received: {paste(parameters, collapse = ', ')}"))

set.seed(seed)

n = n
functionName <- "DGP2"

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
# lambda = 0.0001544701 ## DGP2
# lambda = 0.0001544701 ## DGP1

bernoulliKernel <- bernoulliKernel
lambda = gcvMain(x = x, fx = fx) # optimized according to GCV function
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

degree_df = generate_combs(xargs + 1) ## degree combinations
term_df   = generate_combs(xargs)     ## term combinations

eqn = c()
rmse_list = c()


####### ADD Explanation --- LOOKS LIKE WORKING

for (i in 1:nrow(degree_df)) {                ## Iterate through over all combinations
  comb = degree_df[i, ] %>% as.numeric()      ## eg: 1 1 0
  log_info(f("Calculating Interactional RMSE for term {i}/{nrow(degree_df)}: {paste(comb, collapse = ', ')}"))
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
  
  # ----fHat---------------------------------------
  coef <- (R + n*lambda*I) %>% GInv()
  coef <- coef %*% matrix(y)

  phi <- R

  fHat <- {t(phi) %*% coef } %>% c()


  ## ----metrics------------------------------------
  fHat_rmse = Metrics::rmse(fHat, unlist(fx))
  rmse_list[[i]] = fHat_rmse
  log_info(f("Calculated RMSE: {round(fHat_rmse, 2)}"))

}

df = data.frame(rmse = unlist(rmse_list)) %>% rownames_to_column(var = "id")
df = df[order(df$rmse, decreasing = FALSE), ]
rownames(df) <- seq(1, nrow(df), 1)


log_info("********* MIN 5**************")
for (i in 1:5) {
  log_info(f("{i}th least rmse: {df[i, 2]}, For element order: {paste(degree_df[df[i, 1], ], collapse = ', ')}"))
}

log_info("*********** WANTED ************")

if (functionName == "DGP1") {
  des_index = 8
} else if (functionName == "DGP2") {
  des_index = 8
}

log_info(f("{functionName} RMSE for (1, 1, 1): {paste(df[df$id == des_index, 2], collapse = ', ')}"))
log_info(f("Element order (for confirmation): {paste(degree_df[as.numeric(df[df$id == des_index, 1]), ], collapse = ', ')}"))


min_rmse_index = which.min(rmse_list)
min_rmse_eqn = eqn[min_rmse_index]

log_info("********** MINIMUM **************")

log_info(f("Minimum RMSE Reported: {rmse_list[min_rmse_index]}"))
log_info(f("For Element with degree order: {paste(degree_df[which.min(rmse_list), ], collapse = ', ')}"))
log_info(f("For equation: {min_rmse_eqn}"))

log_info("************************")

# write the metrics to a file
degree_df = degree_df %>% rownames_to_column('id')
df = merge(df, degree_df, by = 'id')
df$n = n; df$sd = sd; df$seed = seed

readr::write_csv(df, file = here("Data", f("{runTimeName}.csv")), append = TRUE, col_names = FALSE)

log_info(f("File saved: {as.character(here('Data'))}{.Platform$file.sep}{file_name}.csv"))
log_info(f("File has colnames: {paste(names(df), collapse = ', ')}"))
log_info("Code Finalized")

