##################################### SETUP ####################################

rm(list = ls())

if (.Platform$GUI == "RStudio") {
  n=100;sd=0.01;seed=12;jobid=010;jobname="testAll";
  runTimeName="newLocalRun";parameters = c(1, 2);b=0;B=100;alpha=0.1
} else {
  args <- commandArgs(TRUE)
  parameters <- as.numeric(args[1:5])
  n = parameters[1]
  sd = parameters[2]
  seed = parameters[3]
  jobid = parameters[4]
  b = parameters[5]
  runTimeName = args[6]
  jobname = args[7]
  B=500
}


suppressMessages(library(here))
suppressMessages(source(here("Code", "utils.R")))
suppressMessages(library(logger, quietly = TRUE))
suppressMessages(source(here("Code", "gcv.R")))
config <- config::get()


date_time = format(Sys.time(), "%H_%M_%b_%d_%Y")
file_name = f("{runTimeName}")
if (.Platform$GUI == "RStudio") {
  appender = appender_tee(here("Logs", f("{file_name}.log")))
} else {
  appender = appender_file(here("Logs", f("{file_name}.log")))
}
log_appender(appender)


##################### UNSCALED ORIGINAL PROCEDURE ##############################

set.seed(seed)
log_info("-----------------------------------------------------")
log_info(f("Process Starting: Epsilon Bootrap procedure for power function"))
log_info(f("Parameters Received: {paste(parameters, collapse = ', ')}"))


#################################### INPUT #####################################

n = n
# B = 100 specified as a local/else parameter
alpha = 0.05

functionName <- "DGP1"

modelVals = modelSp(functionName, n = n)
xargs = modelVals$xargs
x <<- modelVals$x                          # global
fx<<- modelVals$fx                         # global
y <<- {b*fx} + rnorm(n, mean = 0, sd = sd) # global
lambda <<-  gcvMain(x=x, fx=fx)            # global

################################################################################


bernoulliKernel <- bernoulliKernel
p = as.integer(runif(1, n-10, n))
modelVals_p = modelSp(functionName, n=p)
xp <<- modelVals_p$x
#fxnew = modelVlas_p$fx # we don't need fx. So we disregard

calcFHat <- function(X, x, kernel, wmat=0) {
  
  n = max(dim(X))
  p = max(dim(x))
  xDim = min(dim(X))
  
  Xi = c()
  xi = c()
  
  if (missing(wmat)) {
    wmat = diag(1, nrow = n)
  }
  
  for (i in 1:xDim) { # assign each row to x1, x2 ...>
    Xi[[i]] = X[i, ] %>% unlist()
  }
  for (i in 1:xDim) { # assign each row to x1, x2 ...>
    xi[[i]] = x[i, ] %>% unlist()
  }

  # log_info("Best fit is given with lambda value: {lambda}")
  I = diag(1, nrow = n)
  R <- c()
  phi <- c()
  for (i in 1:xDim) { # R = list(R1, R2)
    # log_info(f("Calculating outer R_tilda kernel for <x{i}, x{i}>"))
    R[i] = outer(Xi[[i]], Xi[[i]], bernoulliKernel) %>% list()
    phi[i] = outer(Xi[[i]], xi[[i]], bernoulliKernel) %>% list()
  }
  
  R <- mprodId(kernel = R)
  phi <- mprodId(kernel = phi)
  coef <- { (wmat %*% R) + n*lambda*I} %>% GInv()
  coef <- coef %*% matrix(wmat %*% y)
  phi <- phi
  fHat <- {t(phi) %*% coef } %>% c()
  return(fHat)
}

fHat_original <- calcFHat(X=x, x=xp, bernoulliKernel)

phi_n_original = max(abs(fHat_original))

####################### BOOSTRAP PROCEDURE (WITH W) ############################

phi_n_star = c()
w = c()
fHat = c()
fHat_weighted = c()

for (i in 1:B) {
  log_info(paste0("Starting Iteration ", i, "/", B))
  ## Not sure about calc from here
  modelVals_i = modelSp(functionName, n = p)
  xargs_i = modelVals_i$xargs
  xip = modelVals_i$x
  fxip = modelVals_i$fx
  yi = {b*fxip} + rnorm(p, mean = 0, sd = sd)
  
  # to here (why is this necessary? What is P? Is it different from n?)
  
  wi = rexp(n, rate = 1)
  w[[i]] <- wi
  wmat = diag(wi)
  
  # fHat[[i]] = calcFHat(X=xp, x=xp, kernel=bernoulliKernel) # already calculated above
  fHat_weighted[[i]] = calcFHat(X=x, x=xip, kernel=bernoulliKernel, wmat=wmat)
  
  phi_n_star[i] = max(abs( fHat_weighted[[i]] - fHat_original ))
  
}

################################ SUMMARY #######################################

ptile.t = quantile(x=unlist(phi_n_star), probs=1-(alpha/2), names=FALSE)
p.value = sum(unlist(phi_n_star) > phi_n_original) / B
if(phi_n_original > ptile.t) {
  status = "Rejected"
} else {
    status = "Not_Rejected"}

log_info(paste0("ptile.t value: ", ptile.t))
log_info(paste0("p.value is: ", p.value))

log_info(paste0("Compare inequality: ", phi_n_original, " (phi_original)", "  ??  ", ptile.t, " (upper alpha t-quantile)"))
log_info(paste0("H0: b=0 is: ", status))


############################# WRITE TO FILE ####################################

df = data.frame(
  seed = seed,
  phi_n_original = phi_n_original, 
  phi_n_star = phi_n_star, 
  ptile.t = ptile.t, 
  p.value = p.value, 
  status = status,
  b = b
)

readr::write_csv(df, file = here("Data", f("{runTimeName}_{jobid}.csv")), append = TRUE, col_names = FALSE)


log_info(f("File saved: {as.character(here('Data'))}{.Platform$file.sep}{file_name}.csv"))
log_info(f("File has colnames: {paste(names(df), collapse = ', ')}"))
log_info("Code Finalized")






