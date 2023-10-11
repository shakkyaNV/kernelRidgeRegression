require(here)
source(here("Code", "utils.R"))
require(tibble)
require(tidyr)
require(dplyr)

gcvMain <- function(x, fx) {
  
  ## ----parameters-------------------------------------------------------------
  n = length(fx)
  x = unlist(x)
  
  ## ----kernel----------------------------------------------------------------
  kernel <- bernoulliKernel
  
  logLambda <- seq(-10, 10, length = 50)
  listLambda <- exp(logLambda)
  
  
  ## --------------------------------------------------------------------------
  fitValues <- function(x, fx, lambda, kernel) {
    I = diag(1, nrow = length(fx))
    
    
    for (i in 1:min(dim(x))) {
      xi = evalHere(f("x{i}"))
      log_info(f("Calculating outer Rkernerl for <x{i}, x{i}>"))
      assign(f("Rkernel{i}"), outer(xi, xi, bernoulliKernel))
      Rkernel[f("Rkernel{i}")] = evalHere(f("Rkernel{i}")) %>% list()
    }
    R = mprod(kernel = Rkernel, xdim = min(dim(x)), name = "Rkernel", I = I)
    
    coef <- (R + n*lambda*I) %>% GInv()
    coef <- coef %*% matrix(fx)
    
    phi <- R
    fHat <- (t(phi) %*% coef) %>% c()
    S = t(phi)%*%((R+n*lambda*I) %>% GInv())
    return(list(fHat, S))
  }
  
  df = tibble(fx=fx)
  sf = tibble(lambda = purrr::map_chr(listLambda, f))
  
  for (i in 1:length(listLambda)) {
    lambda = listLambda[i]
    # assign("name", f("{round(lambda, 5)}"))
    # varName = f("fHat_{i}")
    valuesReturn = fitValues(x,fx,lambda, kernel)
    valuesReturn[1][[1]] -> vals
    valuesReturn[2][[1]] -> s
    df[, i+1] <- vals
    sf[i, 2] <- nest(as.data.frame(s))
  }
  
  colnames(df) <- c("x", purrr::map2_chr("fHat", seq(1:50), .f = f))
  
  ## ----gcv-------------------------------------------------------------------
  gcv = vector()
  df %>% 
    mutate(
      across(fHat1:fHat50, ~ sum((fx-.x)^2)
      )
    ) %>% 
    select(-x) %>% 
    slice(1) %>% 
    unlist() -> fitGCV
  
  # calcualte generalized cross validation value 
  for (i in 1:nrow(sf)) {
    gcv[i] <- {
      fitGCV[i] / { 1- (
        sum(diag( # trace
          sf[i, 2] %>% unnest(cols=c(data)) %>% as.matrix() # = S
        )) / (n)
      )}^2
    } / (n)
  }
  
  gcv %>% as_tibble_col(column_name = "gcv") %>% 
    bind_cols(as_tibble_col(listLambda, column_name = "lambdaValue")) %>% 
    arrange(gcv) %>% 
    head(5) -> minGCV
  
  ## --------------------------------------------------------------------------
  minlambdaGCV <- minGCV[{which.min(minGCV$gcv)}, 2] %>% pull()
  return(minlambdaGCV)
}
