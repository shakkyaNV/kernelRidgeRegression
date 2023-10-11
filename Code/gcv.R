source(here("Code", "utils.R"))


gcvMain <- function(n, model, sd, ...) {
  
  ## ----model-----------------------------------------------------------------
  n = n
  functionName = model
  
  modelvals = modelSp(functionName, n, sd, ...)
  x = modelvals$x
  
  for (i in 1:modelvals$xargs) {
    assign(paste0("x", i), x[i, ]  %>% unlist())
  }
  
  x = x1
  y = modelvals$fx
  
  ## ----kernel----------------------------------------------------------------
  kernel <- bernoulliKernel
  
  logLambda <- seq(-10, 10, length = 50)
  listLambda <- exp(logLambda)
  
  
  ## --------------------------------------------------------------------------
  fitValues <- function(x, y, lambda, kernel) {
    I = diag(1, nrow = length(y))
    R = outer(x, x, FUN = kernel)
    
    coef <- (R + n*lambda*I) %>% GInv()
    coef <- coef %*% matrix(y)
    
    phi <- R
    fHat <- (t(phi) %*% coef) %>% c()
    S = t(phi)%*%((R+n*lambda*I) %>% GInv())
    return(list(fHat, S))
  }
  
  df = tibble(y=y)
  sf = tibble(lambda = purrr::map_chr(listLambda, f))
  
  for (i in 1:length(listLambda)) {
    lambda = listLambda[i]
    # assign("name", f("{round(lambda, 5)}"))
    # f("Calculating fitted value for lambda: {lambda}") %>% log_info()
    # varName = f("fHat_{i}")
    valuesReturn = fitValues(x,y,lambda, kernel)
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
      across(fHat1:fHat50, ~ sum((y-.x)^2)
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
  
  f("Best Fit is given with lambda value: {minlambdaGCV}") %>% 
    log_info()
  
  return(minlambdaGCV)
}

