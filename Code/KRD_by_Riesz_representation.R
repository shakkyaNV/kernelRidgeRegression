here::i_am("Code/KRD_by_Riesz_representation.R")
require(here)
require(CVXR)
require(rlang); require(magrittr)

# Setup
set.seed(1)
n = 200
source(here("Code", "utils.R"))
source(here("Code", "Accept_Reject.R"))


# Create the simulation realizations for p(x) = exp(f(x))
p_x_all = sim.fun(n)
x = p_x_all$realization

# create bold(R) = R[Xi, Xj]
R = outer(x, x, FUN = bernoulliKernel) 

# Create phi_x = R[Xi, x]
phi = outer(runif(1000), x, FUN = bernoulliKernel)
lambda = 0.000001

# By reisz representation KRD estimator C_hat_KRD is equal to argmin{} of c
# So we'll solve this argmin using CVXR
c = CVXR::Variable(n)
objective = {-sum(R%*%c)/n} + 
            {{phi%*%c} %>% exp() %>% mean()} + 
            {lambda * quad_form(c, R)}

problem = Problem(Minimize(objective))
result = solve(problem)
c_hat_KRD = result$getValue(c)


# now that we have the RKD estimator for c, We can plug into our defined function
# as a kernel ridge density estimator
x_new = seq(0, 1, length.out = 100)
temp_y = outer(x_new, x, FUN = bernoulliKernel)
fitted_values = temp_y %*% c_hat_KRD


# Actual values from the function itself
p_x = p(x_new)


# Hist
# hist(log(p_x) - mean(log(p_x)), probability = TRUE, breaks = 50,
#      main = "Histogram of RKD Estimator: (Blue is RKD)")
# lines(newx, exp(fitted_values - mean(fitted_values)), 
#       type = 'l', col = "blue")


# Plot 
plot(x_new, fitted_values - mean(fitted_values), type = "l", col = "red")
lines(x_new, f(x_new) - mean(f(x_new)), col = "blue")
title("Blue is function f(x) values, Red is RKD")












