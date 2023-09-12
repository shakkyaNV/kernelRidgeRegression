# base base base


n = 100
x <- seq(1, pi, length.out = n)
y <- sin(x*6) # + rnorm(100, 0, 0.2)
plot(x, y)

m = 2
v = m*2 # why did we define this?

b0 <- function(x) {
  return(rep(1, n))
}
b1 <- function(x) {
  return(x - 1/2)
}
b2 <- function(x) {
  return(x^2 - x + 1/6)
}
b3 <- function(x) {
  return(x^3 - 3/2*x^2 + 1/2*x)
}
b4 <- function(x) {
  return(x^4 - 2*x^3 + x^2 - 1/30)
}

rKernel = 1 + 
  b1(x)*b1(y)/1 + b2(x)*b2(y)/4 + b3(x)*b3(y)/36 + b4(x)*b4(y)/576 +
  ((-1)**1) * b4(abs(x-y))/24

phi = 1 + 
  b1(x)*b1(x)/1 + b2(x)*b2(x)/4 + b3(x)*b3(x)/36 + b4(x)*b4(x)/576 +
  ((-1)**1) * b4(abs(x-x))/24


lambda = 1 / (10**2)
I = diag(1, nrow=100)

coef = (rKernel + MASS::ginv(n*lambda*I))
coef = coef %*% y

phi = phi %>% matrix()
fitted = t(phi) %*% coef

plot(x, y, col = 'red')
lines(x, fitted, col = 'blue')


  