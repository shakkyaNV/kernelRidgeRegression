library(AR)
library(mvtnorm)
library(CVXR)
set.seed(1)
f=function(x){
  s=sin(x*6)+x^2+dbeta(x,5,10)
  s=-1*s/4
  return(s)
}
f=function(x){
  s=dbeta(x,5,20)+dbeta(x,30,5)
  s=-1*s/4
  return(s)
}

p=function(x){
  #s=exp(-x[1]^2/2-x[2]^2/2)
  #s=exp(-x^2)
  #if (x[1]^2+x[2]^2<=1){
  #if (x[1]<1 & x[1]>0 & x[2]<1 & x[2]>0){
  if (x>0 & x<1){
    s=exp(f(x))
  }else{
    s=0
  }
  
  return(s)
}

x.sim=c()
for (i in 1:10000){
  #y=rmvnorm(1, rep(0,2),diag(c(1,1)))
  y=runif(1)
  u=runif(1,0,1)
  #  print(fx(y))
  #  print(dmvnorm(y,rep(0,2),diag(c(1,1))))
  #print(fx(y)/dmvnorm(y,rep(0,2),diag(c(1,1))))
  
  r=p(y)/dunif(y)/2.5
  print(r)
  if (u<=r){
    x.sim=rbind(x.sim,y)
  }
}

newx=seq(0,1,length=100)

d=density(x.sim,from=0,to=1,bw=0.02)
fd=approx(d$x, d$y, xout = newx)
fhat=log(fd$y)

plot(fd$x,fhat-mean(fhat),col="blue", type="l")

f0=f(newx)
lines(newx, f0-mean(f0))