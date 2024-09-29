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
for (i in 1:5000){
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


B1=function(x){
  s=x-1/2
  return(s)
}


B2=function(x){
  s=x^2-x+1/6
  return(s)
}


B3=function(x){
  s=x^3-3/2*x^2+1/2*x
  return(s)
}

B4=function(x){
  s=x^4-2*x^3+x^2-1/30
  return(s)
}

k=function(x,y){
  a=0
  b=1
  xx=(x-a)/(b-a)
  yy=(y-a)/(b-a)
  s=1+B1(xx)*B1(yy)+B2(xx)*B2(yy)/4
  s=s-B4(abs(xx-yy))/24
  return(s)
}



n=200
x=x.sim[1:n]
x=as.numeric(x)
R=outer(x,x,FUN=k)

xx=runif(1000)
R1=outer(xx,x,FUN=k)

lambda=0.000001
a=Variable(n)
obj=-sum(R%*%a)/n+mean(exp(R1%*%a))+lambda*quad_form(a,R)
prob=Problem(Minimize(obj))
result=solve(prob)

ahat=result$getValue(a)


newx=seq(0,1,length=100)
fitted=outer(newx,x,FUN=k)
fitted=fitted%*%ahat

plot(newx,fitted-mean(fitted),ylim=c(-2,2),type="l",col="red")

f0=f(newx)
f0=f0-mean(f0)
lines(newx,f0)
