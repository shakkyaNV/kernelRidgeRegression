library(MASS)
set.seed(1)
n=200
a=0
b=1
x=runif(n,a,b)
x=sort(x)
f=sin(x*6)+x^2+dbeta(x,2,10)
f=dbeta(x,5,20)+dbeta(x,30,5)
y=f+rnorm(n,sd=1)



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
  xx=(x-a)/(b-a)
  yy=(y-a)/(b-a)
  s=1+B1(xx)*B1(yy)+B2(xx)*B2(yy)/4
  s=s-B4(abs(xx-yy))/24
  return(s)
}

# k2=function(x,y){
#   s=dnorm(x-y)
#   return(s)
# }

R=outer(x,x,FUN=k)
I=diag(x=rep(1,n))
lambda=0.00006

cf=ginv(R+lambda*I)%*%y

fitted=R%*%cf


plot(x,y,type="p",pch=20)
lines(x,f,col="red",lwd=2)
lines(x,fitted,col="blue",lwd=2)

sqrt(mean((fitted-f)^2))