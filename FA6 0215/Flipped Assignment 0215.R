getwd()
library(MASS)
a<-read.csv(file.choose())
a
Y<-c(a$y)
Y
X1<-c(a$x1)
X1
X2<-c(a$x2)
X2
length(X1)
ones<-rep(1,36)
X<-cbind(ones,X1,X2)
X
as.data.frame(X)
t(X)
t(X)%*%X
library(MASS)
ginv(t(X)%*%X)
t(X)%*%Y
b<-ginv(t(X)%*%X)%*%t(X)%*%Y
b
as.data.frame(b)
