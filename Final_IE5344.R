getwd()
## Answer to the question no 9
y<-c(0,1,0,1,1,0,1,0,1,0,1,1,1,1,0)
x<-c(36.5,21.2,30.8,27.9,21.4,32.7,24.5,26.3,28.2,34.2,29.7,29.3,24.5,22.1,24.3)
dat<-data.frame(y,x)
dat
model<-glm(y~x,data=dat,family = binomial(link='logit'))
summary(model)
20.190-14.449
qchisq(0.99,1)

## The model is not significant at an alpha = 0.01 level of significance 
## since the difference between the null and the residual deviance (5.741) 
## don't exceed the critical value of chi square (6.634897)

## Answer to the question no 10
dat<-read.csv("SoftDrink.csv")
dat
colnames(dat)<-c("obs","y","x1","x2","x3")
head(dat)
str(dat)
dat$x3<-as.factor(dat$x3)
str(dat)
levels(dat$x3)
model<-lm(y~x3,data=dat)
summary(model)
b0<-c(17.783)
b1<-c(2.370)
expected_del_time<-b0+b1
expected_del_time
## The model that the expected delivery time in Lubbock is beta_0 + beta_1 * x3
## = 20.15 minutes

## Answer to the question no 11
dat<-read.csv("SoftDrink.csv")
dat
colnames(dat)<-c("obs","y","x1","x2","x3")
head(dat)
str(dat)
dat$x1<-as.numeric(dat$x1)
dat$x2<-as.numeric(dat$x2)
str(dat)
model<-lm(y~x1+x2,data=dat)
summary(model)
library(car)
vif(lm(y~x1+x2,data=dat))
## vif: x1 = x2 = 1.668196 
## There are no concerns of multicolinearity between x1 and x2 as the 
## VIF is 1.668196 which is less than 5 

