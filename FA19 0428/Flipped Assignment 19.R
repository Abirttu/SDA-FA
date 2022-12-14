y<-c(24,28,40,42,11,16,126,34,32,32,34,17,30,17,50)
x1<-c(7,5,7,7,6,6,5,6,5,6,5,7,6,6,6) 
x2<-c(4,3,3,2,4,3,3,2,4,2,2,3,3,3,4) 
x3<-c(90,105,105,90,105,90,75,105,90,75,90,75,90,90,75) 
dat<-data.frame(y,x1,x2,x3)
dat

# Answer to the ques no 1
model1<-glm(y~x1+x2+x3,data=dat,family=poisson(link ="log"))
summary(model1)
## Therefore log= 8.243925 -0.346247 *x1 -0.088511 *x2 -0.026742 *x3

# Answer to the ques no 2
model2<-glm(y~x1+x2+x3,data=dat,family=poisson(link ="identity"))
summary(model2)
## Therefore identity= 153.6777 -6.7614 *x1 -5.3548 *x2 -0.6835 *x3

# Answer to the ques no 3
213.45-133.38  #Difference between null and residual deviance of log link model
qchisq(0.95,3)  #Critical value of log link model
## Difference between null and residual deviance value 80.07 exceeds the critical
## value 7.814728. Hence the poisson regression model from ques 1 is adequate.

213.45-157.17  #Difference between null and residual deviance of identity link model
qchisq(0.95,3)   #Critical value of identity link model
## Difference between null and residual deviance value 56.28 exceeds the critical
## value 7.814728. Hence the poisson regression model from ques 2 is adequate.
## We will choose the model1 with log link.

# Answer to the ques no 4
pred<-predict(model1,type="response",data.frame(x1=5,x2=3,x3=90))
pred
## The estimated mean number of unpopped kernels when Temperature with 5, 
## Oil with 3, and Time with 90 is 46.54296

# Answer to the ques no 5
pred<-predict(model1,type="response",data.frame(x1=5,x2=3,x3=90))
pred
sum(dpois(0:30,pred))
## The estimated probability that there will be less or equal to 30 unpopped kernels 
## when with Temperature 5, Oil 3, and Time 90 is 0.006484185





