y<-c(0,0,1,1,0,1,1,1,0,0,1,1,1,0,1,0,1,0,0,0)
x1<-c(45000,40000,60000,50000,55000,50000,35000,65000,53000,48000,37000,31000,40000,75000,43000,49000,37500,71000,34000,27000) # Income
x2<-c(2,4,3,2,2,5,7,2,2,1,5,7,4,2,9,2,4,1,5,6)  # Age 
dat<-data.frame(y,x1,x2)
dat

# Answer to the ques no A
model<-glm(y~x1+x2,data=dat,family=binomial(link='logit'))
model
## Therefore logit= -7.047e+00 + 7.382e-05 *x1 + 9.879e-01 *x2

# Answer to the ques no B
summary(model)
qchisq(0.95,2)  #Critical value
27.726-21.082   #Difference between null and residual deviance
## Null deviance: 27.726  on 19  degrees of freedom
## Residual deviance: 21.082  on 17  degrees of freedom
## AIC: 27.082
## Difference between null and residual deviance value 6.644 exceeds the critical
## value 5.991465. Hence the logistic regression model from part A is adequate.

# Answer to the ques no C
## Beta_1 is the marginal expected increase in the logit of the probability of 
## purchasing new automobile with an increase of income $1.
## Beta_2 is the marginal expected increase in the logit of the probability of 
## purchasing new automobile with an increase of age 1 year of current auto.

# Answer to the ques no D
pred<-predict(model,type="response",data.frame(x1=45000,x2=5))
pred
## The estimated probability that a person with an income of 45000 and 5 years old 
## car will purchase a new vehicle in the next six months is 0.7710279


