# Answer to the ques no 13.1.A
x<-c(400,220,490,210,500,270,200,470,480,310,240,490,420,330,280,210,300,470,230,430,460,220,250,200,390)
y<-c(0,1,0,1,0,0,1,0,0,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,0)
dat<-data.frame(x,y)
dat
mod<-glm(y~x,data=dat,family=binomial(link='logit'))
summary(mod)
## Therefore logit= 6.070884 - 0.017705*x 
pred<-predict(mod,type="response",data.frame(x=350))
pred
## when the target speed is 350 knots, the expected probability of a hit is
## 0.4686013

# Answer to the ques no 13.3.A
x<-c(2500,2700,2900,3100,3300,3500,3700,3900,4100,4300)
n<-c(50,70,100,60,40,85,90,50,80,65)
success<-c(10,17,30,21,18,43,54,33,60,51)
dat<-data.frame(x,n,success)
dat
failure<-n-success
dat<-cbind(dat,failure)
dat
mod1<-glm(cbind(success,failure)~x,family=binomial(link='logit'))
summary(mod1)
## Therefore logit= -5.3397115 + 0.0015484*x 
pred<-predict(mod1,type="response",data.frame(x=3500))
pred
## A fastener will fail under a load of 3500psi, the expected probability is
## 0.519941

# Answer to the ques no 13.3.B
pred<-predict(mod1,type="response",data.frame(x=4000))
pred
sum(dbinom(0:10,25,pred))
## the expected probability that 10 or less will fail is 0.001687081  

## Alternative if the expected probability that 18 or less will fail) 

pred<-predict(mod1,type="response",data.frame(x=4000))
pred
sum(dbinom(0:18,25,pred))
## the expected probability that 18 or less will fail is 0.6536887


