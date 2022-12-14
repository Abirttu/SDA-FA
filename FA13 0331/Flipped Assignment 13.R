getwd()
dat<-read.csv("data-heartbeat-3.csv")
dat<-dat[,-c(8:10)]
head(dat)
colnames(dat)<-c("y","x1","x2","x3","x4","x5","x6","x7")
head(dat)
# Answer to the question no 1
str(dat)
model<-lm(y~x1,data=dat)
summary(model)
# change from the character to factor gives the same summary
dat$x1<-as.factor(dat$x1)
str(dat)
model<-lm(y~x1,data=dat)
summary(model)
# 1a) continent does not seem to be a significant predictor 
# as the p-value of the F-Stat is not significant
# 1b) the regression parameters in the model indicate the marginal contribution 
# of continent over the african continent on the expected heartbeat (response), 
# where continent Africa (AF) is the intercept (base) and doesn't have 
# regression parameter

# Answer to the question no 2
str(dat)
model<-lm(y~x2,data=dat)
summary(model)
# 2a) Exercise frequency with a fixed marginal response is not significant
# as the p-value of the F-Stat is not significant
# 2b) Heartbeat decrease by 0.3039 beats per min (which is the slope)
# if exercise frequency is increased by one unit

# Answer to the question no 3
str(dat)
dat$x2<-as.factor(dat$x2)
str(dat)
levels(dat$x2)
model<-lm(y~x2,data=dat)
summary(model)
# 3a) Exercise frequency with a varying marginal response is not significant
# as the p-value of the F-Stat is not significant
# 3b) the regression parameters denote the marginal effect of the exercise 
# frequency over the lowest level of exercise frequency "never" on the expected
# heartbeat (response) where never exercise frequency  is the intercept (base) 
# and doesn't have regression parameter

# Answer to the question no 4
dat1<-read.csv("data-heartbeat-3.csv")
dat1<-dat1[,-c(8:10)]
head(dat1)
colnames(dat1)<-c("y","x1","x2","x3","x4","x5","x6","x7")
str(dat1)
dat1$x2<-as.factor(dat1$x2)
levels(dat1$x2)
dat1$x5<-as.factor(dat1$x5)
levels(dat1$x5)
str(dat1)
full<-lm(y~.,data=dat1)
summary(full)
library(car)
vif(full)
# vif is not that much high so no issues with multi-collinearity
# Working backward:-
# dropping x2 due to insignificant p value of the t-stat
full1<-lm(y~x1+x3+x4+x5+x6+x7,data=dat1)
summary(full1)
# dropping x4
full2<-lm(y~x1+x3+x5+x6+x7,data=dat1)
summary(full2)
# dropping x6
full3<-lm(y~x1+x3+x5+x7,data=dat1)
summary(full3)
# dropping x3
full4<-lm(y~x1+x5+x7,data=dat1)
summary(full4)
# final model
final_fit<-lm(y~x1+x5+x7,data=dat1)
summary(final_fit)

# 4a) As seen in the Q2 and Q3 the marginal effect on the response 
# was assumed to vary in the model for x2 since the overall fit was 
# better.
# 4b) 0.4184 percent of the variation is explained by the final fit model 
# the p-value of the F-Stat is significant (level 0.05) in the 
# final_fit model.
# 4c) Students from the North America continent and students who don't 
# consume coffee are the significant factors. It is a bit surprising that
# exercise frequency is not found to be significant here.  
# the regression parameters in the model indicate the marginal effect of  
# north american continent born students on the expected heartbeat.
# the regression parameters in the model indicate the marginal effect of  
# the students who don't consume coffee on the expected heartbeat.
