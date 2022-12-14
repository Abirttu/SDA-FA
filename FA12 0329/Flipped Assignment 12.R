getwd()
library(MASS)
library(car)
dat<-read.csv("data-table-B8.csv")
dat
mod1<-lm(y~x1+x2,dat)
summary(mod1)
vif(mod1)
# Answer to the ques no: A
# from the summary of the mod1, the p value of the F-stat and t-stat of the
# regression parameters shows highly significant
# VIF for mod1 is x1= 1.016535 and x2= 1.016535

mod2<-lm(y~x1+x2+x1:x2,dat)
summary(mod2)
vif(mod2)
# Answer to the ques no: B
# from the summary of the mod2, the p value of the F-stat is significant
# however the p value of t-stat for the regression parameters of the
# interaction (x1:x2) is not significant
# VIF for mod2 is x1= 3.639435 and x2= 1.505416, x1:x2=3.822936

dat1<-data.frame(scale(dat[,1:2],center=TRUE,scale=TRUE),dat[,3])
colnames(dat1)<-c("x1","x2","y")
mod3<-lm(y~x1+x2,dat1)
head(dat1)
summary(mod3)
vif(mod3)
# Answer to the ques no: C
# after the standardization of the predictor variables with the first order 
# model VIF for mod3 is x1= 1.016535 and x2= 1.016535

dat2<-data.frame(scale(dat[,1:2],center=TRUE,scale=TRUE),dat[,3])
colnames(dat2)<-c("x1","x2","y")
mod4<-lm(y~x1+x2+x1:x2,dat2)
head(dat2)
summary(mod4)
vif(mod4)

# Answer to the ques no: D
# after the standardization of the predictor variables with the second order 
# interactions VIF for mod4 is x1= 1.018439,x2= 1.078223 and x1:x2= 1.066356


# Answer to the ques no: E
# for the question A & C, first order model before and after the standardization
# the VIF value remains the same. So for the first order model,
# standardization is not required since there is no interactions of
# the predictor variables x1 and x2
# for the question B & D, second order interactions before and after the standardization
# the VIF value varies. So for the second order interactions
# standardization is required to reduce the effects of multicolinearity.

