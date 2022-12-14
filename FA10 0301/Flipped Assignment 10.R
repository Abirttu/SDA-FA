getwd()
library(MASS)
dat<-read.csv(file.choose("SoftDrinkDeliveryTime.csv"))
dat
dat<-dat[,-c(1)]
dat
colnames(dat)<-c("y","x1","x2")
colnames(dat)
str(dat)
dat$x1<-as.numeric(dat$x1)
dat$x2<-as.numeric(dat$x2)
head(dat)
str(dat)
mod<-lm(y~.+x1:x2,data=dat)
summary(mod)

# Ans to the ques No. 01
# b0 = 7.139
# b1 = 1.014
# b2 = 0.006
# b3 = 0.0007
# R_squared = 0.9782

plot(mod)
as.data.frame(hatvalues(mod))
plot(mod,4)
# Ans to the ques No. 02
# Residuals vs fitted plot doesn't show any pattern. So, variance is constant
# Normal Q-Q plot shows a straight line. So, error is normally distributed
# Standardized residual plot suggests observations 1, 10, and 11 are the 
# possibly outliers
# Residuals vs leverage plot shows that two observations 9 and 22
# have leverage (leverage higher than 0.24) from the thumbs rule
# Cook's distance shows that observation 9 is influential

# Ans to the ques No. 03
newdat<-dat[-9,]
head(newdat)
str(newdat)
mod2<-lm(y~.+x1:x2,data=newdat)
summary(mod2)

# Ans to the ques No. 04
# b0 = 5.798
# b1 = 1.266
# b2 = 0.008
# b3 = 0.0003
# R_squared = 0.9502

plot(mod2)
as.data.frame(hatvalues(mod2))

# Ans to the ques No. 05
# Residuals vs fitted plot doesn't show any pattern. So, variance is constant
# Normal Q-Q plot shows a straight line. So, error is normally distributed
# Standardized residual plot suggests observations 1, 10, and 11 are the 
# possibly outliers
# Residuals vs leverage plot shows that observation 21 and 22 
# has leverage (leverage higher than 0.25) from the thumbs rule and using data.frame(hatvalues)
# Cook's distance shows that no observation is influential

# Ans to the ques No. 06
# from the diagnostic plot we can say that observation 21 and 22 has 
# leverage but they are not an influential point

newdat2<-dat[-c(21,22),]
head(newdat2)
str(newdat2)
mod3<-lm(y~.+x1:x2,data=newdat2)
summary(mod3)
plot(mod3)
summary(mod)$coeff
summary(mod3)$coeff
summary(mod)$r.square
summary(mod3)$r.square

# Ans to the ques No. 07
# from the above summary we can see that by removing the observations 21 & 22
# which are leverage beta estimates significantly changed between mod and mod3
# R^2 value also went down after removing two leverage observations 21 and 22
# these two observations 21 and 22 should not be removed because removing these
# two observations will affect the regression

