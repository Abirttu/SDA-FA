getwd()
install.packages("broom")
install.packages("MuMIn")
install.packages("olsrr")
# All possible first order models by AIC's
dat<-read.csv("data-table-B9.csv")
library(MuMIn)
fullmodel <- lm(y~., data=dat,na.action = "na.fail")
summary.fit<-dredge(fullmodel)
summary.fit
plot(summary.fit)
# the best fitting model based on dredge is y~x2+x3+x4 with the lowest AIC 383.8
# since lower the AIC, the better our model.

# We also had a close competition(Second best fitting) which is y~x1+x2+x3+x4
# with the AIC=384.2
# All possible first order models to find best subset
library(olsrr)
fullmodel <- lm(y~., data=dat)
summary.fit<-ols_step_best_subset(fullmodel)
summary.fit
plot(summary.fit)
# Subsequent Model Analysis
summary(lm(y~x2,data=dat)) #model 1
summary(lm(y~x2+x3,data=dat)) #model 2
summary(lm(y~x2+x3+x4,data=dat))#model 3
summary(lm(y~x1+x2+x3+x4,data=dat))#model 4
# The best models using the criterion AIC, Adj R^2, F-stat p value, model 4
# and model 3 are the best fitting model because their AIC, Adj R^2 values 
# are almost same though from their summary, individual parameters x1 from model 4 and 
# x4 from model 3 is not significant. so we are applying vif in the below
# to check collinearity
# Another good fit can be model 2 since from the summary of the model
# indicates all individual parameters are significant though AIC
# value is slightly greater than model 4 and model 3 and Adj R^2 value is 
# slightly smaller than model 4 and model 3
# Analysis of the candidate models
library(car)
vif(lm(y~x1+x2+x3+x4,data=dat))
vif(lm(y~x2+x3+x4,data=dat))
vif(lm(y~x2+x3,data=dat))
# It seems that there is no issue with the multicollinearity.
