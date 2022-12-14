getwd()
dat<-read.csv("data-table-B21.csv")
dat
mod<-lm(y~x_1+x_2+x_3+x_4,data=dat)
summary(mod)
# Answer to the ques no a
# The p-value for the F-Statistic is small enough 4.756e-07 or shows 
#significant,but none of the regression parameters have a 
# signficant t-statistic value which shows that there might be 
# some issues with multicolinearity
library(car)
vif(mod)
# Answer to the ques no b
# The VIFs for all of the predictors are very high.
# So it has serious problems with multicolinearity

# dropping x_4 due to the not significant value and highest VIF
mod3<-lm(y~x_1+x_2+x_3,data=dat) 
summary(mod3)
vif(mod3)
# from the summary of mod3, dropping x_3 due to the not significant value 
# and VIF of x_3 is moderately high.

mod4<-lm(y~x_1+x_2,data=dat)
summary(mod4)
vif(mod4)

# Answer to the ques no c
# The model with predictors x1 and x2 have
# highly significant values and their VIF's also very small  
# This is a good fit model 