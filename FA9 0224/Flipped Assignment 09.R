getwd()
library(MASS)
dat<-read.csv(file.choose("data-table-B8.csv"))
dat
y<-c(dat$y)
y
x1<-c(dat$x1)
x1
x2<-c(dat$x2)
x2
model<-lm(y~x1*x2)
model
summary(model)

# Answer to the ques no. A
# beta0=12.50
# beta1=256.74
# beta2=0.099
# beta3=0.76
# Model Equations: y=12.50+256.74*x1+0.099*x2+0.76*(x1:x2)+e

plot(model)

# Answer to the ques no. B
# Residual vs Fitted values plot shows random scatter so the variance is constant
# Normality plot shows that errors are normally distributed
# Model is adequate

reduced<-lm(y~x1+x2)
summary(reduced)
full<-lm(y~x1*x2)
summary(full)
anova(reduced,full)

# Answer to the ques no. C
# # At a significance level of 0.05, the p-value of 2.318e-13 in the full model
# denotes that the regression is significant from the summary of the full model
# whereas ANOVA test is not necessary here

summary(full)

# Answer to the ques no. D
# The regression parameter for the interaction between x1 and x2 is 
# insignificant from the summary of the full model
# Final model is our reduced model

final<-lm(y~x1+x2)
summary(final)

# Final model equation y = 11.09 + 350.1*x1 + 0.1089*x2 + e
