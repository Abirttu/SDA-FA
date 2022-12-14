getwd()
dat<-read.csv(file.choose("data-prob-7-6.csv"))
dat
# Answer to the ques no a

model<-lm(y~x1*x2+I(x1^2)+I(x2^2),data=dat)
model
summary(model)
#beta0 = 3025.32
#beta1 = -194.27
#beta2 = -6.05
#beta3 = 3.63
#beta4 = 1.15
#beta5 = -1.33
# Model Equation = 3025.32 -194.27*x1 -6.05*x2 +3.63*(x1^2) +1.15*(x2^2)
# -1.33*(x1*x2)

# Answer to the ques no b

plot(model)
# Residual vs. fitted plot shows the random scatter pattern and hence
# the variance is constant
# Normal probability plot shows normality so errors may be normally
# distributed

# Answer to the ques no c

library(MASS)
b<-boxcox(model)
b
# since lambda=1 is within 95% confidence interval so box-cox transformation
# will not be performed

# Answer to the ques no d
reduced<-lm(y~x1+x2,data=dat)
summary(reduced)
full<-lm(y~x1*x2+I(x1^2)+I(x2^2),data=dat)
summary(full)
anova(reduced,full)
# full model is significant than the reduced model with the p-value 
# 0.044<0.05
# from the summary of the full model, we can say that the final model is

# Answer to the ques no e
final<-lm(y~x2+I(x2^2),data=dat)
final
summary(final)
plot(final)
# none of the points are influential because in none of the observations
# cook's distance >1

# Answer to the ques no f

model<-lm(y~x1*x2+I(x1^2)+I(x2^2),data=dat)
model
df<-data.frame(x1=30,x2=23)
pred<-predict(model,df,interval="prediction",level=0.95)
pred
