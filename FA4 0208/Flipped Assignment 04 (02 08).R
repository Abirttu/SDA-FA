# index as x
x<-c(16.7,17.1,18.2,18.1,17.2,18.2,16.0,17.2,18.0,17.2,16.9,17.1,18.2,17.3,17.5,16.6)
# days as y
y<-c(91,105,106,108,88,91,58,82,81,65,61,48,61,43,33,36)
length(x)
# Answer to the ques no. a
plot(x,y,main="ScatterPlot of x and y")

# Answer to the ques no. b
model<-lm(y~x)
model
summary(model)
# Slope beta1 = 15.3 and the intercept beta0 = -193
# Model equation y_hat= -193 + 15.3 * x

# Answer to the ques no. c
abline(model)

# Answer to the ques no. d

plot(model)

#Constant variance check: Residual vs. fitted value plot shows random scatter
# which indicates that variance is constant.
# Normality Check: From the Normal Q-Q plot we can see that the error is normally
# distributed
# from the fitted values vs. sqrt(standardized residuals) plot we see that all
# the observations lie within the value 
# 1.5 so it seems that there will be no outliers or possibly observations 1,2 
# and 15 may be outliers

# Answer to the ques no. e

# From the summary of the model, we see that abs value of t is 1.624 which is less
# than the value of t[alpha/2,n-2] which is 2.144 which does not reject the null 
# hypothesis beta1 = 0 hence the regression is not significant




