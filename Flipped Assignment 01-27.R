# index as x
x<-c(16.7,17.1,18.2,18.1,17.2,18.2,16.0,17.2,18.0,17.2,16.9,17.1,18.2,17.3,17.5,16.6)
# days as y
y<-c(91,105,106,108,88,91,58,82,81,65,61,48,61,43,33,36)
model<-lm(y~x)
model
summary(model)

# Ans to the ques no A
# intercept beta0 = -193, slope beta1 = 15.3
# Estimates of the fitted Regression line is y= -193 + 15.3 * x

summary(model)$r.square

# Ans to the ques no B
# Value of R^2 is 0.1584636

summary(model)

# Ans to the ques no C
# from the model summary, we can conclude that Since p value > 0.05 
# so these procedure does not reject the null hypothesis b1=0 
# hence the regression is not significant

x <- c(16.7,17.1,18.2,18.1,17.2,18.2,16.0,17.2,18.0,17.2,16.9,17.1,18.2,17.3,17.5,16.6)
y <- c(91,105,106,108,88,91,58,82,81,65,61,48,61,43,33,36)
a<-summary(model)$sigma
a
c<-a^2
c
x_bar<-mean(x)
x_bar
b<-sum((x-x_bar)^2)
b
t_value<-c(15.3/(sqrt(c/b)))
t_value

# Ans to the ques no C (Alternative way)
# from the model summary or the manual calculations done above we can say that
# abs(t)=1.6240 < 2.145 T(alpha/2, n-2) (from t table) so these procedure does not 
# reject the null hypothesis b1=0 hence the regression is not significant

x <- c(16.7,17.1,18.2,18.1,17.2,18.2,16.0,17.2,18.0,17.2,16.9,17.1,18.2,17.3,17.5,16.6)
y <- c(91,105,106,108,88,91,58,82,81,65,61,48,61,43,33,36)
model<-lm(y~x)
xnew<-seq(min(x),max(x),0.01)
conf<-predict(model,data.frame(x=xnew),interval="confidence")
conf
pred<-predict(model,data.frame(x=xnew),interval="prediction")
pred
plot(x,y)
abline(model)
lines(xnew,conf[,2])
lines(xnew,conf[,3])
lines(xnew,pred[,2])
lines(xnew,pred[,3])

# Ans to the ques no D
# answer showed above


x <- c(16.7,17.1,18.2,18.1,17.2,18.2,16.0,17.2,18.0,17.2,16.9,17.1,18.2,17.3,17.5,16.6)
y <- c(91,105,106,108,88,91,58,82,81,65,61,48,61,43,33,36)
model<-lm(y~x)
xnew<-c(17.0)  # when Index is x=17.0
conf<-predict(model,data.frame(x=xnew),interval="confidence")
conf

# Ans to the ques no E
# Confidence interval fit value is 67.05, lower interval is 52.53
# and upper interval is 81.58
# Meaning: we are 95% confident that on the mean number of days the ozone 
# level exceeds 20ppm when the meterological index is 17.0 is between
# these range [52.53,81.58]



x <- c(16.7,17.1,18.2,18.1,17.2,18.2,16.0,17.2,18.0,17.2,16.9,17.1,18.2,17.3,17.5,16.6)
y <- c(91,105,106,108,88,91,58,82,81,65,61,48,61,43,33,36)
model<-lm(y~x)
xnew<-c(17.0)  # when Index is x=17.0
pred<-predict(model,data.frame(x=xnew),interval="prediction")
pred

# Ans to the ques no F
# prediction interval fit value is 67.05, lower interval is 13.99 
# and upper interval is 120.12
# Meaning: 95% prediction interval of [13.99,120.12] means that 
# we are 95% confident that on the mean number of days the ozone 
# level exceeds 20ppm when the meterological index is 17.0 will fall 
# within this range [13.99,120.12].
# we can say that prediction interval plot will be much wider than 
# the confidence interval plot

# if asked to calculate 99% or 90% confidence and prediction interval 
# then the formula to find out alpha will be 100*(1-alpha)% alpha= 0.01 0r 0.1
# add level=0.90/0.99 after the confidence/prediction interval in R command

    


