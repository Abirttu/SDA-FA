library(MASS)  # for box cox
# Minutes of Exposure as x
x<-c(seq(1:12))
x
# Number of Bacteria as y
y<-c(175,108,95,82,71,50,49,31,28,17,16,11)
y
plot(x,y,main="scatterplot of x and y")
model<-lm(y~x)
model
summary(model)
summary(model)$r.square
# Answer to the Ques No a
# intercept beta0= 142.20 and slope beta1= -12.48
# simple linear regression model y = 142.20 - 12.48*x
# value of R^2 = 0.8693

plot(model)
residuals<-summary(model)$residuals
residuals
plot(x,residuals)
# Answer to the Ques No b
# Residuals vs Fitted values plot shows a pattern. Hence, variance is not constant.
# from the Normal probability plot, we see that there is a problem with the normality 
# hence, errors may not be normally distributed.
# # Residuals seem to increase with time which do not represent independence. 
# Hence, model is not adequate.

# Answer to the Ques No c
b<-boxcox(y~x)
b
#95% CI is not within 1 so we can go ahead to perform power transformation.
lambda<-c(b$x)
likelihood<-c(b$y)
which.max(likelihood)
lambda[which.max(likelihood)]
# or
lambda[53]
y
y1<-y^(0.10)
y1
model1<-lm(y1~x)
model1
summary(model1)
summary(model1)$r.square
# Answer to the Ques No d
# intercept beta0= 1.69 and slope beta1= -0.03
# transformed of the simple linear regression model y = 1.69 - 0.03*x
# value of R^2 = 0.9844

plot(model1)
residuals<-summary(model1)$residuals
residuals
plot(x,residuals)
# Answer to the Ques No e
# Residuals vs Fitted values plot shows random scatter. Hence, variance is constant.
# from the Normal probability plot, we see that there is no problem with the normality 
# hence, error is normally distributed
# Residuals do not show any pattern with time. This represents independence.
# Hence, model is adequate.

y2<-1.69184-(0.03447*10)  #when x=10
y2                   
y3 <- exp(log(y2) /lambda[which.max(likelihood)])
y3
((y3 - 17)/17)*100  # finding percentage

# Answer to the Ques No f
# from the transformed model the estimated number of bacteria at 10 minutes of 
# exposure would be 19.10672. The estimated number of bacteria is 12.39245% higher
# than the recorded data.
# compared to the transformed model with the initial data model, we will pick the
# transformed model due to the more better model adequacy than the initial model
# also R^2 value improved in the transformed model too.

x<-c(seq(1:12))
x
y<-c(175,108,95,82,71,50,49,31,28,17,16,11)
y
y1<-y^(0.10)
y1
model1<-lm(y1~x)
model1
xnew<-c(seq(min(x),max(x),1))
xnew
pred<-predict(model1,data.frame(x=xnew),interval="prediction",level=0.95)
pred
# transformed back equation
y4 <- exp(log(pred[10,1]) /lambda[which.max(likelihood)]) 
y4
lower_pred_interval<-exp(log(pred[10,2]) /lambda[which.max(likelihood)])
lower_pred_interval
pred_interval<-y4-lower_pred_interval
pred_interval

# Answer to the Ques No g
# the 95% prediction interval on the number of bacteria at 10 minutes of 
# exposure is 19.10015 with +- 4.872724




