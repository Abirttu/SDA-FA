 # flip assign 01/25 no. 02

x<-c(1,2,3,3,4,4.5)
y<-c(1.5,2,3,4,4.5,6)
model<-lm(y~x)
summary(model)
model
b0<-c(-0.1421)  #intercept
b0
b1<-c(1.2487)  #slope
b1

# Linear Regression Model using least square is y=-0.1421+1.2487*x

y_cap<-b0+b1*x
y_cap

#Estimate of E[Y at X] is b0+b1*x = -0.1421+1.2487*x

summary(model)$sigma
a<-sigma(model)
a
b<-a^2
b

#Estimate of Var[Y] is sigma^2 = 0.30

length(x)
x_bar<-sum(x)/6
x_bar
x_bar<-mean(x)
x_bar
c<-sum((x-x_bar)^2)
c
d<-(x-x_bar)^2
d
length(y)
y_bar<-sum(y)/6
y_bar
t<-((b1-0)/(sqrt(b/c)))  #Calculate t statistic
t
summary(model)

# Since p=0.0028<0.05(level of significance) so these procedure 
# rejects the null hypothesis b1=0
# Hence the regression model is significant 
# OR similarly abs(t)>t(n-2,alpha/2) that is 6.53>2.78(from t table) so these procedure rejects the null hypothesis b1=0 

summary(model)$r.square

n<-c(6)
n
n-2<-c(4)
n-2
t_n<-c(2.78) # from the t table in google
t_n
newx<-c(3)
newx

#Calculate a 95% confidence interval on the mean response when x=3

x<-c(1,2,3,3,4,4.5)
y<-c(1.5,2,3,4,4.5,6)
model<-lm(y~x)
newx<-c(3)
upper_bound<-b0+b1*newx+t_n*sqrt(b*(1/n+((newx-x_bar)^2)/c))
upper_bound
lower_bound<-b0+b1*newx-t_n*sqrt(b*(1/n+((newx-x_bar)^2)/c))
lower_bound
conf<-predict(model,data.frame(x=newx),interval="confidence")
conf

#Calculate a 95% prediction interval on a new observation when x=3

x<-c(1,2,3,3,4,4.5)
y<-c(1.5,2,3,4,4.5,6)
model<-lm(y~x)
newx<-c(3)
upper_bound<-b0+b1*newx+t_n*sqrt(b*(1+1/n+((newx-x_bar)^2)/c))
upper_bound
lower_bound<-b0+b1*newx-t_n*sqrt(b*(1+1/n+((newx-x_bar)^2)/c))
lower_bound
pred<-predict(model,data.frame(x=newx),interval="prediction")
pred

min(x)
max(x)
newx2<-seq(1,4.5,0.05)
newx2
conf<-predict(model,data.frame(x=newx2),interval="confidence")
conf
pred<-predict(model,data.frame(x=newx2),interval="prediction")
pred
plot(x,y)
abline(model)
lines(newx2,conf[,2])
lines(newx2,conf[,3])
lines(newx2,pred[,2])
lines(newx2,pred[,3])
