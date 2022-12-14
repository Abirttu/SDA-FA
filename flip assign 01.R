# Flipped Assignment 01 in R (01/18/2022)
x<-c(1,2,3)
y<-c(4,2,3)
plot(x,y)
length(x)
length(y)
a<-sum(x)
a
b<-sum(y)
b
c<-sum(x*y) # Calculate sum of (x*y)
c
d<-sum(x^2) # calculate sum of (x^2)
d
e<-sum(x)^2  # calculate whole sum of (x)^2 
e
x_bar<-a/3
x_bar
y_bar<-b/3
y_bar
beta1<-(c-(a*b/3))/(d-(e/3))  # calculate slope 
beta1
beta0<-(b/3-(beta1*(a/3)))  #calculate intercept
beta0
model<-lm(y~x)  # Fit least square parameters estimation
model
y_cap<-beta0+beta1*x   # calculate least square regression equation
y_cap
sse<-sum((y-y_cap)^2)  # calculate sse/residual error
sse
ssr<-sum((y_cap-y_bar)^2)  # calculate ssr
ssr
sst<-sum((y-y_bar)^2)  # calculate sst /total error
sst
sst<-sse+ssr   # calculate sst (alternative way)
sst
# Calculate coefficient of determination R^2
r_square<-ssr/sst
r_square
summary(model)
summary(model)$r.square
# Calculate standard error of regression/mean squared error (MSE) formula: SSE/n-2
sigma_square_cap<-sse/(3-2)
sigma_square_cap
# Alternative way to calculate the sigma value
sigma(model)
summary(model)$sigma
df<-data.frame(y,x)
df
plot(x,y)
abline(model)
