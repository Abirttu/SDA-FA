getwd()
dat<-read.csv("data-likelihood-exponential.csv")
mean(dat$x)
lambda<-c(1/1.113032)
lambda
# the possible values of the rate lambda = 0.4,0.9,1.4

# Answer to the question A
x<-dat[1:25,1]
head(x)
exp(sum(log(dexp(x,rate=0.4))))
exp(sum(log(dexp(x,rate=0.9))))
exp(sum(log(dexp(x,rate=1.4))))
# lambda = 0.9 is the most likely as it has a greater likelihood 
# than the other values of lambda = 0.4 and lambda = 1.4

# Answer to the question B
x<-dat[1:50,1]
head(x)
exp(sum(log(dexp(x,rate=0.4))))
exp(sum(log(dexp(x,rate=0.9))))
exp(sum(log(dexp(x,rate=1.4))))
# lambda = 0.9 is the most likely as it has a greater likelihood 
# than the other values of lambda = 0.4 and lambda = 1.4

# Answer to the question C
# The value of lambda = 0.9 is the same when considering 
# 25 versus 50 observations.
x<-dat[1:5,1]
x
y<-dexp(x,rate=0.4)
y
plot(x,y)

