getwd()
dat<-read.csv(file.choose())
dat


model1<-lm(y~x1+x2,data=dat)  # if don't ask for the interaction
summary(model1)

# alternative way

mod<-lm(y~.,data=dat)
mod
summary(mod)
# Answer to the ques no A
# Model Equation using LM function: y = beta0+beta1*x1+beta2*x2+ e

# Answer to the ques no B
# beta0 = 11.09
# beta1 = 350.1
# beta2 = 0.1089

# Model Equation
# y_hat = 11.09+350.1*x1+0.1089*x2+ e
