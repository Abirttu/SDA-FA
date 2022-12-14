getwd()
dat<-read.csv(file.choose("data-table-B9.csv"))
dat

model<-lm(y~x1*x2*x3*x4-x1:x2:x3-x1:x2:x4-x1:x3:x4-x2:x3:x4-x1:x2:x3:x4,data=dat)
summary(model)
model<-lm(y~x1*x2*x3*x4-x1:x2:x3-x1:x2:x4-x1:x3:x4-x2:x3:x4-x1:x2:x3:x4-x1:x3-x2:x3,data=dat)
# Eliminating x1:x3 and x2:x3 interactions because of NA values observed
summary(model)
plot(model)

# Answer to the ques no A
# By examining the residual vs fitted plots, we can say that there seems no pattern
# exist also normality plot shows that there is no problem with the normality.
# hence, we can say that the model is adequate.
# p-value of 1.947e-13 indicates small value
# so the regression model is significant

reduced<-lm(y~x1+x2+x3+x4,data=dat)
reduced
full<-lm(y~x1*x2*x3*x4-x1:x2:x3-x1:x2:x4-x1:x3:x4-x2:x3:x4-x1:x2:x3:x4-x1:x3-x2:x3,data=dat)
full
anova(reduced,full)

# Answer to the ques no B
# Using a partial F-test on the above, we can see that our full model's 
# (with two factors interaction) p-value is 0.024 which is less than 0.05 so 
# all the two factors interaction model is significant than the reduced model.

model<-lm(y~x1*x2*x3*x4-x1:x2:x3-x1:x2:x4-x1:x3:x4-x2:x3:x4-x1:x2:x3:x4-x1:x3-x2:x3,data=dat)
summary(model)

# Answer to the ques no C
# the best fitting model using partial F tests will be the full model with two
# factors interaction. from the summary of the model we can conclude that
# x2 and x2:x4 interactions have the significant values. therefore, the final
# model is
final<-lm(y~x2+x3+x4+x2:x4,data=dat)
final
summary(final)
plot(final)

newx2<-c(10.0,3.0)
newx3<-c(0.5,0.25) 
newx4<-c(0.75,0.85) 
data.frame(newx2,newx3,newx4)
predict(final,data.frame(x2=newx2,x3=newx3,x4=newx4))

# Answer to the ques no D
predict(final,data.frame(x2=newx2,x3=newx3,x4=newx4),interval="confidence",level = 0.95)

# Answer to the ques no E
predict(final,data.frame(x2=newx2,x3=newx3,x4=newx4),interval="prediction",level = 0.95)



