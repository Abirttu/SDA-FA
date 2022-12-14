getwd()
dat<-read.csv("data-table-B9.csv")
head(dat)
str(dat)
summary(lm(y~x1+x2+x3+x4+
             +x1:x2+x1:x3+x1:x4
           +x2:x3+x2:x4
           +x3:x4-x1:x3-x2:x3,data=dat))
library(car)
vif(lm(y~x1+x2+x3+x4+
         +x1:x2+x1:x3+x1:x4
       +x2:x3+x2:x4
       +x3:x4-x1:x3-x2:x3,data=dat))
dat<-data.frame(scale(dat[,1:4],center=TRUE,scale=TRUE),dat[,5])
colnames(dat)<-c("x1","x2","x3","x4","y")
head(dat)
summary(lm(y~x1+x2+x3+x4+
             +x1:x2+x1:x3+x1:x4
           +x2:x3+x2:x4
           +x3:x4-x1:x3-x2:x3,data=dat))
library(car)
vif(lm(y~x1+x2+x3+x4+
         +x1:x2+x1:x3+x1:x4
       +x2:x3+x2:x4
       +x3:x4-x1:x3-x2:x3,data=dat))
# Answer to the ques no a
# It makes sense to scale the predictor variables since we will be considering 
# their two-factor interactions as candidates for inclusion

model1<-lm(y~1,data=dat)
formula(model1)
step(model1,scope~x1+x2+x3+x4+
     +x1:x2+x1:x3+x1:x4
     +x2:x3+x2:x4
     +x3:x4
     ,direction="forward")
summary(lm(y ~ x2 + x3 + x4 + x2:x4, data = dat))
vif(lm(y ~ x2 + x3 + x4 + x2:x4, data = dat))
# Answer to the ques no b
# Using forward selection, model with the terms x2, x3,x4 and x2:x4 has selected
# and found significant

model2<-lm(y~.,data=dat)
formula(model2)
step(model2,scope~x1+x2+x3+x4+
       +x1:x2+x1:x3+x1:x4
     +x2:x3+x2:x4
     +x3:x4,direction="both")
summary(lm(y ~ x2 + x3 + x4 + x2:x4, data = dat))
vif(lm(y ~ x2 + x3 + x4 + x2:x4, data = dat))
# Answer to the ques no c
# Using both selection, model with the terms x2, x3,x4 and x2:x4 has selected
# and found significant

model3<-lm(y~.+.*.,data=dat)
formula(model3)
step(model3,direction = "backward")
summary(lm(y ~ x2 + x3 + x4 + x2:x4, data = dat))
vif(lm(y ~ x2 + x3 + x4 + x2:x4, data = dat))
# Answer to the ques no d
# Using backward selection, model with the terms x2, x3,x4 and x2:x4 has selected
# and found significant

# Answer to the ques no e
# From the parts b,c,d we have found the same model y ~ x2 + x3 + x4 + x2:x4
# where p value of the F-stat is significant, 73.36% of the variation is 
# explained by the model.


