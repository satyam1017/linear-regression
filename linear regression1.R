getwd()
setwd("Desktop")
 vehicle<-read.csv("vehicle.csv",header=T)
str(vehicle)
#finding missing data
which(is.na(data))

summary(vehicle)
vehicle$lh[vehicle$lh==0]<-mean(vehicle$lh)
vehicle$lc[vehicle$lc==0]<-mean(vehicle$lc)
which(is.na(vehicle)
  #scatterplot
pairs(vehicle[3:5])
cor(vehicle[3:5])
#Data partion
set.seed(1243)
ind<-sample(2,nrow(vehicle),prob=c(.7,.3),replace=T)
training<-vehicle[ind==1,]
testing<-vehicle[ind==2,]
#linear model

model<-lm(lc~lh+Mileage,data=training)
summary(model)
model<-lm(lc~lh,data=training)
summary(model)
plot(lc~lh,training)
abline(model,color="blue")
#prediction
pred<-predict(model,testing)
head(pred)
head(pred)
head(testing)
#variance inflation factor(vif)
newmodel<-lm(lc~lh+Mileage+mc+fm,data=training)
library(faraway)
vif(newmodel)
#as value of vif for each variable is less then 10,so there i no multicollinearity



