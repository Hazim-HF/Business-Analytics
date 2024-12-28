mydat=read.csv(file.choose())
attach(mydat)

#simple linear regression
lm(revenues~marketing_total, data=mydat)
model1=lm(revenues~marketing_total, data=mydat)
#compute residual
res= revenues-(model1$coefficients[1]+model1$coefficients[2]*marketing_total)

#or extract residual from the model1 object
model1$residual

#checking
hist(res)
qqnorm(res)
plot(res)
plot(model1) #diagnostic plot

#predict unknown output
new.data=data.frame(marketing_total=c(450,460,470))
predict.lm(model1,new.data,interval="predict",level=0.99)

##########################################################
#sampling data
library(dplyr)
sampled.data=sample_frac(mydat,0.5,replace=FALSE)
samp.model=lm(revenues~marketing_total, data=sampled.data)
library(stats)
confint(samp.model)

#############################################################
#transformation & influential observations
x0=1:10
y0=c(1,1.41,1.73,2,2.24,2.45,2.65,2.83,3,3.16)
fit0=lm(y0~x0)
par(mfrow=c(1,3))
plot(x0,y0)
abline(fit0)
hist(fit0$residuals)
plot(fit0$fitted.values,fit0$residuals)
abline(h=0)

#not normal, not equal variance, so transform y
library(MASS)
boxcox(fit0)
y0t=y0^2
fit0t=lm(y0t~x0)
par(mfrow=c(1,3))
plot(x0,y0t)
abline(fit0t)
hist(fit0t$residuals)
plot(fit0t$fitted.values,fit0t$residuals)
abline(h=0)

#transformation on x
x1=c(1,5,15,30,60,120,240,480,720,1440,2880,5760,10080)
y1=c(0.84,0.71,0.61,0.56,0.54,0.47,0.45,0.38,0.36,0.26,0.2,0.16,0.08)
fit1=lm(y1~x1)
par(mfrow=c(1,3))
plot(x1,y1)
abline(fit1)
hist(fit1$residuals)
plot(fit1$fitted.values,fit1$residuals)
abline(h=0)

#log transformation
x1t=log(x1)
fit1t=lm(y1~x1t)
par(mfrow=c(1,3))
plot(x1t,y1)
abline(fit1t)
hist(fit1t$residuals)
plot(fit1t$fitted.values,fit1t$residuals)
abline(h=0)

#outliers and influential observations
x4=c(1:20)
y4=c(0.4,2.2,2.2,5.6,5.3, 5.2, 7.5, 8.7,9.6,9.7,12.5,12.4,12.4,12.8,16.1,16,17,11.5,19.8,20.6)
wo=lm(y4~x4)
xh4=c(1:17, 19,20)
yh4=c(0.4,2.2,2.2,5.6,5.3, 5.2, 7.5, 8.7,9.6,9.7,12.5,12.4,12.4,12.8,16.1,16,17,19.8,20.6)
wto=lm(yh4~xh4)

################################################################
#multiple linear regression
model2=lm(revenues~google_adwords+facebook+twitter, data=mydat)
plot(model2)