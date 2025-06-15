getwd()

data = read.csv("marketing.csv")
model <- lm(revenues ~ marketing_total, data = data)
str(model)
model$fitted.values
res_manual <- data$revenues - model$fitted.values
res_model <- model$residuals
yhat_model <- model$fitted.values
yhat_manual <- model$coefficients[1] + (data$marketing_total * model$coefficients[2])

df <- data.frame(cbind(yhat_model, yhat_manual, res_model, res_manual))
print(df)
str(df)
?abline()

# Normality assumption
par(mfrow = c(1,2))
hist(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)

# Equal Variance assumption
plot(model$fitted.values, model$residuals)
abline(0,0, lwd=3);abline(h = c(-6.5,6.5), lwd=3,lty=3)

# Predict
newdata <- data.frame(marketing_total = 460)
predict.lm(model,newdata, level = 0.99, interval = 'predict')
?predict.lm()


library(dplyr)
set.seed(4510)

market_sample <- sample_frac(data,0.3,repalce = TRUE)
sample_model <- lm(revenues ~ marketing_total, data = market_sample)
sample_model
confint(sample_model)














