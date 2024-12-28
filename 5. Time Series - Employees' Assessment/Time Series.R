library(lubridate)
library(dplyr)
data = read.csv(file.choose())
head(data)

monthly_ride = as.data.frame(data %>%
				group_by(year=year(datetime), month=month(datetime)) %>%
				summarise(riders=sum(count)))

table(monthly_ride$year, monthly_ride$month)

riders = monthly_ride[,3]
riders

monthly = ts(riders, frequency=12, start=c(2011,1))
monthly

plot(decompose(monthly))

# By looking at the plot, we can see that there's a trend, so we need to differentiate 1 time, and there's also
# seasonality. So, we need to differentiate the second time

plot(monthly)
y=diff(monthly)
plot(y)
z=diff(diff(monthly))
plot(z)

acf(monthly)
pacf(monthly)

# from the plot, we can infer that pacf has AR(1) and MA(1) & MA(2) are potential.
model1 = arima(monthly, c(1,0,0), seasonal=list(order=c(0,0,0)))
model2 = arima(monthly, c(1,1,0), seasonal=list(order=c(0,0,0)))
model3 = arima(monthly, c(2,1,0), seasonal=list(order=c(0,0,0)))
model4 = arima(monthly, c(1,1,0), seasonal=list(order=c(0,1,0)))
model5 = arima(monthly, c(0,1,1), seasonal=list(order=c(0,0,0)))

model1
model2
model3
model4
model5

tsdiag(model1)
tsdiag(model2)
tsdiag(model3)
tsdiag(model4)
tsdiag(model5)

library(forecast)
yr_forecast = forecast(monthly, h=12)
plot(yr_forecast)

tbats(monthly)
j=forecast(tbats(monthly), h=12)
plot(j)

summary(j$mean)
summary(j$upper)
summary(j$lower)

mean_2011 = round(as.numeric(filter(monthly_ride, year == 2011) %>%
			summarise(mean = mean(riders))), 0)
mean_2012 = round(as.numeric(filter(monthly_ride, year == 2012) %>%
			summarise(mean = mean(riders))), 0)
mean_2013 = round(mean(j$mean),0)
max_mean_2013 = round(max(j$mean),0)

abline(h=max(j$mean), lty=2, col='blue')
segments(2011, mean_2011, x1=2012, y1=mean_2011, col='darkgray', lty=2, lwd=2)
segments(2012, mean_2012, x1=2013, y1=mean_2012, col='darkgray', lty=2, lwd=2)
segments(2013, mean_2013, x1=2014, y1=mean_2013, col='darkgray', lty=2, lwd=2)

text(2011.15, mean_2011 + 10000, mean_2011)
text(2012, mean_2012 + 10000, mean_2012)
text(2013, mean_2013 + 10000, mean_2013)
text(2013.85, max_mean_2013 + 10000, max_mean_2013)

