library(dplyr)
library(lubridate)

# ch6 rider data

rider = read.csv('Ch6_ridership_data_2011-2012.csv')
str(rider)
rider = rider %>%
  group_by(Year = year(datetime), Month = month(datetime)) %>%
  summarise(sum(count))
table(rider$Year, rider$Month)
str(rider)
rider_ts = ts(rider[,3], frequency = 12, start = c(2011,1))
str(rider_ts)
plot(decompose(rider_ts))
plot(forecast(auto.arima(rider_ts)))
plot(forecast(tbats(rider_ts),h=12))



# gdp data

gdp = read.csv('gdp_gni_annual_nominal.csv')
str(gdp)
gdp = gdp %>%
  filter(series == 'abs') %>%
  select(c(2,3)) %>%
  filter(year(date) > 1954)
data = gdp %>% 
  group_by(Year = year(date), Month = month(date)) %>%
  summarise(Price = mean(gdp))
table(data$Year, data$Month)
gdp_ts = ts(data[,3], frequency=1, start=c(1947))
class(gdp_ts)
gdp_ts
plot(decompose(gdp_ts))
par(mfrow=c(1,2))
plot(forecast(auto.arima(gdp_ts)))
plot(forecast(tbats(gdp_ts)))

# Quarterly gdp

gdp = read.csv('gdp_qtr_nominal.csv')
str(gdp)
gdp = gdp %>%
  filter(series == 'abs') %>%
  select(c(2,3))
data = gdp %>% 
  group_by(Year = year(date), Month = month(date)) %>%
  summarise(Price = mean(value))
table(data$Year, data$Month)
gdp_ts = ts(data[,3], frequency=4, start=c(2015,1))
class(gdp_ts)
gdp_ts
par(mfrow=c(1,2))
plot(decompose(gdp_ts))
plot(forecast(auto.arima(gdp_ts)))
plot(forecast(tbats(gdp_ts)))

# stock market

ftse = read.csv("FTSE Malaysia KLCI Historical Data.csv")
head(ftse)
ftse = ftse[,c(1,2)]
head(ftse)
str(ftse)
ftse$Price = as.numeric(gsub(',','',ftse$Price))
ftse$Date = as.Date(ftse$Date, format = '%m/%d/%Y')
ftse = ftse %>% 
  group_by(Year = year(Date), Month = month(Date)) %>%
  summarise(Price = mean(Price))
head(ftse)
str(ftse)
head(ftse)
table(ftse$Year, ftse$Month)
ftse_ts = ts(ftse[,3], frequency = 12, start=c(2010,5))
class(ftse_ts)
ftse_ts

plot(decompose(ftse_ts))
par(mfrow=c(1,2))
plot(forecast(auto.arima(ftse_ts)))
plot(forecast(tbats(ftse_ts)))

# Claude AI Analysis 
# Load required libraries
library(tidyverse)
library(forecast)
library(tseries)
library(ggplot2)
library(zoo)

# Read and prepare the data
data <- read.csv("gdp_gni_annual_nominal.csv")
data$date <- as.Date(data$date)

# Filter for 'abs' series only (absolute values)
abs_data <- data %>% 
  filter(series == "abs")

# Create time series objects
gdp_ts <- ts(abs_data$gdp, start = 1947, frequency = 1)
gni_ts <- ts(abs_data$gni, start = 1947, frequency = 1)

# 1. Basic Time Series Plot
ggplot(abs_data, aes(x = date)) +
  geom_line(aes(y = gdp, color = "GDP"), size = 1) +
  geom_line(aes(y = gni, color = "GNI"), size = 1) +
  labs(title = "GDP and GNI Over Time",
       x = "Year",
       y = "Value",
       color = "Metric") +
  theme_minimal() +
  scale_color_manual(values = c("GDP" = "blue", "GNI" = "red"))

# 2. Decomposition Analysis for GDP
gdp_decomp <- decompose(gdp_ts)
plot(gdp_decomp)

# 3. Calculate rolling statistics
window_size <- 5
abs_data <- abs_data %>%
  mutate(
    gdp_ma = rollmean(gdp, k = window_size, fill = NA),
    gdp_sd = rollapply(gdp, width = window_size, FUN = sd, fill = NA)
  )

# 4. Growth Rate Analysis
ggplot(data %>% filter(series == "growth_yoy"), aes(x = date)) +
  geom_line(aes(y = gdp, color = "GDP Growth"), size = 1) +
  geom_line(aes(y = gni, color = "GNI Growth"), size = 1) +
  labs(title = "Year-over-Year Growth Rates",
       x = "Year",
       y = "Growth Rate (%)",
       color = "Metric") +
  theme_minimal()

# 5. Stationarity Test
adf_test_gdp <- adf.test(gdp_ts, alternative = "stationary")
print("ADF Test Results for GDP:")
print(adf_test_gdp)

# 6. Correlation Analysis
correlation <- cor(abs_data$gdp, abs_data$gni, use = "complete.obs")
print(paste("Correlation between GDP and GNI:", round(correlation, 4)))

# 7. Forecast future values using ARIMA
gdp_arima <- auto.arima(gdp_ts)
gdp_forecast <- forecast(gdp_arima, h = 5)
plot(gdp_forecast, main = "GDP Forecast for Next 5 Years")

# 8. Per Capita Analysis
ggplot(abs_data, aes(x = date)) +
  geom_line(aes(y = gdp_capita, color = "GDP per capita"), size = 1) +
  geom_line(aes(y = gni_capita, color = "GNI per capita"), size = 1) +
  labs(title = "Per Capita GDP and GNI Over Time",
       x = "Year",
       y = "Value per Capita",
       color = "Metric") +
  theme_minimal()

# 9. Summary Statistics
summary_stats <- abs_data %>%
  summarise(
    gdp_mean = mean(gdp, na.rm = TRUE),
    gdp_sd = sd(gdp, na.rm = TRUE),
    gni_mean = mean(gni, na.rm = TRUE),
    gni_sd = sd(gni, na.rm = TRUE),
    gdp_capita_mean = mean(gdp_capita, na.rm = TRUE),
    gni_capita_mean = mean(gni_capita, na.rm = TRUE)
  )
print("Summary Statistics:")
print(summary_stats)
