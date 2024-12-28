install.packages("dplyr")
#Chapter 1 ETL
library(dplyr)
fp_bike = "C:\\Users\\hazim\\OneDrive\\01. Education\\02. Master\\01. Business Analytics\\2 - Data Extraction data set\\Ch1_bike_sharing_data.csv"
bike = read.csv(fp_bike, header=T)
#read.csv by default header=T, sep=","
bike = read.table(fp_bike, header=T, sep=",")
dim(bike)
extracted_rows = filter(bike, registered==0, season==1 | season==2)
extracted_rows = filter(bike, registered==0, season %in% c(1,2))
identical(extracted_rows, extracted_rows)
dim(extracted_rows)
extracted_columns = select(extracted_rows, c(season, casual))
add_revenue = mutate(extracted_columns, revenue =casual*5)
grouped = group_by(add_revenue, season)
report = summarise(grouped, sum(casual), sum(revenue))
write.csv(report, "revenue_report.csv", row.names=F)
write.table(report, "revenue_report.txt", row.names=F, sep="\t")
getwd()
#Chapter2 Data Cleaning
library(stringr)
fp_bad_bike = "C:\\Users\\hazim\\OneDrive\\01. Education\\02. Master\\01. Business Analytics\\2 - Data Extraction data set\\raw_bikeshare_data.csv"
bad_bike = read.csv(fp_bad_bike)
str(bad_bike)
dim(bad_bike)
head(bad_bike)
tail(bad_bike)
table(is.na(bad_bike))
str_detect(bad_bike, "NA")
table(is.na(bad_bike$sources))
bad_data = str_subset(bad_bike$humidity, "[a-z A-Z]")
location = str_detect(bad_bike$humidity, bad_data)
bad_bike[location, ]
bad_bike$humidity = str_replace_all(bad_bike$humidity, bad_data, "61")
bad_bike$humidity = as.numeric(bad_bike$humidity)
bad_bike$holiday = factor(bad_bike$holiday, levels=c(0, 1), labels=c("no", "yes"))
bad_bike$workingday = factor(bad_bike$workingday, levels=c(0, 1), 
                             labels=c("no", "yes"))
bad_bike$season = factor(bad_bike$season, levels=c(1,2,3,4), 
                         labels=c("spring", "summer", "fall", "winter"), ordered=T)
bad_bike$weather = factor(bad_bike$weather, levels=c(1,2,3,4), 
                          labels=c("clr_part_cloud", "mist_cloudy", "lt_rain_snow", "hvy_rain_snow"), ordered=T)
library(lubridate)
bad_bike$datetime = mdy_hm(bad_bike$datetime)
unique(bad_bike$sources)
library(stringr)
bad_bike$sources = tolower(bad_bike$sources)
bad_bike$sources = str_trim(bad_bike$sources)
na_loc = is.na(bad_bike$sources)
bad_bike$sources[na_loc] = "unknown"
library(DataCombine)
web_sites = "(www.[a-z]*.[a-z]*)"
current = unique(str_subset(bad_bike$sources, web_sites))
replace = rep("web", length(current))
replacements = data.frame(from=current, to=replace)
bad_bike = FindReplace(data=bad_bike, Var="sources", replacements, from="from",
                       to="to", exact=F)
unique(bad_bike$sources)
# Chapter 3 EDA
fp_mktg = "C:\\Users\\hazim\\OneDrive\\01. Education\\02. Master\\01. Business Analytics\\3 - Data Exploration data set\\Ch3_marketing.csv"
mktg = read.csv(fp_mktg, stringsAsFactors = T)
mktg$pop_density = factor(mktg$pop_density, levels = c("Low", "Medium", "High"),
                          ordered = T)
summary(mktg$google_adwords)
fivenum(mktg$google_adwords)
mean(mktg$google_adwords)
sd(mktg$google_adwords)
var(mktg$google_adwords)
summary(mktg$pop_density)
plot(mktg$pop_density)
boxplot(mktg$google_adwords)
hist(mktg$google_adwords)
mktg$emp_factor = cut(mktg$employees, 2)
mktg$emp_factor = NULL
str(mktg)

mosaicplot(table(mktg$pop_density, mktg$emp_factor), col = c("gray", "black"),
           main = "Factor / Factor")
boxplot(mktg$marketing_total~mktg$pop_density, main = "Factor / Numeric")
plot(mktg$google_adwords, mktg$revenues, main = "Numeric / Numeric")
cor(mktg$google_adwords, mktg$revenues)
cor.test(mktg$google_adwords, mktg$revenues)

# Chapter 3 Linear Regression

fp_adv = "C:\\Users\\hazim\\OneDrive\\01. Education\\02. Master\\01. Business Analytics\\4 - Linear Regression for Business data set\\marketing.csv"
adv <- read.csv(fp_mktg);str(adv)
pairs(adv)
model <- lm(adv$revenues~adv$marketing_total);str(model)
# Before using SLR, the data must satisfy the following assumption
# Checking for Linearity
# Plot a scatterplot and look at the shape and distribution of the data
plot(adv$revenues, adv$marketing_total, main = "Scatterplot of Revenue VS Marketing Total", xlab = "Revenue",
     ylab = "Marketing Total")
# Checking for Independence
# Use your common sense and knowledge

# Checking for Normality
# The residuals form a normal distribution around regression line with mean = 0
# Can also use Q-Q plot
qqnorm(model$residuals);qqline(model$residuals)
hist(model$residuals)

# Check for Equal Variance
# Plot residuals VS fitted value
plot(model$fitted.values, model$residuals);abline(h=0)

# Predict unknown output using SLR
# Works best if predict value within range of existing model
summary(adv$marketing_total)

