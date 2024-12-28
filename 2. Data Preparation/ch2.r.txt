bike <- read.csv("{filename}.csv") 
getwd()
setwd()
str(bike)
bike <- read.table("./data/Ch1_bike_sharing_data.csv", sep = ",", header = TRUE) 

library(dplyr)

#filtering data
extracted_rows <- filter(bike, registered == 0,
 season == 1 | season == 2)

dim(extracted_rows)

using_membership <- filter(bike, registered == 0, season %in% c(1, 2))

identical(extracted_rows, using_membership)

#add calculated column
add_revenue <- mutate(extracted_columns, revenue = casual * 5)

#aggregate data
grouped <- group_by(add_revenue, season)
report <- summarise(grouped, sum(casual), sum(revenue))

write.csv(report, "revenue_report.csv", row.names = FALSE)
write.table(report, "revenue_report.txt", row.names = FALSE, sep = "\t")

#flawed data
table(is.na(bike))
library(stringr)
bad_data <- str_subset(bike$humidity, "[a-z A-Z]")
location <- str_detect(bike$humidity, bad_data)
bike[location, ]
bike$humidity <- str_replace_all(bike$humidity, bad_data, "61")
bike[location, ]

#convert data types
bike$humidity <- as.numeric(bike$humidity)
bike$holiday <- factor(bike$holiday, levels = c(0, 1),
 labels = c("no", "yes"))
bike$workingday <- factor(bike$workingday, levels = c(0, 1),
 labels = c("no", "yes"))
bike$season <- factor(bike$season, levels = c(1, 2, 3, 4),
 labels = c("spring", "summer",
 "fall", "winter"),
 ordered = TRUE)
bike$weather <- factor(bike$weather, levels = c(1, 2, 3, 4),
 labels = c("clr_part_cloud",
 "mist_cloudy",
 "lt_rain_snow",
 "hvy_rain_snow"),
 ordered = TRUE)

library(lubridate)
bike$datetime <- mdy_hm(bike$datetime)

#adapt data
unique(bike$sources)
bike$sources <- tolower(bike$sources)
bike$sources <- str_trim(bike$sources)
na_loc <- is.na(bike$sources)
bike$sources[na_loc] <- "unknown"

#combine data
library(DataCombine)
web_sites <- "(www.[a-z]*.[a-z]*)"
current <- unique(str_subset(bike$sources, web_sites))
replace <- rep("web", length(current))
replacements <- data.frame(from = current, to = replace)
bike <- FindReplace(data = bike, Var = "sources", replacements,
 from = "from", to = "to", exact = FALSE)
unique(bike$sources)
bike$sources <- as.factor(bike$sources)
write.csv(bike,"Ch2_clean_bike_sharing_data.csv",
 row.names = FALSE)



