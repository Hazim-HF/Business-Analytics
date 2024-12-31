gdp = read.csv(file.choose())
str(gdp)
gdp$date = as.Date(gdp$date, format = "%Y-%m-%d")
data = gdp %>% 
  subset(series == 'abs' & year(date) > 1954, select = c('date', 'gdp'))
gdp_ts = ts(data, frequency=1, start=c(1955,1))
dim(gdp_ts)
class(gdp_ts)
plot(decompose(gdp_ts))


