# Assignment business analytics - Hathim

library(arrow)
library(dplyr)
library(shiny)

gdp_sector = read_parquet("https://storage.dosm.gov.my/gdp/gdp_qtr_nominal_supply.parquet")

gdp = read_parquet("https://storage.dosm.gov.my/gdp/gdp_qtr_nominal.parquet")

str(gdp)
unique(gdp$series)

gdp = gdp %>% subset(series == 'abs')

plot(gdp$value, main = 'Quarterly Nominal GDP', xlab = 'GDP', ylab = 'Year', 
     type = 'l', axes = F)
axis(2)
axis(1, at = 1:nrow(gdp), labels = gdp$date)

ui = fluidPage(
    # Insert title
    titlePanel('Try test')
    
    # Insert subtitle
    
)

server = function(input, output) {}

shinyApp(ui = ui, server = server)
?plot
