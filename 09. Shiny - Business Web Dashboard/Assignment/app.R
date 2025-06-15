# Assignment business analytics - Hathim

library(arrow)
library(dplyr)
library(shiny)

gdp_sector = read_parquet("https://storage.dosm.gov.my/gdp/gdp_qtr_nominal_supply.parquet")

gdp = read_parquet("https://storage.dosm.gov.my/gdp/gdp_qtr_nominal.parquet")

###############################################################################
ui = fluidPage(
    # Insert title
    titlePanel('Time Series Forecasting for Malaysia GDP'),
    sidebarLayout(sidebarPanel(sliderInput('date', min=10, max=100, value=10,
                                           label = 'Specify the year')),
                  mainPanel(
                    plotOutput('')
                  ))
    
    # Insert subtitle
    
)
###############################################################################
server = function(input, output) {}
###############################################################################
shinyApp(ui = ui, server = server)
