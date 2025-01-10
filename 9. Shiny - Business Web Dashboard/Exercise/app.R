# Exercise using LM

library(shiny)
library(ggplot2)
library(dplyr)

marketing = read.csv('Ch8_marketing.csv')

server <- function(input, output) {
  output$myplot = renderPlot(
    plot(marketing))
}

###############################################################################

ui <- 
  fluidPage(
    # 1. Insert tile
    titlePanel('Revenue Prediction from Marketing Expenditures'),
    
    # 2. Insert sidebar
    sidebarLayout(
      # 2.1 Locate 
      sidebarPanel(
        # 2.1.1 Insert slider
        sliderInput(inputId = 'spend', label = 'Expenditure Level in $K', 
                    min=54, max=481, value=250)
      ),
      
      # 2.2 Insert main panel
      mainPanel(
        # 2.2.1 Insert plot
        plotOutput('myplot')
      )
    )
  )

# Run the app ----
shinyApp(ui = ui, server = server)



