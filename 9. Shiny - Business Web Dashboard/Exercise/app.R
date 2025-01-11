# Exercise using LM

library(shiny)
library(ggplot2)
library(dplyr)

revenue = read.csv('Ch8_marketing.csv')
model = lm(revenues ~ marketing_total, data = revenue)

###############################################################################

server <- function(input, output) {
  output$prediction_plot = renderPlot({
    plot(marketing)
    abline(model)
    newdata = data.frame(marketing_total = input$exp)
    pred = predict.lm(model, newdata, interval = 'predict')
    points(c(rep(input$exp,2)),c(pred[2],pred[3]),
           col = 'orange')
    segments(input$exp, pred[2], input$exp, pred[3],
             col = 'orange')
    text(55,55,'Predicted value of $', pos = 4)
    points(input$exp, pred[1], pch = 19, col = 'blue')
  })
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
        sliderInput(inputId = 'exp', label = 'Expenditure Level in $K', 
                    min=54, max=481, value=250)
      ),
      
      # 2.2 Insert main panel
      mainPanel(
        # 2.2.1 Insert plot
        plotOutput('prediction_plot')
      )
    )
  )

###############################################################################

# Run the app ----
shinyApp(ui = ui, server = server)

