library(shiny)
library(ggplot2)
library(dplyr)

marketing = read.csv(file.choose())

server <- function(input, output) {
  output$myplot = renderPlot(
    plot(marketing))
}

################################################################################

ui <- 
  fluidPage(
    titlePanel('Revenue Prediction from Marketing Expenditures'),
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = 'exp', label = 'Expenditure Level in $K', min=10, max=100, value=c(50))
      ),
      mainPanel(
        plotOutput('myplot')
      )
    )
  )

# Run the app ----
shinyApp(ui = ui, server = server)
?sliderInput

