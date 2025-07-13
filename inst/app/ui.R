# inst/app/ui.R
library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Practice: Random Histogram"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          inputId = "n",
          label   = "Number of observations:",
          min     = 1,
          max     = 1000,
          value   = 100
        )
      ),
      
      mainPanel(
        plotOutput(outputId = "histPlot")
      )
    )
  )
)

