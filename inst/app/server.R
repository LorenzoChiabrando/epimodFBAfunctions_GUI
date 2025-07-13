# inst/app/server.R
library(shiny)

shinyServer(function(input, output, session) {
  
  output$histPlot <- renderPlot({
    # draw input$n random normals and plot
    x <- rnorm(input$n)
    hist(x,
         main  = paste("Histogram of", input$n, "random normals"),
         xlab  = "Value",
         breaks = 30)
  })
  
})

