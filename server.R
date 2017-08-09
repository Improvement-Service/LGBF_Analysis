library(shiny)

shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot(
    plot(x,y)
  )
 

  })

