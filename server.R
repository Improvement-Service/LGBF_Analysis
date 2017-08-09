shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot(
    ggplot(bnch_data[bnch_data$Indicator2 == "CHN1",], 
           aes(x = 'Local Authority', y = Value, fill = Time))+
      geom_bar(position = "dodge", stat = "identity")
            
  )
 

  })

