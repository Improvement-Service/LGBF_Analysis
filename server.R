shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    colnames(bnch_data)[1] <- "Local_Authority"
    ggplot(bnch_data[bnch_data$Indicator2 == "CHN1",], 
           aes(x = Local_Authority, y = Value, fill = Time))+
      geom_bar(position = "dodge", stat = "identity")
            
  })
 

  })

