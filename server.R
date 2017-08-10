shinyServer(function(input, output) {
  
  output$indicator <- renderUI({
    bnch_data_subset <- filter(bnch_data, Domain == input$category)
    selectInput("indicator2", "Please Select Indicator", unique(bnch_data_subset[[5]]), selected = input$indicator2)
  })
  
    output$plot1 <- renderPlot({
    colnames(bnch_data)[1] <- "Local_Authority"
    bnch_data <- filter(bnch_data, Local_Authority %in% input$LA)
    ggplot(bnch_data[bnch_data$Indicator2 == input$indicator2,], 
           aes(x = Local_Authority, y = Value, fill = Time))+
      geom_bar(position = "dodge", stat = "identity")+
      theme_bw()+
      guides(fill = FALSE)
            
  })
 

  })

