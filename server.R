shinyServer(function(input, output) {
  
  output$indicator <- renderUI({
    bnch_data_subset <- filter(excl_Scotland, Domain == input$category)
    selectInput("indicator2", "Please Select Indicator", unique(bnch_data_subset[[5]]), selected = input$indicator2)
  })
  
    output$plot1 <- renderPlot({
    colnames(excl_Scotland)[1] <- "Local_Authority"
    excl_Scotland <- filter(excl_Scotland, Local_Authority %in% input$LA)
    ggplot(excl_Scotland[excl_Scotland$Indicator2 == input$indicator2,], 
           aes(x = Local_Authority, y = Value, fill = Time))+
      geom_bar(position = "dodge", stat = "identity")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.x = element_text(angle = 90, hjust = 1))+
      guides(fill = FALSE)
            
  })
 

  })

