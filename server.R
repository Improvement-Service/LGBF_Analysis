shinyServer(function(input, output, session) {
  
  output$indicator <- renderUI({
    bnch_data_subset <- filter(excl_Scotland, Domain == input$category)
    selectInput("indicator2", "Please Select Indicator", unique(bnch_data_subset[[5]]))
    })
  
  output$series <- renderUI({
    bnch_data_indi <- filter(excl_Scotland, Indicator2 == input$indicator2)
    checkboxGroupInput("TSeries", "Select Time Series", unique(bnch_data_indi$Time), selected = unique(bnch_data_indi$Time)) 
 })
  
  observeEvent(eventExpr = input$FmlyGrp2,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "LA",
                                          selected = if(input$FmlyGrp == "All"){
                                            unique(excl_Scotland$`Local Authority`)} 
                                            else{
                                              unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrp))[[1]] 
                                            }
                 )
               }
  )
  
ScotMed10_11_Fun <- reactive({
  yrs <- c(input$TSeries)
    ScotMed10_11 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2010-11" & Indicator2 %in% input$indicator2)
    ScotMed10_11 <- ScotMed10_11[ScotMed10_11$Time %in% yrs,]
    if(is.null(median(ScotMed10_11$Value, na.rm = TRUE))){
      ScotMed10_11 <- 0
      }
    else {
      ScotMed10_11<- median(ScotMed10_11$Value)
    }
})

ScotMed11_12_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed11_12 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2011-12" & Indicator2 %in% input$indicator2)
  ScotMed11_12 <- ScotMed11_12[ScotMed11_12$Time %in% yrs,]
  if(is.null(median(ScotMed11_12$Value, na.rm = TRUE))){
    ScotMed11_12 <- 0
  }
  else{
    ScotMed11_12<- median(ScotMed11_12$Value)
  }
})

ScotMed12_13_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed12_13 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2012-13" & Indicator2 %in% input$indicator2)
  ScotMed12_13 <- ScotMed12_13[ScotMed12_13$Time %in% yrs,]
  if(is.null(median(ScotMed12_13$Value, na.rm = TRUE))){
    ScotMed12_13 <- 0
  }
  else{
    ScotMed12_13<- median(ScotMed12_13$Value)
  }
})

ScotMed13_14_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed13_14 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2013-14" & Indicator2 %in% input$indicator2)
  ScotMed13_14 <- ScotMed13_14[ScotMed13_14$Time %in% yrs,]
  if(is.null(median(ScotMed13_14$Value, na.rm = TRUE))){
    ScotMed13_14 <- 0
  }
   else{
     ScotMed13_14<- median(ScotMed13_14$Value)
  }
})

ScotMed14_15_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed14_15 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2014-15" & Indicator2 %in% input$indicator2)
  ScotMed14_15 <- ScotMed14_15[ScotMed14_15$Time %in% yrs,]
  if(is.null(median(ScotMed14_15$Value, na.rm = TRUE))){
    ScotMed14_15 <- 0
  }
   else{
    ScotMed14_15<- median(ScotMed14_15$Value)
  }
})

ScotMed15_16_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed15_16 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2015-16" & Indicator2 %in% input$indicator2)
  ScotMed15_16 <- ScotMed15_16[ScotMed15_16$Time %in% yrs,]
  if(is.null(median(ScotMed15_16$Value, na.rm = TRUE))){
    ScotMed15_16 <- 0
  }
   else{
     ScotMed15_16<- median(ScotMed15_16$Value)
   }
})

ScotMed10_14_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed10_14 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2010-14" & Indicator2 %in% input$indicator2)
  ScotMed10_14 <- ScotMed10_14[ScotMed10_14$Time %in% yrs,]
  if(is.null(median(ScotMed10_14$Value, na.rm = TRUE))){
    ScotMed10_14 <- 0
  }
  else{
    ScotMed10_14<- median(ScotMed10_14$Value)
  }
})

ScotMed12_15_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed12_15 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2012-15" & Indicator2 %in% input$indicator2)
  ScotMed12_15 <- ScotMed12_15[ScotMed12_15$Time %in% yrs,]
  if(is.null(median(ScotMed12_15$Value, na.rm = TRUE))){
    ScotMed12_15 <- 0
  }
  else{
    ScotMed12_15<- median(ScotMed12_15$Value)
  }
})

ScotMed13_16_Fun <- reactive({
  yrs <- c(input$TSeries)
  ScotMed13_16 <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% "2013-16" & Indicator2 %in% input$indicator2)
  ScotMed13_16 <- ScotMed13_16[ScotMed13_16$Time %in% yrs,]
  if(is.null(median(ScotMed13_16$Value, na.rm = TRUE))){
    ScotMed13_16 <- 0
  }
  else{
    ScotMed13_16<- median(ScotMed13_16$Value)
  }
})

  output$PlotTitle <- renderText({
    paste("",input$indicator2)
  })

    output$plot1 <- renderPlot({
    colnames(excl_Scotland)[1] <- "Local_Authority"
    excl_Scotland <- filter(excl_Scotland, Local_Authority %in% input$LA & Time %in% input$TSeries)
    ggplot(excl_Scotland[excl_Scotland$Indicator2 == input$indicator2,], 
           aes(x = Local_Authority, y = Value, fill = Time))+
      geom_bar(position = "dodge", stat = "identity")+
      theme_bw()+
      geom_hline(yintercept = ScotMed10_11_Fun())+
      geom_hline(yintercept = ScotMed11_12_Fun())+
      geom_hline(yintercept = ScotMed12_13_Fun())+
      geom_hline(yintercept = ScotMed13_14_Fun())+
      geom_hline(yintercept = ScotMed14_15_Fun())+
      geom_hline(yintercept = ScotMed15_16_Fun())+
      geom_hline(yintercept = ScotMed10_14_Fun())+
      geom_hline(yintercept = ScotMed12_15_Fun())+
      geom_hline(yintercept = ScotMed13_16_Fun())+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.x = element_text(angle = 90, hjust = 1))+
      guides(fill = FALSE)
            
  })
 })



