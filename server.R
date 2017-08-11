shinyServer(function(input, output, session) {
  
  output$indicator <- renderUI({
    bnch_data_subset <- filter(excl_Scotland, Domain == input$category)
    selectInput("indicator2", "Please Select Indicator", unique(bnch_data_subset[[5]]), width = "40%")
    })
  
  output$series <- renderUI({
    bnch_data_indi <- filter(excl_Scotland, Indicator2 == input$indicator2)
    checkboxGroupInput("TSeries", "Select Time Series", unique(bnch_data_indi$Time), selected = unique(bnch_data_indi$Time)) 
 })
  
  observeEvent(eventExpr = input$FmlyGrp2,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "LA",
                                          selected = unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrp))[[1]])
               }
               )

    output$plot1 <- renderPlot({
    colnames(excl_Scotland)[1] <- "Local_Authority"
    excl_Scotland <- filter(excl_Scotland, Local_Authority %in% input$LA & Time %in% input$TSeries)
    ggplot(excl_Scotland[excl_Scotland$Indicator2 == input$indicator2,], 
           aes(x = Local_Authority, y = Value, fill = Time))+
      geom_bar(position = "dodge", stat = "identity")+
      theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.x = element_text(angle = 90, hjust = 1))+
      guides(fill = FALSE)
            
  })
#Create Ui Outputs for year on year section    
    output$indicatorYr <- renderUI({
      bnch_data_subset <- filter(bnch_data, Domain == input$categoryYr)
      selectInput("indicatorYrSrv", "Please Select Indicator", unique(bnch_data_subset[[5]]), width = "40%")
    })
    
    bnch_data_indiYR <- reactive({
      dta <- filter(excl_Scotland, Indicator2 == input$indicatorYrSrv)
    })
    
    output$baseYr <- renderUI({
      bnch_data_indiYR <- bnch_data_indiYR()
  #selectizeInput lets you have it start blank instead of
  #selecting the first value - as selectInput does!
      selectizeInput("baseYrSrv", "Year", 
                     choices = unique(bnch_data_indiYR$Time),
                    options = list(
                      placeholder = "-",
                      onInitialize = I('function() {this.setValue("");}')
                    )
               ) 
    })
    output$compYr <- renderUI({
      bnch_data_indiYR <- bnch_data_indiYR()
      selectizeInput("compYrSrv", "Comparator Year:", 
                     choices = c(unique(bnch_data_indiYR[bnch_data_indiYR$Time != input$baseYrSrv, "Time"])),
      options = list(
        placeholder = "-",
        onInitialize = I('function() {this.setValue("");}')
      ))
    })
#calculate changes for each LA between base and comparator year
    chngDta <- reactive({
      data <- bnch_data_indiYR()
  #only keep selected years
      yrs <- c(input$compYrSrv, input$baseYrSrv)
      data <- data[data$Time %in% yrs,]
    #Now calculate higher year (x[2]) minus lower year (x[1])
      Diffdata <- round(ave(data$Value, as.factor(data$`Local Authority`), 
                  FUN = function(x){x[2] -x[1]}), 1)[1:33]
    #This one calculates % change  
      PerDiffdata <- round(ave(data$Value, as.factor(data$`Local Authority`), 
             FUN = function(x){(x[2]/x[1]-1)*100}), 1)[1:33]
      data <- data.frame( data$`Local Authority`[1:33], Diffdata, PerDiffdata,
                    stringsAsFactors = FALSE)
    })
    output$`Year-on-Year-Plot` <-renderPlot({
      dat <- chngDta()
      colnames(dat)[1] <- "Local_Authority"
      if(input$RelVal == FALSE){
      ggplot(data = dat, aes(x = Local_Authority, y = Diffdata)) +
        geom_bar(stat = "identity", position= "dodge", fill = "darkblue")+
        theme_bw()+
        ylab(paste("Change from", as.character(input$baseYrSrv), "to", as.character(input$compYrSrv))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
      } else{
        ggplot(data = dat, aes(x = Local_Authority, y = PerDiffdata)) +
          geom_bar(stat = "identity", position= "dodge", fill = "darkblue")+
          theme_bw()+
          ylab(paste("Percentage Change from", as.character(input$baseYrSrv), "to", as.character(input$compYrSrv))) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"))
      }
    })
    
    observeEvent(eventExpr = input$FmlyGrp2Yr,
                 handlerExpr = {
                   updateCheckboxGroupInput(session = session,
                                            inputId = "LAYr",
                                            selected = unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpYr))[[1]])
                 }
    )
    
 })



