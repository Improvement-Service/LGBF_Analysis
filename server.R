shinyServer(function(input, output, session) {

 #create reactive input that updates local authoriy to select all or clear all  
   observeEvent(eventExpr = input$LAAll,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "LA",
                                          selected = unique(excl_Scotland$`Local Authority`)
                 )
               }
  )
  
  observe({
    if(input$LAClear >0){
      updateCheckboxGroupInput(session = session, 
                               inputId = "LA",
                               selected = character(0))
    }
  })
  
            
  output$indicator <- renderUI({
    bnch_data_subset <- filter(excl_Scotland, Domain == input$category)
    selectInput("indicator2", "Please Select Indicator", unique(bnch_data_subset$Title), width = "100%")
    })
  
 output$series <- renderUI({
    bnch_data_indi <- filter(excl_Scotland, Title == input$indicator2)
    checkboxGroupInput("TSeries", "Select Time Series", unique(bnch_data_indi$Time), selected = NULL) 
 })

#create a reactive function to store time series choices available    
 TDta <- reactive({
   dta <- filter(excl_Scotland, Title == input$indicator2)
   TChoices <- unique(dta$Time)
 })   

#create buttons to select all or clear all in time series
  observeEvent(eventExpr = input$SeriesAll,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "TSeries",
                                          selected = TDta())
               }
  )
  
  observe({
    if(input$SeriesClear >0){
      updateCheckboxGroupInput(session = session, 
                               inputId = "TSeries",
                               selected = character(0))
    }
    
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
  
SelectedDta <- reactive({
  dta <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% input$TSeries & Title %in% input$indicator2)
})

#Calculates median values for each year group selected, based on the indicator selected and the authorities selected
MedFun <- reactive({
  SelectedDta <- SelectedDta()
  MedianVal <- round(ave(SelectedDta$Value, as.factor(SelectedDta$Time), FUN = function(x){median(x, na.rm = TRUE)}))
})

  output$PlotTitle <- renderText({
    paste("",input$indicator2)
  })

    output$plot1 <- renderPlot({
    colnames(excl_Scotland)[1] <- "Local_Authority"
    excl_Scotland <- filter(excl_Scotland, Local_Authority %in% input$LA & Time %in% input$TSeries)
    ggplot(excl_Scotland[excl_Scotland$Title == input$indicator2,], 
           aes(x = Local_Authority, y = Value, fill = Time))+
      geom_bar(position = "dodge", stat = "identity")+
      theme_bw()+
      geom_hline(aes(yintercept = MedFun(), colour = Time))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.3))+
      guides(fill = FALSE)
            
  })
    
#Create Ui Outputs for year on year section  
    observeEvent(eventExpr = input$LAYrAll,
                 handlerExpr = {
                   updateCheckboxGroupInput(session = session,
                                            inputId = "LAYr",
                                            selected = unique(bnch_data$`Local Authority`)
                   )
                 }
    )
    
    observe({
      if(input$LAYrClear >0){
        updateCheckboxGroupInput(session = session, 
                                 inputId = "LAYr",
                                 selected = character(0))
      }
      
    })
    
  
    output$indicatorYr <- renderUI({
      bnch_data_subset <- filter(bnch_data, Domain == input$categoryYr)
      selectInput("indicatorYrSrv", "Please Select Indicator", unique(bnch_data_subset$Title))
    })
    
    bnch_data_indiYR <- reactive({
      dta <- filter(excl_Scotland, Title == input$indicatorYrSrv)
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
      dat <- filter(dat,Local_Authority %in% input$LAYr)
      if(input$RelVal == FALSE){
      ggplot(data = dat, aes(x = Local_Authority, y = Diffdata)) +
        geom_bar(stat = "identity", position= "dodge", fill = "darkblue")+
        theme_bw()+
        ylab(paste("Change from", as.character(input$baseYrSrv), "to", as.character(input$compYrSrv))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
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
                                            selected = if(input$FmlyGrpYr == "All"){
                                              unique(bnch_data$`Local Authority`)
                                            } else{
                                              unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpYr)[[1]])
                                            }
    )
                 }
    )
    
## By Council Tab
  #create checkbox for selecting year, only shows years that are available for the domain selected
    output$seriesCNCL <- renderUI({
      DtaCNCL <- filter(excl_Scotland, `Local Authority` %in% input$LA_CNCL & Domain %in% input$categoryCNCL)
      checkboxGroupInput("TSeriesCNCL", "Select Time Series", unique(DtaCNCL$Time), selected = NULL, inline = TRUE) 
    })
    
    #create a reactive function to store time series choices available    
   TDtaCNCL <- reactive({
     dtaCNCL <- filter(excl_Scotland, `Local Authority` %in% input$LA_CNCL & Domain %in% input$categoryCNCL)
      TChoicesCNCL <- unique(dtaCNCL$Time)
    })   
    
    #create buttons to select all or clear all in time series
    observeEvent(eventExpr = input$SeriesCNCLALL,
                 handlerExpr = {
                   updateCheckboxGroupInput(session = session,
                                            inputId = "TSeriesCNCL",
                                            selected = TDtaCNCL())
                 }
    )
    
    observe({
      if(input$SeriesCNCLClear >0){
        updateCheckboxGroupInput(session = session, 
                                 inputId = "TSeriesCNCL",
                                 selected = character(0))
      }
      
    }) 
    
    
    
    
    
  #calculate median, minimum and maximum values for each indicator for each year
    StatVals <- ddply(excl_Scotland,. (Time, Title), transform, Minimum = min(Value, na.rm = TRUE),
                      Maximum = max(Value, na.rm = TRUE), Median = median(Value, na.rm = TRUE))
    
  #split data by whether one is high
    OneIsHigh <- filter(excl_Scotland, `One is high` == "Yes")
    OneIsLow <- filter(excl_Scotland, `One is high` == "No")
  #calculate rankings
    RankHigh <- ddply(OneIsHigh,. (Time, Title), transform, Ranking = rank(-Value, ties.method = "max"))
    RankLow <- ddply(OneIsLow,. (Time, Title), transform, Ranking = rank(Value, ties.method = "max"))
    Rankings <- rbind(RankHigh, RankLow)
    
  #add the min, max and med values to the dataset
    SumStat <- left_join(StatVals, Rankings)
  #select only the columns of the data needed
    SumStat <- select(SumStat, Local.Authority, Value, Time, Title, Domain, Minimum, Maximum, Median, Ranking)
    
  #filter previous data to show only Scotland Values, add these to the new dataframe as a new column "Scotland Values"
    Scotland_subset <- filter(bnch_data, `Local Authority` == "Scotland")
    Scotland_subset <- select(Scotland_subset, Value, Time, Title, Domain)
    colnames(Scotland_subset)[1] <- "Scotland Value"
    SumStat <- left_join(SumStat, Scotland_subset)
  #order columns 
    SumStat <- SumStat[,c(1,5,4,3,2,6,7,8,10,9)]
    colnames(SumStat)[3] <- "Indicator"
   
   
  #create a reactive function to filter the new data set to only show what is selected for local authority and domain  
    SelectedDtaCNCL <- reactive({
      CNCLdta <- filter(SumStat, Local.Authority %in% input$LA_CNCL & Domain %in% input$categoryCNCL & Time %in% input$TSeriesCNCL)
    })
    
   #create a table which displays all of the values   
      output$CnclTbl <- renderDataTable({
        SelectedDtaCNCL <- SelectedDtaCNCL()
        SelectedDtaCNCL <- select(SelectedDtaCNCL, -Local.Authority, -Domain)
        SelectedDtaCNCL <- arrange(SelectedDtaCNCL, Indicator, Time)
     
       datatable(SelectedDtaCNCL) %>%
       formatRound(c(3:7), digits = 1)
       
      })
      
    #add a title above the table
      output$TableTitle <- renderText({
        paste(input$LA_CNCL,":",input$categoryCNCL)
      })
 })



