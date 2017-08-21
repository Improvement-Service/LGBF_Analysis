shinyServer(function(input, output, session) {
  
  output$indicator <- renderUI({
    bnch_data_subset <- filter(excl_Scotland, Domain == input$category)
    selectInput("indicator2", "Please Select Indicator", sort(unique(bnch_data_subset$Title)))
    })
  
  output$series <- renderUI({
    bnch_data_indi <- filter(excl_Scotland, Title == input$indicator2)
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
  
SelectedDta <- reactive({
  dta <- filter(excl_Scotland, `Local Authority` %in% input$LA & Time %in% input$TSeries & Title %in% input$indicator2)
})

#Calculates median values for each year group selected, based on the indicator selected and the authorities selected
MedFun <- reactive({
  SelectedDta <- SelectedDta()
  MedianVal <- round(ave(SelectedDta$Value, as.factor(SelectedDta$Time), FUN = median))
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
            axis.text.x = element_text(angle = 90, hjust = 1))+
      guides(fill = FALSE)
            
  })
#Create Ui Outputs for year on year section======================================    
    output$indicatorYr <- renderUI({
      bnch_data_subset <- filter(bnch_data, Domain == input$categoryYr)
      selectInput("indicatorYrSrv", "Please Select Indicator", sort(unique(bnch_data_subset$Title)))
    })
    
    bnch_data_indiYR <- reactive({
      dta <- filter(bnch_data, Title == input$indicatorYrSrv)
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
                                            selected = if(input$FmlyGrpYr == "All"){
                                              unique(bnch_data$`Local Authority`)
                                            } else{
                                              unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpYr)[[1]])
                                            }
    )
                 }
    )
    


##Create outputs for Dispersion Page ========================
output$indicatorDisp <- renderUI({
  bnch_data_subset <- filter(excl_Scotland, Domain == input$categoryDisp)
  selectInput("indicator2Disp", "Please Select Indicator", sort(unique(bnch_data_subset$Title)))
})
output$seriesDisp <- renderUI({
  bnch_data_indi <- filter(excl_Scotland, Title == input$indicator2Disp)
  checkboxGroupInput("TSeriesDisp", "Select Time Series", unique(bnch_data_indi$Time), selected = unique(bnch_data_indi$Time)) 
})

observeEvent(eventExpr = input$FmlyGrp2Disp,
             handlerExpr = {
               updateCheckboxGroupInput(session = session,
                                        inputId = "LADisp",
                                        selected = if(input$FmlyGrpDisp == "All"){
                                          unique(excl_Scotland$`Local Authority`)} 
                                        else{
                                          unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpDisp))[[1]] 
                                        }
                            )
                  }
            )
  #generate tables and graphs
  output$tableDisp <- DT::renderDataTable({
    dta <- filter(excl_Scotland, `Local Authority` %in% input$LADisp & Title == input$indicator2Disp & Time %in% input$TSeriesDisp)[c(1,3,4,15)]
    dta$Value <- round(dta$Value, 2)
    if(dta$`One is high` == "Yes"){
      brks <- quantile(dta$Value, probs = seq(0, 1, 0.25), na.rm = TRUE)
      clrs <- brewer.pal(length(brks) +1, "Blues")
      txtbrks <- quantile(dta$Value, probs = c(0,0.75), na.rm = TRUE)
      txtclrs <- c("black", "black", "white")
    }else{
      brks <- quantile(dta$Value, probs = seq(0, 1, 0.25), na.rm = TRUE)
      clrs <- rev(brewer.pal(length(brks) +1, "Blues"))
      txtbrks <- quantile(dta$Value, probs = c(0,0.75), na.rm = TRUE)
      txtclrs <- c("white", "black", "black")
    }
    dta <- spread(dta[c(1,2,3)], key = Time, value = Value)
    tbl <- datatable(dta, class = "row-border",extensions = c("Scroller", "FixedColumns"), rownames = FALSE, 
                     options = list(pageLength = 32, scrollY = 700, dom = "t", 
                  scrollX = TRUE, fixedColumns = list(leftColumns = 1))) %>%
      formatStyle(names(dta)[2:ncol(dta)], color = styleInterval(txtbrks, txtclrs),
                  backgroundColor = styleInterval(brks, clrs), lineHeight = "40%")
  })
  
  output$boxDisp <- renderPlot({
    bpdta <- filter(excl_Scotland, `Local Authority` %in% input$LADisp & Title == input$indicator2Disp & Time %in% input$TSeriesDisp)
    ggplot(data = bpdta, aes(x = Time, y = Value)) +
      geom_boxplot() +
      theme_bw()
  })

##Create outputs for Tme Series Page ========================
output$indicatorTSD <- renderUI({
    bnch_data_subset <- filter(excl_Scotland, Domain == input$categoryTSD)
    selectInput("indicator2TSD", "Please Select Indicator", sort(unique(bnch_data_subset$Title)))
  })
output$seriesDisp <- renderUI({
    bnch_data_indi <- filter(excl_Scotland, Title == input$indicator2TSD)
    checkboxGroupInput("TSeriesTSD", "Select Time Series", unique(bnch_data_indi$Time), selected = unique(bnch_data_indi$Time)) 
  })
  
observeEvent(eventExpr = input$FmlyGrp2TSD,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                   inputId = "LATSD",
                                  selected = if(input$FmlyGrpTSD == "All"){
                                            unique(excl_Scotland$`Local Authority`)} 
                                          else{
                                            unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpTSD))[[1]] 
                                          }
                 )
               }
  )
})