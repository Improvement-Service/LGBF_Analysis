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
    awesomeCheckboxGroup("TSeries", "", unique(bnch_data_indi$Year), selected = unique(bnch_data_indi$Year)) 
 })

#create a reactive function to store time series choices available    
 TDta <- reactive({
   dta <- filter(excl_Scotland, Title == input$indicator2)
   TChoices <- unique(dta$Year)
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
                                            else if(input$category %in% c("Children's Services", "Adult Social Care", "Housing Services")){
                                              unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrp))[[1]] 
                                            } else{
                                              unique(filter(excl_Scotland, `Family group (Other)` %in% input$FmlyGrp))[[1]]
                                            }
                 )
               }
  )
  
SelectedDta <- reactive({
  dta <- filter(excl_Scotland, `Local Authority` %in% input$LA & Year %in% input$TSeries & Title %in% input$indicator2)
})

#Calculates median values for each year group selected, based on the indicator selected and the authorities selected
MedFun <- reactive({
  SelectedDta <- SelectedDta()
  MedianVal <- round(ave(SelectedDta$Value, as.factor(SelectedDta$Year), FUN = function(x){median(x, na.rm = TRUE)}))
})

  output$PlotTitle <- renderText({
    paste("",input$indicator2)
  })

    output$plot1 <- renderPlotly({
    colnames(excl_Scotland)[1] <- "Local_Authority"
    excl_Scotland <- filter(excl_Scotland, Local_Authority %in% input$LA & Year %in% input$TSeries)
    p <- ggplot(excl_Scotland[excl_Scotland$Title == input$indicator2,])+
      geom_bar(aes(x = Local_Authority, y = Value, fill = Year, 
                    text = paste("Local Authority:", `Local_Authority`, "<br>", "Year:", `Year`,
                                 "<br>", "Value:", `Value`)), colour = "black",position = "dodge", stat = "identity")+
      theme_bw()+
      xlab("")+ylab("")+
      scale_y_continuous(expand = c(0,0))+
      geom_hline(aes(yintercept = MedFun(), colour = Year))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.x = element_text(angle = 90, hjust = 5, vjust = 2, size = 14))+
      guides(fill = FALSE)
    ggplotly(p, tooltip = c("text")) %>% hide_legend()
            
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
    
  

#Create Ui Outputs for year on year section======================================    

    output$indicatorYr <- renderUI({
      bnch_data_subset <- filter(bnch_data, Domain == input$categoryYr)
      selectInput("indicatorYrSrv", "Please Select Indicator", sort(unique(bnch_data_subset$Title)), width = "100%")
    })
    
    bnch_data_indiYR <- reactive({
      dta <- filter(bnch_data, Title == input$indicatorYrSrv)
    })
    
    output$baseYr <- renderUI({
      bnch_data_indiYR <- bnch_data_indiYR()
  #selectizeInput lets you have it start blank instead of
  #selecting the first value - as selectInput does!
      selectizeInput("baseYrSrv", "Start Year", 
                     choices = unique(bnch_data_indiYR$Year),
                    options = list(
                      placeholder = "Select Start Year",
                      onInitialize = I('function() {this.setValue("");}')
                    )
               ) 
    })
    output$compYr <- renderUI({
      bnch_data_indiYR <- bnch_data_indiYR()
      selectizeInput("compYrSrv", "End Year:", 
                     choices = c(unique(bnch_data_indiYR[bnch_data_indiYR$Year != input$baseYrSrv, "Year"])),
      options = list(
        placeholder = "Select End Year",
        onInitialize = I('function() {this.setValue("");}')
      ))
    })
#calculate changes for each LA between base and comparator year
    chngDta <- reactive({
      data <- bnch_data_indiYR()
  #only keep selected years
      yrs <- c(input$compYrSrv, input$baseYrSrv)
      data <- data[data$Year %in% yrs,]
    #Now calculate higher year (x[2]) minus lower year (x[1])
      Diffdata <- round(ave(data$Value, as.factor(data$`Local Authority`), 
                  FUN = function(x){x[2] -x[1]}), 1)[1:33]
    #This one calculates % change  
      PerDiffdata <- round(ave(data$Value, as.factor(data$`Local Authority`), 
             FUN = function(x){(x[2]/x[1]-1)*100}), 1)[1:33]
      data <- data.frame( data$`Local Authority`[1:33], Diffdata, PerDiffdata,
                    stringsAsFactors = FALSE)
    })
    output$`Year-on-Year-Plot` <-renderPlotly({
      dat <- chngDta()
      colnames(dat)[1] <- "Local_Authority"
      dat <- filter(dat,Local_Authority %in% input$LAYr)
      if(input$RelVal == FALSE){
      pp <- ggplot(data = dat, aes(x = Local_Authority, y = Diffdata)) +
        geom_bar(stat = "identity", position= "dodge", fill = "darkblue")+
        theme_bw()+
        xlab("")+
        ylab(paste("Change", as.character(input$baseYrSrv), "to", as.character(input$compYrSrv))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"))
      } else{
        pp <- ggplot(data = dat, aes(x = Local_Authority, y = PerDiffdata)) +
          geom_bar(stat = "identity", position= "dodge", fill = "darkblue")+
          theme_bw()+
          xlab("")+
          ylab(paste("Percentage Change", as.character(input$baseYrSrv), "to", as.character(input$compYrSrv))) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"))
      }
      ggplotly(pp)
    })
    
    observeEvent(eventExpr = input$FmlyGrp2Yr,
                 handlerExpr = {
                   updateCheckboxGroupInput(session = session,
                                            inputId = "LAYr",
                                            selected = if(input$FmlyGrpYr == "All"){
                                              unique(bnch_data$`Local Authority`)
                                            } else if(input$categoryYr %in% c("Children's Services", "Adult Social Care", "Housing Services")){
                                              unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpYr))[[1]] 
                                            } else{
                                              unique(filter(excl_Scotland, `Family group (Other)` %in% input$FmlyGrpYr))[[1]]
                                            }
    )
                 }
    )
    

## By Council Tab
  #create checkbox for selecting year, only shows years that are available for the domain selected
    output$seriesCNCL <- renderUI({
      DtaCNCL <- filter(excl_Scotland, `Local Authority` %in% input$LA_CNCL & Domain %in% input$categoryCNCL)
      awesomeCheckboxGroup("TSeriesCNCL", "Select Time Series", unique(DtaCNCL$Year), selected = unique(DtaCNCL$Year), inline = TRUE) 
    })
    
    #create a reactive function to store time series choices available    
   TDtaCNCL <- reactive({
     dtaCNCL <- filter(excl_Scotland, `Local Authority` %in% input$LA_CNCL & Domain %in% input$categoryCNCL)
      TChoicesCNCL <- unique(dtaCNCL$Year)
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
    StatVals <- ddply(excl_Scotland,. (Year, Title), transform, Minimum = min(Value, na.rm = TRUE),
                      Maximum = max(Value, na.rm = TRUE), Median = median(Value, na.rm = TRUE))
    
  #split data by whether one is high
    OneIsHigh <- filter(excl_Scotland, `One is high` == "Yes")
    OneIsLow <- filter(excl_Scotland, `One is high` == "No")
  #calculate rankings
    RankHigh <- ddply(OneIsHigh,. (Year, Title), transform, Ranking = rank(-Value, ties.method = "max"))
    RankLow <- ddply(OneIsLow,. (Year, Title), transform, Ranking = rank(Value, ties.method = "max"))
    Rankings <- rbind(RankHigh, RankLow)
    
  #add the min, max and med values to the dataset
    SumStat <- left_join(StatVals, Rankings)
  #select only the columns of the data needed
    SumStat <- select(SumStat, Local.Authority, Value, Year, Title, Domain, Minimum, Maximum, Median, Ranking)
    
  #filter previous data to show only Scotland Values, add these to the new dataframe as a new column "Scotland Values"
    Scotland_subset <- filter(bnch_data, `Local Authority` == "Scotland")
    Scotland_subset <- select(Scotland_subset, Value, Year, Title, Domain)
    colnames(Scotland_subset)[1] <- "Scotland Value"
    SumStat <- left_join(SumStat, Scotland_subset)
  #order columns 
    SumStat <- SumStat[,c(1,5,4,3,2,6,7,8,10,9)]
    colnames(SumStat)[3] <- "Indicator"

  #create a reactive function to filter the new data set to only show what is selected for local authority and domain  
    SelectedDtaCNCL <- reactive({
      CNCLdta <- filter(SumStat, Local.Authority %in% input$LA_CNCL & Domain %in% input$categoryCNCL & Year %in% input$TSeriesCNCL)
      })
    
   #create a table which displays all of the values   
      cnclTblOut <- function(){
        SelectedDtaCNCL <- SelectedDtaCNCL()
        SelectedDtaCNCL <- select(SelectedDtaCNCL, -Local.Authority, -Domain)
        SelectedDtaCNCL <- arrange(SelectedDtaCNCL, Indicator, Year)
        indis <- unique(SelectedDtaCNCL$Indicator)
        lstGrps <- c()
        for(i in 1:length(indis)){
          rws <- sum(SelectedDtaCNCL$Indicator == indis[i])
          names(rws) <- indis[i]
          lstGrps <- c(lstGrps, rws)
        }
        grph <- select(SelectedDtaCNCL, c("Year","Indicator", "Ranking")) %>%
          mutate(maxt = 32) %>%
          gather(Indi, values, c("Ranking", "maxt")) %>%
          group_by(Indicator, Year) %>%
          dplyr::summarise(grphs = spk_chr(
            values, type = "bullet", width = "100"
          ))
        grph <- dplyr::arrange(grph, Indicator, Year)
        SelectedDtaCNCL$`Ranked Position` <- grph$grphs
        
        format_table(SelectedDtaCNCL[-1], align = "c") %>%
          group_rows(index = lstGrps) %>%
          htmltools::HTML() %>%
          shiny::div() %>%
          sparkline::spk_add_deps()
      }
    
      output$CnclTbl <- renderUI({
        cnclTblOut()
        })
      
    #add a title above the table
      output$TableTitle <- renderText({
        paste(input$LA_CNCL,":",input$categoryCNCL)
      })
 


##Create outputs for Dispersion Page ========================
output$indicatorDisp <- renderUI({
  bnch_data_subset <- filter(excl_Scotland, Domain == input$categoryDisp)
  selectInput("indicator2Disp", "Please Select Indicator", sort(unique(bnch_data_subset$Title)), width = "100%")
})
      
#create buttons to select all or clear all in time series
  observeEvent(eventExpr = input$LADispAll,
                   handlerExpr = {
                     updateCheckboxGroupInput(session = session,
                                              inputId = "LADisp",
                                              selected = unique(excl_Scotland$'Local Authority'))
                   }
      )
      
      observe({
        if(input$LADispClear >0){
          updateCheckboxGroupInput(session = session, 
                                   inputId = "LADisp",
                                   selected = character(0))
        }
        
      })       
    
output$seriesDisp <- renderUI({
  bnch_data_indi <- filter(excl_Scotland, Title == input$indicator2Disp)
  awesomeCheckboxGroup("TSeriesDisp", "", unique(bnch_data_indi$Year), selected = unique(bnch_data_indi$Year)) 
})

#create reactive funciton to store time series choices available
TChoicesDisp <- reactive({
  dta <- filter(excl_Scotland, Title == input$indicator2Disp)
  Tchoices <- unique(dta$Year)
})

#create buttons to select all or clear all in time series
observeEvent(eventExpr = input$seriesDispAll,
             handlerExpr = {
               updateCheckboxGroupInput(session = session,
                                        inputId = "TSeriesDisp",
                                        selected = TChoicesDisp())
             }
)

observe({
  if(input$seriesDispClear >0){
    updateCheckboxGroupInput(session = session, 
                             inputId = "TSeriesDisp",
                             selected = character(0))
  }
  
}) 

observeEvent(eventExpr = input$FmlyGrp2Disp,
             handlerExpr = {
               updateCheckboxGroupInput(session = session,
                                        inputId = "LADisp",
                                        selected = if(input$FmlyGrpDisp == "All"){
                                          unique(excl_Scotland$`Local Authority`)} 
                                        else if(input$categoryDisp %in% c("Children's Services", "Adult Social Care", "Housing Services")){
                                          unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpDisp))[[1]] 
                                        } else{
                                          unique(filter(excl_Scotland, `Family group (Other)` %in% input$FmlyGrpDisp))[[1]]
                                        }
                            )
                  }
            )
  #generate tables and graphs
  output$tableDisp <- DT::renderDataTable({
    dta <- filter(excl_Scotland, `Local Authority` %in% input$LADisp & Title == input$indicator2Disp & Year %in% input$TSeriesDisp)[c(1,3,4,15)]
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
    dta <- spread(dta[c(1,2,3)], key = Year, value = Value)
    tbl <- datatable(dta, class = "row-border",extensions = c("Scroller", "FixedColumns"), rownames = FALSE, 
                     options = list(pageLength = 32, scrollY = 720, dom = "t", 
                  scrollX = TRUE, fixedColumns = list(leftColumns = 1))) %>%
      formatStyle(names(dta)[2:ncol(dta)], color = styleInterval(txtbrks, txtclrs),
                  backgroundColor = styleInterval(brks, clrs), lineHeight = "50%") %>%
      formatStyle(colnames(dta), fontSize = "110%")
  })
  
  output$boxDisp <- renderPlot({
    bpdta <- filter(excl_Scotland, `Local Authority` %in% input$LADisp & Title == input$indicator2Disp & Year %in% input$TSeriesDisp)
    ggplot(data = bpdta, aes(x = Year, y = Value)) +
      geom_violin(draw_quantiles = c(0.5), colour = "black") +
      theme_bw() +
      theme(panel.grid.major = element_blank())
  })

##Create outputs for Tme Series Page ========================
  observeEvent(eventExpr = input$LATSDAll,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                          inputId = "LATSD",
                                          selected = unique(excl_Scotland$'Local Authority'))
               }
  )
  
  observe({
    if(input$LATSDClear >0){
      updateCheckboxGroupInput(session = session, 
                               inputId = "LATSD",
                               selected = character(0))
    }
    
  }) 
  
  
  output$indicatorTSD <- renderUI({
    bnch_data_subset <- filter(excl_Scotland, Domain == input$categoryTSD)
    selectInput("indicator2TSD", "Please Select Indicator", sort(unique(bnch_data_subset$Title)), width = "100%")
  })
output$seriesTSD <- renderUI({
    bnch_data_indi <- filter(excl_Scotland, Title == input$indicator2TSD)
    awesomeCheckboxGroup("TSeriesTSD", "", unique(bnch_data_indi$Year), selected = unique(bnch_data_indi$Year)) 
  })

#create reactive funciton to store time series choices available
TChoicesTSD <- reactive({
  dta <- filter(excl_Scotland, Title == input$indicator2TSD)
  Tchoices <- unique(dta$Year)
})

#create buttons to select all or clear all in time series
observeEvent(eventExpr = input$seriesTSDAll,
             handlerExpr = {
               updateCheckboxGroupInput(session = session,
                                        inputId = "TSeriesTSD",
                                        selected = TChoicesTSD())
             }
)

observe({
  if(input$seriesTSDClear >0){
    updateCheckboxGroupInput(session = session, 
                             inputId = "TSeriesTSD",
                             selected = character(0))
  }
  
}) 
  
observeEvent(eventExpr = input$FmlyGrp2TSD,
               handlerExpr = {
                 updateCheckboxGroupInput(session = session,
                                   inputId = "LATSD",
                                  selected = if(input$FmlyGrpTSD == "All"){
                                            unique(excl_Scotland$`Local Authority`)} 
                                  else if(input$categoryTSD %in% c("Children's Services", "Adult Social Care", "Housing Services")){
                                    unique(filter(excl_Scotland, `Family group (People)` %in% input$FmlyGrpTSD))[[1]] 
                                  } else{
                                    unique(filter(excl_Scotland, `Family group (Other)` %in% input$FmlyGrpTSD))[[1]]
                                  }
                 )
               }
  )
TSDData <- reactive({
  dta <- filter(excl_Scotland, `Local Authority` %in% input$LATSD & Title == input$indicator2TSD & Year %in% input$TSeriesTSD)[c(1,3:4, 15)]
  })
output$TSDTable1 <- renderDataTable({
  dta <- TSDData()[2:3]
  p <- dta %>% group_by(Year) %>%
    summarise_at(., vars(Value), funs(mean, min, max, median, sd), na.rm =TRUE) %>%
    mutate_at(., vars(mean:sd), funs(round), digits = 2)
  colnames(p)[6] <- c("Standard Deviation")
  datatable(p,  extensions = "Scroller",
            options = list(pageLength = 8, scrollY = 150, dom = "t", rownames = FALSE))
  })



output$TSDTable2 <- renderDataTable({
  dta <- TSDData()
  yrs <- input$TSeriesTSD
##this creates the custom headers for the table  
  cont = htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, "Local Authority"),
        lapply(yrs, th, colspan = 4, class = 'dt-center')
      ),
      tr(
        lapply(rep(c("Value", "Change", "Rank", "Rank Change"), length(yrs)), th)
      )
    )
   )
  )
    dta$Value <- round(dta$Value,2)
#calculate ranks by year
  if("no" %in% dta$`One is high`){
  dta$rank <- ave(dta$Value, dta$Year, FUN = function(x) rank(x, ties.method = "first"))
  } else{
  dta$rank <- ave(dta$Value, dta$Year, FUN = function(x) rank(-x, ties.method = "first"))
  }
  dta$rankMov<- ave(dta$rank, dta$`Local Authority`, FUN = function(x) {x - lag(x,1)})
  dta$valMov<- round(ave(dta$Value, dta$`Local Authority`, FUN = function(x) {x - lag(x,1)}),2)
  dta <- dcast(setDT(dta), `Local Authority`~Year, value.var = c("Value","valMov","rank", "rankMov"))
#Sort by year  
  lng <- length(yrs) +1
  colNums <- c(1)
  for(i in 2:lng){
    tmp <- seq(from  = i, to = ncol(dta), by = length(yrs))
    colNums <- c(colNums, tmp)
  }
  srtDta <- dta %>% select(colNums)
  datatable(srtDta, extensions = "Scroller", options = list(pageLength = 32, scrollX = TRUE,
                          scrollY = 400, dom = "t"),container = cont, rownames = FALSE) %>%
    formatStyle(c(5,9,13,17,21,25), `border-right` = "1px solid black")
  })

##Inputs for ranking Page
output$indicatorRank <- renderUI({
  bnch_data_subset <- filter(excl_Scotland, Domain == input$categoryRank)
  selectInput("indiRank", "Select Indicator", sort(unique(bnch_data_subset$Title)), width = "90%") 
  })
##Producing Graph for Ranking page
output$rankPlot <- renderPlotly({
  dtaRnk <- filter(excl_Scotland, Title == input$indiRank)
  dtaRnk$selection <- ifelse(dtaRnk$`Local Authority` == input$RnkLA, "Yes", "No")
  if("no" %in% dtaRnk$`One is high`){
    dtaRnk$ranks <- ave(dtaRnk$Value, dtaRnk$Year, FUN = function(x) rank(x, ties.method = "first"))
  }
  else{
    dtaRnk$ranks <- ave(dtaRnk$Value, dtaRnk$Year, FUN = function(x) rank(-x, ties.method = "first"))
  }
  selDta <- ifelse(input$ValRank == FALSE, "ranks","Value")
  colnames(dtaRnk)[1] <- "Local_Authority"
  grp <- ggplot(data = dtaRnk) +
    geom_line(aes(x = Year, y = !!ensym(selDta), group = `Local_Authority`, colour = selection, size = selection,
                         text = paste(" Local Authority: ", `Local_Authority`,"<br>",
                                      "Year: ", `Year`,"<br>" , "Rank: ", `ranks`,"<br>", "Value: ", `Value`)), na.rm = TRUE) +
    {if(input$ValRank== FALSE)
      scale_y_reverse(lim = c(32,1), breaks = seq(1,32, 6)) 
    }+
    theme_bw()+
    guides(colour = FALSE)+
    scale_x_discrete(expand = c(0.001,0.01))+
  scale_size_manual(breaks = c("Yes", "No"), values = c(0.4,1.5))+
    scale_colour_manual(breaks = c("Yes", "No"), values = c("grey","red"))
  ggplotly(grp, tooltip = c("text")) %>% layout(showlegend = FALSE)
  })

  output$tbSv <- downloadHandler(
    filename = function(){paste0(input$LA_CNCL, "_",input$categoryCNCL,".pdf")},
    content = function(con){
      export_formattable(cnclTblOut(), con)
          }
  )
  
  selectedCat <- reactiveValues(catg=NULL)
  observeEvent(input$category, selectedCat$catg <- input$category)
  observeEvent(input$categoryYr, selectedCat$catg <- input$categoryYr)
  observeEvent(input$categoryCNCL, selectedCat$catg <- input$categoryCNCL)
  observeEvent(input$categoryDisp, selectedCat$catg <- input$categoryDisp)
  observeEvent(input$categoryTSD, selectedCat$catg <- input$categoryTSD)
  observeEvent(input$categoryRank, selectedCat$catg <- input$categoryRank)
  
  observeEvent(selectedCat$catg, updateSelectInput(session,"category", selected = selectedCat$catg))
  observeEvent(selectedCat$catg, updateSelectInput(session,"categoryYr", selected = selectedCat$catg))
  observeEvent(selectedCat$catg, updateSelectInput(session,"categoryCNCL", selected = selectedCat$catg))
  observeEvent(selectedCat$catg, updateSelectInput(session,"categoryDisp", selected = selectedCat$catg))
  observeEvent(selectedCat$catg, updateSelectInput(session,"categoryTSD", selected = selectedCat$catg))
  observeEvent(selectedCat$catg, updateSelectInput(session,"categoryRank", selected = selectedCat$catg))
  
  })