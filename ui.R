shinyUI(navbarPage(id = "pageList",
                   
  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
header = ##Some css
  tags$head(tags$style(
    "#plot1 {height:75vh !important; border-right:0px; padding-right:0px}",
    "#Year-on-Year-Plot {height:75vh !important}",
    "#CnclTbl {height:75vh !important}",
    ".form-group.shiny-input-container.shiny-input-checkboxgroup.shiny-bound-input{margin-top:-10px; font-size: 1em}",
    ".well{padding-left:5px; padding-top:5px}"
  )),
  tabPanel("By Indicator",
      fluidPage(
        fluidRow(
          column(4,
      selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain),
                          selected = "Adult Social Care")
      ),
      column(6,
                  uiOutput("indicator")
        )
      ),
      fluidRow(
    sidebarPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto;",
      h5("Select Local Authority"),
      div(class = "multicol",style = "column-count:2;-webkit-column-count:2; -moz-column-count:2; margin-top:0px",
        awesomeCheckboxGroup("LA", label = "", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$`Local Authority`))),
      actionBttn("LAAll", "Select All"),
      actionBttn("LAClear", "Clear All"),
      uiOutput("series"),
      actionBttn("SeriesAll", "Select All"),
      actionBttn("SeriesClear", "Clear All"),
      awesomeRadio("FmlyGrp", "Select Family Group", c("All",1,2,3,4), inline = TRUE),
      actionBttn("FmlyGrp2", "Update Family Group"), width = 4
    ),
    mainPanel(
      tags$b(uiOutput("PlotTitle")),
      plotlyOutput("plot1"), width = 8
    )
      )
    )),

##New tab for year on year changes
  tabPanel("Year-on-Year Change",
#some css
  tags$head(tags$style(
#    ".chckBx {column-count:2;-webkit-column-count:2; -moz-column-count:2}"
      HTML("div.checkbox {margin-top: 0px;}")
     )),
    fluidPage(
        fluidRow(
        column(4,
          selectInput("categoryYr", "Select Indicator Category", unique(excl_Scotland$Domain),
                        selected = "Adult Social Care")
                   ),
        column(6,
          uiOutput("indicatorYr")
          )
          ),
     fluidRow( 
          sidebarPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto;",
                       h5("Select Local Authority"),
                       div(class = "multicol",style = "column-count:2;-webkit-column-count:2; -moz-column-count:2; margin-top:0px",
                         awesomeCheckboxGroup("LAYr", label = "", unique(bnch_data$`Local Authority`), selected = bnch_data$`Local Authority`)),
                        actionBttn("LAYrAll", "Select All"),
                       actionBttn("LAYrClear", "Clear All"),
                        uiOutput("baseYr"),
                        uiOutput("compYr"),
                        radioButtons("FmlyGrpYr", "Select Family Group", c("All",1,2,3,4), inline = TRUE),
                        actionBttn("FmlyGrp2Yr", "Update Family Group"),
                        awesomeCheckbox("RelVal", "Show Percentage Change", value = FALSE)
        ),
           mainPanel(
             plotlyOutput("Year-on-Year-Plot")
           )
      )
    )
  ),

##new tab for By Council
tabPanel("By Council",
    
            fluidPage(
              fluidRow(
                column(6,
           selectInput("categoryCNCL", "Select Indicator Category", unique(excl_Scotland$Domain),
                               selected = "Adult Social Care", width = "100%")
           ),
           column(6,
                   selectInput("LA_CNCL", "Select Local Authority", unique(excl_Scotland$`Local Authority`), selected = "Aberdeen City", width = "100%")
           ),
           column(12,
                   uiOutput("seriesCNCL")
                  
           ),
           column(1, style = "margin-left:5px;padding:0px; max-width:auto",
                  actionBttn("SeriesCNCLALL", "Select All")
           ),
           column(1, style = "max-width:auto",
                  actionBttn("SeriesCNCLClear", "Clear All")
           )
           ),
             fluidRow(
               column(4,
           tags$b(uiOutput("TableTitle"))
           ),
           column(12,
           DT::dataTableOutput("CnclTbl")
               )
         )
         
            )

),

#New Tab for Dispersion ====================================
    tabPanel("Dispersion",
             wellPanel(
               fluidPage(
                 fluidRow(
                  column(6, 
                     selectInput("categoryDisp", "Select Indicator Category", unique(excl_Scotland$Domain),
                                   selected = "Children's Services")
                   ),
                  column(6,
                       uiOutput("indicatorDisp")
                       )
               ), width = "100%"
             )),   
        fluidRow(
             column(4,
                    wellPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto; margin-right:1px; padding-right:1px; margin-left:5px",
                          h5("Select Local Authority"),
                          div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                              awesomeCheckboxGroup("LADisp", label = "", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                          ),
                          actionBttn("LADispAll", "Select All"),
                          actionBttn("LADispClear", "Clear All"),
                          h5("Select time series"),
                          uiOutput("seriesDisp"),
                          actionBttn("seriesDispAll", "Select All"),
                          actionBttn("seriesDispClear", "Clear All"),
                          radioButtons("FmlyGrpDisp", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                          actionBttn("FmlyGrp2Disp", "Update Family Group")
             )
             ),
             column(8,
                    mainPanel(id = "mainDisp", style = "padding-left:1px; margin-left:1px",
                 splitLayout(
                   cellWidths = c("70%", "80%"),
                 div(DT::dataTableOutput("tableDisp"),style = "font-size:74%; line-height:40%"),
                 plotOutput("boxDisp")
                 )
               )
             )
           )
        ),
#New Tab for Time Series Data
    tabPanel("Time Series Data",
             wellPanel(
               fluidPage(
                 fluidRow(
                  column(6,  
                      selectInput("categoryTSD", "Select Indicator Category", unique(excl_Scotland$Domain),
                                   selected = "Children's Services")
                   ),
                  column(6,
                       uiOutput("indicatorTSD")
                       ),
                    width = "100%"
             ))),             
      fluidRow(column(4,
                      wellPanel(style = "height:75vh;overflow-y:auto;",
                        h5("Select Local Authority"),
                        div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                            awesomeCheckboxGroup("LATSD", label = "", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                        ),
                        actionBttn("LATSDAll", "Select All"),
                        actionBttn("LATSDClear", "Clear All"),
                        h5("Select time series"),
                        uiOutput("seriesTSD"),
                        actionBttn("seriesTSDAll", "Select All"),
                        actionBttn("seriesTSDClear", "Clear All"),
                        radioButtons("FmlyGrpTSD", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                        actionBttn("FmlyGrp2TSD", "Update Family Group")
                      )
                      ),
               column(8,
                      dataTableOutput("TSDTable1"),
                      dataTableOutput("TSDTable2")))
    ),
##New Tab with Rankings
    tabPanel("Rankings Over Time",
             fluidPage(
               fluidRow(
                 column(6,  
                        selectInput("categoryRank", "Select Indicator Category", unique(excl_Scotland$Domain),
                                    selected = "Children's Services", width = "90%")
                 ),
                 column(6,
                        uiOutput("indicatorRank")
                 ),
                 width = "100%"
               ),
             
                 plotlyOutput("rankPlot", height = "80vh")
             )
      
    )
))

