shinyUI(navbarPage(id = "pageList",
                   
  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
header = ##Some css
  tags$head(tags$style(
    "#plot1 {height:40vh !important; border-right:0px; padding-right:0px}",
    "#Year-on-Year-Plot {height:78vh !important}",
    "#CnclTbl {height:78vh !important}",
    ".form-group.shiny-input-container.shiny-input-checkboxgroup.shiny-bound-input{margin-top:-10px; font-size: 1em}",
    ".well{padding-left:5px; padding-top:5px}",
    ".bttn-unite{margin-bottom:10px}",
    ".control-label{font-weight:bold; color:black}",
    "#TableTitle{font-weight:bold;color:black}",
    "#TSDTable1 th {text-align: center}",
    HTML("h5{font-weight:bold;color:black}
          .multico{
              padding-left:10px;
             -webkit-column-count: 5; 
             -moz-column-count: 5;   
         column-count: 5; 
         -moz-column-fill: auto;
          -column-fill: auto;
          }
         label{font-weight:bold; color:black}")
  )),
  tabPanel("All Councils By Indicator",
      fluidPage(
        fluidRow(
          column(4, style = "padding-left:10px",
      selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain))
      ),
      column(6,
                  uiOutput("indicator")
        )
      )
      ),
      fluidRow(
        mainPanel(
          tags$b(uiOutput("PlotTitle")),
          plotlyOutput("plot1", height = "200px"), width = 12
        ),
    wellPanel(id = "sidPnl", style = "overflow-y:auto;padding-left:10px",
      h5("Select Local Authorities"),
      tags$div(align = "left", class = "multico",
        awesomeCheckboxGroup("LA", label = "", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$`Local Authority`), inline = FALSE)),
      actionBttn("LAAll", "Select All",size = "sm"),
      actionBttn("LAClear", "Clear All",size = "sm"),
      hr(style = "margin-top:0px; margin-bottom:3px"),
      column(6,h5("Select years"),
      uiOutput("series", style = "margin-top:3px;margin-bottom:10px;"),
      actionBttn("SeriesAll", "Select All",size = "sm"),
      actionBttn("SeriesClear", "Clear All",size = "sm")),
      column(6,h5("Select Family Group"),
      awesomeRadio("FmlyGrp", "", c("All",1,2,3,4), inline = TRUE),
      actionBttn("FmlyGrp2", "Update Family Group",size = "sm"))
    )
    )
    ),

##New tab for year on year changes
  tabPanel("Year-on-Year Change",
#some css
  tags$head(tags$style(
#    ".chckBx {column-count:2;-webkit-column-count:2; -moz-column-count:2}"
      HTML("div.checkbox {margin-top: 0px;}")
     )),
    fluidPage(
        fluidRow(
        column(4, style = "padding-left:10px",
          selectInput("categoryYr", "Select Indicator Category", unique(excl_Scotland$Domain))
                   ),
        column(6,
          uiOutput("indicatorYr")
          )
          ),
     fluidRow( 
          sidebarPanel(id = "sidPnl", style = "height:78vh;overflow-y:auto;",
                       h5("Select Local Authority"),
                       div(class = "multicol",style = "column-count:2;-webkit-column-count:2; -moz-column-count:2; margin-top:0px",
                         awesomeCheckboxGroup("LAYr", label = "", unique(bnch_data$`Local Authority`), selected = bnch_data$`Local Authority`)),
                        actionBttn("LAYrAll", "Select All",size = "sm"),
                       actionBttn("LAYrClear", "Clear All",size = "sm"),
                        uiOutput("baseYr"),
                        uiOutput("compYr"),
                       awesomeRadio("FmlyGrpYr", "Select Family Group", c("All",1,2,3,4), inline = TRUE),
                        actionBttn("FmlyGrp2Yr", "Update Family Group",size = "sm"),
                        awesomeCheckbox("RelVal", "Show Percentage Change", value = FALSE)
        ),
           mainPanel(
             plotlyOutput("Year-on-Year-Plot")
           )
      )
    )
  ),

##new tab for By Council
tabPanel("All Indicators by Council",
    
            fluidPage(
              fluidRow(
                column(6,
           selectInput("categoryCNCL", "Select Indicator Category", unique(excl_Scotland$Domain),
                                width = "100%")
           ),
           column(6,
                   selectInput("LA_CNCL", "Select Local Authorities", unique(excl_Scotland$`Local Authority`), width = "80%")
                  ),
           column(10,
                   uiOutput("seriesCNCL")),
           column(2,
                  div(style = "float:right",switchInput("CNCLreal", onLabel = "Cash", offLabel = "Real", inline=TRUE, offStatus = "success", label = "<i class=\"glyphicon glyphicon-gbp\"></i>"))
           ),
           column(12,
                  div(style = "padding-right:8px;padding-left:4px;display:inline",actionBttn("SeriesCNCLALL", "Select All", size = "sm")),
          
                  actionBttn("SeriesCNCLClear", "Clear All", size = "sm"),
                  div(style = "float:right",downloadBttn("tbSv","Save this table", size = "sm"))
           )
           ),
             fluidRow(
               column(4,
           tags$b(uiOutput("TableTitle"))
           ),
           column(12,
              htmlOutput("CnclTbl")
               )
         )
         
            )

),

#New Tab for Dispersion ====================================
    tabPanel("Dispersion",
               fluidPage(
                 fluidRow(
                  column(4, 
                     selectInput("categoryDisp", "Select Indicator Category", unique(excl_Scotland$Domain))
                   ),
                  column(6,
                       uiOutput("indicatorDisp")
                       )
               )
             ),   
        fluidRow(
             column(4, style = "padding-left:10px",
                    wellPanel(id = "sidPnl", style = "height:80vh;overflow-y:auto; margin-right:1px; padding-right:1px",
                          h5("Select Local Authorities"),
                          div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2; margin-bottom:5px",
                              awesomeCheckboxGroup("LADisp", label = "", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                          ),
                          actionBttn("LADispAll", "Select All",size = "sm"),
                          actionBttn("LADispClear", "Clear All",size = "sm"),
                          h5("Select years"),
                          uiOutput("seriesDisp",style = "margin-top:10px;margin-bottom:5px;column-count:2;-webkit-column-count:2; -moz-column-count:2"),
                          actionBttn("seriesDispAll", "Select All",size = "sm"),
                          actionBttn("seriesDispClear", "Clear All",size = "sm"),
                          awesomeRadio("FmlyGrpDisp", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                          div(style = "margin-bottom:0px",actionBttn("FmlyGrp2Disp", "Update Family Group",size = "sm"))
             )
             ),
             column(8,
                    mainPanel(id = "mainDisp", style = "padding-left:1px; margin-left:1px",
           #      splitLayout(
          #         cellWidths = c("70%", "80%"),
                 div(DT::dataTableOutput("tableDisp"),style = "font-size:80%; line-height:80%; width:160%")
          #       plotOutput("boxDisp")
          #       )
               )
             )
           )
        ),
#New Tab for Time Series Data
    tabPanel("Indicators Over Time",
               fluidPage(
                 fluidRow(
                  column(4,  
                      selectInput("categoryTSD", "Select Indicator Category", unique(excl_Scotland$Domain))
                   ),
                  column(6,
                       uiOutput("indicatorTSD")
                       )
             )),             
      fluidRow(column(4, style = "padding-left:10px",
                      wellPanel(style = "height:80vh;overflow-y:auto; margin-right:1px; padding-right:1px",
                        h5("Select Local Authorities"),
                        div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                            awesomeCheckboxGroup("LATSD", label = "", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                        ),
                        actionBttn("LATSDAll", "Select All",size = "sm"),
                        actionBttn("LATSDClear", "Clear All",size = "sm"),
                        h5("Select years", style = "margin-top:3px"),
                        uiOutput("seriesTSD",style = "margin-top:10px;margin-bottom:5px;column-count:2;-webkit-column-count:2; -moz-column-count:2"),
                        actionBttn("seriesTSDAll", "Select All",size = "sm"),
                        actionBttn("seriesTSDClear", "Clear All",size = "sm"),
                        awesomeRadio("FmlyGrpTSD", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                        actionBttn("FmlyGrp2TSD", "Update Family Group",size = "sm")
                      )
                      ),
               column(8,
                      div(dataTableOutput("TSDTable1"), style = "font-size:16px; line-height: 30px")
 #                     dataTableOutput("TSDTable2")
                      ))
    ),
##New Tab with Rankings
    tabPanel("Rankings Over Time",
             fluidPage(
               fluidRow(
                 column(3,  
                        selectInput("categoryRank", "Select Indicator Category", unique(excl_Scotland$Domain),
                                    width = "90%")
                 ),
                 column(5,
                        uiOutput("indicatorRank")
                 ),
                 column(2,
                      selectInput("RnkLA", "Select Local Authorities", unique(excl_Scotland$`Local Authority`), selected = "Aberdeen City")),
                 column(2,awesomeCheckbox("ValRank", "Show values")),
                 width = "100%"
               ),
             
                 plotlyOutput("rankPlot", height = "80vh")
             )
      
    )
))

