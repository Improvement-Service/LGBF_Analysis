shinyUI(navbarPage(id = "pageList",
                   
  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
header = ##Some css
  tags$head(tags$style(
    "#plot1 {height:75vh !important}",
    "#Year-on-Year-Plot {height:75vh !important}",
    "#CnclTbl {height:75vh !important}"
  )),
  tabPanel("By Indicator",
      fluidPage(
        wellPanel(
        fluidRow(
          column(6,
      selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain),
                          selected = "Adult Social Care")
      ),
      column(6,
                  uiOutput("indicator")
        )
      )
      ),
    sidebarPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto;",
      h5("Select Local Authority"),
      div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
        checkboxGroupInput("LA", label = NA, unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$`Local Authority`))),
      actionButton("LAAll", "Select All"),
      actionButton("LAClear", "Clear All"),
      uiOutput("series"),
      actionButton("SeriesAll", "Select All"),
      actionButton("SeriesClear", "Clear All"),
      radioButtons("FmlyGrp", "Select Family Group", c("All",1,2,3,4), inline = TRUE),
      actionButton("FmlyGrp2", "Update Family Group"), width = 4
    ),
    mainPanel(
      tags$b(uiOutput("PlotTitle")),
      plotlyOutput("plot1"), width = 8
    ))),

##New tab for year on year changes
  tabPanel("Year-on-Year Change",
#some css
  tags$head(tags$style(
#    ".chckBx {column-count:2;-webkit-column-count:2; -moz-column-count:2}"
      HTML("div.checkbox {margin-top: 0px;}")
     )),
    fluidPage(
      wellPanel(
        fluidRow(
        column(6,
          selectInput("categoryYr", "Select Indicator Category", unique(excl_Scotland$Domain),
                        selected = "Adult Social Care")
                   ),
        column(6,
          uiOutput("indicatorYr")
          )
          ), width = "100%"
      ),
     fluidRow( 
          sidebarPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto;",
                       h5("Select Local Authority"),
                       div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                         checkboxGroupInput("LAYr", label = NA, unique(bnch_data$`Local Authority`), selected = bnch_data$`Local Authority`)),
                        actionButton("LAYrAll", "Select All"),
                        actionButton("LAYrClear", "Clear All"),
                        uiOutput("baseYr"),
                        uiOutput("compYr"),
                        radioButtons("FmlyGrpYr", "Select Family Group", c("All",1,2,3,4), inline = TRUE),
                        actionButton("FmlyGrp2Yr", "Update Family Group"),
                        checkboxInput("RelVal", "Show Percentage Change", value = FALSE)
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
           column(1,
                  actionButton("SeriesCNCLALL", "Select All")
           ),
           column(1,
                  actionButton("SeriesCNCLClear", "Clear All")
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
             column(3,
                    wellPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto; margin-right:1px; padding-right:1px; margin-left:5px",
                          h5("Select Local Authority"),
                          div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                              checkboxGroupInput("LADisp", label = NA, unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                          ),
                          actionButton("LADispAll", "Select All"),
                          actionButton("LADispClear", "Clear All"),
                          uiOutput("seriesDisp"),
                          actionButton("seriesDispAll", "Select All"),
                          actionButton("seriesDispClear", "Clear All"),
                          radioButtons("FmlyGrpDisp", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                          actionButton("FmlyGrp2Disp", "Update Family Group")
             )
             ),
             column(9,
                    mainPanel(id = "mainDisp", style = "padding-left:1px; margin-left:1px",
                 splitLayout(
                   cellWidths = c("70%", "80%"),
                 div(DT::dataTableOutput("tableDisp"),style = "font-size:74%; line-height:40%"),
                 plotlyOutput("boxDisp")
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
      fluidRow(column(3,
                      wellPanel(style = "height:75vh;overflow-y:auto;",
                        h5("Select Local Authority"),
                        div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                         checkboxGroupInput("LATSD", label = NA, unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                        ),
                        actionButton("LATSDAll", "Select All"),
                        actionButton("LATSDClear", "Clear All"),
                        uiOutput("seriesTSD"),
                        actionButton("seriesTSDAll", "Select All"),
                        actionButton("seriesTSDClear", "Clear All"),
                        radioButtons("FmlyGrpTSD", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                        actionButton("FmlyGrp2TSD", "Update Family Group")
                      )
                      ),
               column(9,
                      dataTableOutput("TSDTable1"),
                      dataTableOutput("TSDTable2")))
    )
))

