shinyUI(navbarPage(id = "pageList",
                   
  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
header = ##Some css
  tags$head(tags$style(
 "#plot1 {height:75vh !important}",
 "#Year-on-Year-Plot {height:75vh !important}",
 "#boxDisp {height:75vh !important}"
)),
  tabPanel("Something",
    wellPanel(
      div(class = "row",
          div(class = "span6", style = "display:inline-block; width:40vw",
      selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain),
                          selected = "Children's Services")
          ),
          div(class = "span6", style = "display:inline-block; width:40vw",
            uiOutput("indicator")
          )
      ), width ="100%"
    ),
    sidebarPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto;",
      checkboxGroupInput("LA", "Select Local Authority", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority')),
      uiOutput("series"),
      radioButtons("FmlyGrp", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
      actionButton("FmlyGrp2", "Update Family Group")
    ),
    mainPanel(
      uiOutput("PlotTitle"),
      plotOutput("plot1")
    )
  ), ##New tab for year on year changes
  tabPanel("Year-on-Year Change",
#some css
  tags$head(tags$style(
#    ".chckBx {column-count:2;-webkit-column-count:2; -moz-column-count:2}"
      HTML("div.checkbox {margin-top: 0px;}")
     )),
    fluidPage(
      wellPanel(
        div(class = "row",
          div(class = "span6", style = "display:inline-block; width:40vw",
                   selectInput("categoryYr", "Select Indicator Category", unique(excl_Scotland$Domain),
                        selected = "Children's Services")
                   ),
          div(class = "span6", style = "display:inline-block; width:40vw",
                      uiOutput("indicatorYr"))
          ), width = "100%"
      ),
     fluidRow( 
          sidebarPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto;",
                       h5("Select Local Authority"),
                       div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                         checkboxGroupInput("LAYr", label = NA, unique(bnch_data$`Local Authority`), selected = unique(bnch_data$'Local Authority'))
                       ),
                         uiOutput("baseYr"),
                        uiOutput("compYr"),
                        radioButtons("FmlyGrpYr", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                        actionButton("FmlyGrp2Yr", "Update Family Group"),
                        checkboxInput("RelVal", "Show Percentage Change", value = FALSE)
        ),
           mainPanel(
             plotOutput("Year-on-Year-Plot")
           )
      )
    )
  ),
#New Tab for Dispersion ====================================
    tabPanel("Dispersion",
             wellPanel(
               div(class = "row", style = "padding-left:5px",
                   div(class = "span6", style = "display:inline-block; width:40vw; padding-right:10px",
                       selectInput("categoryDisp", "Select Indicator Category", unique(excl_Scotland$Domain),
                                   selected = "Children's Services")
                   ),
                   div(class = "span6", style = "display:inline-block; width:40vw",
                       uiOutput("indicatorDisp"))
               ), width = "100%"
             ),   
        fluidRow(
             column(3,
                    wellPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto; margin-right:1px; padding-right:1px; margin-left:5px",
                          h5("Select Local Authority"),
                          div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                              checkboxGroupInput("LADisp", label = NA, unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                          ),
                          uiOutput("seriesDisp"),
                          radioButtons("FmlyGrpDisp", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                          actionButton("FmlyGrp2Disp", "Update Family Group")
             )
             ),
             column(9,
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
               div(class = "row", style = "padding-left:5px",
                   div(class = "span6", style = "display:inline-block; width:40vw",
                       selectInput("categoryTSD", "Select Indicator Category", unique(excl_Scotland$Domain),
                                   selected = "Children's Services")
                   ),
                   div(class = "span6", style = "display:inline-block; width:40vw",
                       uiOutput("indicatorTSD"))
               ), width = "100%"
             ),             
      fluidRow(column(3,
                      wellPanel(style = "height:75vh;overflow-y:auto;",
                        h5("Select Local Authority"),
                        div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                         checkboxGroupInput("LATSD", label = NA, unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                        ),
                        uiOutput("seriesTSD"),
                        radioButtons("FmlyGrpTSD", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                        actionButton("FmlyGrp2TSD", "Update Family Group")
                      )
                      ),
               column(9,
                      dataTableOutput("TSDTable1"),
                      dataTableOutput("TSDTable2")))
    )
))