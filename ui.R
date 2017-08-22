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
    wellPanel(
      fluidPage(
        fluidRow(
          column(6,
      selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain),
                          selected = "Adult Social Care")
      ),
      column(6,
                  uiOutput("indicator")
        )
      )
      )
      ),
    sidebarPanel(id = "sidPnl", style = "height:55vh;overflow-y:auto;",
      checkboxGroupInput("LA", "Select Local Authority", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority')),
      uiOutput("series"),
      radioButtons("FmlyGrp", "Select Family Group", c("All",1,2,3,4), inline = TRUE),
      actionButton("FmlyGrp2", "Update Family Group")
    ),
    mainPanel(
      tags$b(uiOutput("PlotTitle")),
      plotOutput("plot1")
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
      wellPanel(
        div(class = "row",
          div(class = "span6", style = "display:inline-block; width:40vw",
                   selectInput("categoryYr", "Select Indicator Category", unique(excl_Scotland$Domain),
                        selected = "Adult Social Care")
                   ),
          div(class = "span6", style = "display:inline-block; width:40vw",
                      uiOutput("indicatorYr"))
          ), width = "100%"
      ),
     fluidRow( 
          sidebarPanel(id = "sidPnl", style = "height:55vh;overflow-y:auto;",
                       h5("Select Local Authority"),
                       div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                         checkboxGroupInput("LAYr", label = NA, unique(bnch_data$`Local Authority`), selected = unique(bnch_data$'Local Authority'))
                       ),
                         uiOutput("baseYr"),
                        uiOutput("compYr"),
                        radioButtons("FmlyGrpYr", "Select Family Group", c("All",1,2,3,4), inline = TRUE),
                        actionButton("FmlyGrp2Yr", "Update Family Group"),
                        checkboxInput("RelVal", "Show Percentage Change", value = FALSE)
        ),
           mainPanel(
             plotOutput("Year-on-Year-Plot")
           )
      )
    )
  ),
##new tab for By Council
tabPanel("By Council",
          wellPanel(
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
                   )))),
           fluidPage(id = "MnPnl", style = "height:55vh;overflow-y:auto;",
             fluidRow(
               column(4,
           tags$b(uiOutput("TableTitle"))
           ),
           column(12,
           DT::dataTableOutput("CnclTbl")
               )
         ))
         
  

)))