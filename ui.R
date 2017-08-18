shinyUI(navbarPage(id = "pageList",
                   
  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
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
               div(class = "row",
                   div(class = "span6", style = "display:inline-block; width:40vw",
                       selectInput("categoryDisp", "Select Indicator Category", unique(excl_Scotland$Domain),
                                   selected = "Children's Services")
                   ),
                   div(class = "span6", style = "display:inline-block; width:40vw",
                       uiOutput("indicatorDisp"))
               ), width = "100%"
             ),   
             sidebarPanel(id = "sidPnl", style = "height:75vh;overflow-y:auto;",
                          h5("Select Local Authority"),
                          div(style = "column-count:2;-webkit-column-count:2; -moz-column-count:2",
                              checkboxGroupInput("LADisp", label = NA, unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority'))
                          ),
                          uiOutput("seriesDisp"),
                          radioButtons("FmlyGrpDisp", "Select Family Group", c(1,2,3,4, "All"), inline = TRUE),
                          actionButton("FmlyGrp2Disp", "Update Family Group")
             ),
             mainPanel(
               fluidRow(
                 splitLayout(
                   cellWidths = c("40%", "60%"),
                 div(DT::dataTableOutput("tableDisp"),style = "font-size:74%; line-height:40%"),
                 plotOutput("boxDisp")
                 )
               )
             )
        )
))