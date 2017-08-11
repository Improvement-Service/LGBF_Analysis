shinyUI(navbarPage(id = "pageList",
                   
  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
  tabPanel("Something",
    wellPanel(selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain),
                          selected = "Children's Services", width = "40%"),
                  uiOutput("indicator"), width ="100%"),
    sidebarPanel(id = "sidPnl", style = "height:90vh;overflow-y:auto;",
      checkboxGroupInput("LA", "Select Local Authority", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority')),
      uiOutput("series"),
      radioButtons("FmlyGrp", "Select Family Group", c(1,2,3,4, "all"), inline = TRUE),
      actionButton("FmlyGrp2", "Update Family Group")
    ),
    mainPanel(
      plotOutput("plot1")
    )
  ), ##New tab for year on year changes
  tabPanel("Year-on-Year Change",
      wellPanel(selectInput("categoryYr", "Select Indicator Category", unique(excl_Scotland$Domain),
                        selected = "Children's Services", width = "40%"),
            uiOutput("indicatorYr"), width ="100%"),
           sidebarPanel(id = "sidPnl", style = "height:90vh;overflow-y:auto;",
                        checkboxGroupInput("LAYr", "Select Local Authority", unique(bnch_data$`Local Authority`), selected = unique(bnch_data$'Local Authority')),
                        uiOutput("baseYr"),
                        uiOutput("compYr"),
                        radioButtons("FmlyGrpYr", "Select Family Group", c(1,2,3,4, "all"), inline = TRUE),
                        actionButton("FmlyGrp2Yr", "Update Family Group"),
                        checkboxInput("RelVal", "Show Percentage Change", value = FALSE)
           ),
           mainPanel(
             plotOutput("Year-on-Year-Plot")
           )
  )
))