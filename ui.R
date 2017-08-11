shinyUI(navbarPage(id = "pageList",
                   
  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
  tabPanel("Something",
    wellPanel(selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain),
                          selected = "Children's Services", width = "40%"),
                  uiOutput("indicator"), width ="100%", fixed = FALSE, 
                  draggable = FALSE, top = "28px"),
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
         sidebarPanel(id = "sidPnl2", style = "height:90vh;overflow-y:auto;",
                      selectInput("categoryYr", "Select Indicator Category", unique(excl_Scotland$Domain),selected = "Children's Services"),
                      #uiOutput("indicatorYr"),
                      checkboxGroupInput("LAYr", "Select Local Authority", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority')),
                     # uiOutput("seriesYr"),
                      radioButtons("FmlyGrpYr", "Select Family Group", c(1,2,3,4, "all"), inline = TRUE),
                      actionButton("FmlyGrp2Yr", "Update Family Group")
                    )
          )
  )
)
