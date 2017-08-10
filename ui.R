shinyUI(navbarPage(id = "pageList",

  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
  tabPanel("Something",
    sidebarPanel(
      selectInput("category", "Select Indicator Category", unique(excl_Scotland$Domain),selected = "Children's Services"),
      uiOutput("indicator"),
      checkboxGroupInput("LA", "Select Local Authority", unique(excl_Scotland$`Local Authority`), selected = unique(excl_Scotland$'Local Authority')),
      uiOutput("series"),
      radioButtons("FmlyGrp", "Select Family Group", unique(excl_Scotland$`Family group (People)`), inline = TRUE),
      actionButton("FmlyGrp2", "Update Family Group")
    ),

    mainPanel(
      plotOutput("plot1")
    )
  )))
