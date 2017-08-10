shinyUI(navbarPage(id = "pageList",

  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
  tabPanel("Something",
    sidebarPanel(
      selectInput("category", "Select Indicator Category", unique(bnch_data$Domain),selected = "Children's Services"),
      uiOutput("indicator"),
      checkboxGroupInput("LA", "Select Local Authority", unique(bnch_data$`Local Authority`), selected = unique(bnch_data$`Local Authority`))
    ),

    mainPanel(
      plotOutput("plot1")
    )
  )))
