shinyUI(navbarPage(id = "pageList",

  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
  tabPanel("Something",
    sidebarPanel(
      selectInput("inputName", "Title of Input", c("this", "is", "where","you", "choose", "inputs"))
    ),

    mainPanel(
      plotOutput("plot1")
    )
  )))
