library(shiny)
library(ggplot2)
shinyUI(navbarPage(id = "pageList",

  title = "Test LGBF Tool", 
#Can change the theme - see shinyTheme package  
  theme = shinytheme("simplex"),
  tabPanel
  sidebarLayout(
    sidebarPanel(

    ),

    mainPanel(
  
    )
  )
))
