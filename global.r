library(shiny)
library(tidyverse)
library(shinythemes)


bnch_data <- read_csv("bnch_dta_macrosttest.csv")
excl_Scotland <- filter(bnch_data, `Local Authority` != "Scotland")