library(shiny)
library(tidyverse)
library(shinythemes)


bnch_data <- read_csv("bnch_dta_macrosttest.csv")
excl_Scotland <- filter(bnch_data, `Local Authority` != "Scotland")
excl_Scotland$Time<- gsub("^14-15", "2014-15", excl_Scotland$Time)
excl_Scotland$Time<- gsub("^15-16", "2015-16", excl_Scotland$Time)
excl_Scotland$Time <- gsub("/", "-", excl_Scotland$Time)