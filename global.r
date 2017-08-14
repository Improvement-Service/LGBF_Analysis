library(shiny)
library(tidyverse)
library(plyr)
library(shinythemes)


bnch_data <- read_csv("bnch_dta_macrosttest.csv")
#get rid of NA values
bnch_data <- bnch_data[complete.cases(bnch_data$Value),]
#Tidy names
bnch_data$Time<- gsub("^14-15", "2014-15", bnch_data$Time)
bnch_data$Time<- gsub("^15-16", "2015-16", bnch_data$Time)
bnch_data$Time <- gsub("/", "-", bnch_data$Time)
bnch_data <- filter(bnch_data, !Time %in% c("1974", "2012", "2013"))
##Tidy up the names for 
#The Gross Cost of \"Children Looked After\" in a Community Setting per Child per Week adjusted for inflation
bnch_data$Title <- gsub('\"', "", bnch_data$Title)
excl_Scotland <- filter(bnch_data, `Local Authority` != "Scotland")