library(shiny)
library(tidyverse)
library(plyr)
library(shinythemes)
library(RColorBrewer)
library(DT)
library(data.table)
library(plotly)
library(shinyWidgets)
library(dygraphs)
library(kableExtra)
library(formattable)
library(sparkline)

Sys.setenv("plotly_username" = "NMCassidy")
Sys.setenv("plotly_api_key" = "xwk9zuxumf")

bnch_data <- readRDS("data/data-March19.rds")
#get rid of NA values
bnch_data <- bnch_data[complete.cases(bnch_data$Value),]
#Tidy names
bnch_data$Year<- gsub("^14-15", "2014-15", bnch_data$Year)
bnch_data$Year<- gsub("^15-16", "2015-16", bnch_data$Year)
bnch_data$Year<- gsub("^16-17", "2016-17", bnch_data$Year)
bnch_data$Year <- gsub("/", "-", bnch_data$Year)
bnch_data <- filter(bnch_data, !Year %in% c("1974", "2012", "2013"))
#remove data that is used only in calculation of the indicators
bnch_data <- filter(bnch_data, !is.na(`One is high`))
#arrange options in alphabetical order
bnch_data<- arrange(bnch_data, Domain, Title, Year)
##Tidy up the names for 
#The Gross Cost of \"Children Looked After\" in a Community Setting per Child per Week adjusted for inflation
bnch_data$Title <- gsub('\"', "", bnch_data$Title)
bnch_data$Value <- round(bnch_data$Value,1)
excl_Scotland <- filter(bnch_data, `Local Authority` != "Scotland")
