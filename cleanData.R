###====Code for preparing data to be loaded into tool=====##

library(tidyverse)
library(readxl)

##read in most up to date master data sheet and metadata
setwd("C:/Users/cassidy.nicholas/OneDrive - IS/LGBF-Spotfire/data")
data <- read_excel("new bnch data.xlsx", sheet = 2)[1:1857] ##remove last few columns with Scotland values from SHS
data <- data[complete.cases(data$`Local Authority`),]
##remove columns with all NAs
all_na <- function(x) any(!is.na(x))
data <- data %>% select_if(all_na)
##Get metadata - this needs to be manually updated (in excel) with any new indicators
metadata <- read_excel("Metadata - Shiny.xlsx")
metadata$Title <- tools::toTitleCase(metadata$Title)
simdData <- read_excel("SIMDvalues.xlsx")
colnames(simdData)[2:3] <- c("Family group (People)", "Family group (Other)")

##For some reason ENV4b is changing A to a - fix this
metadata[metadata$`Variable name` == "ENV4b",2] <- "Percentage of A Class Roads that Should be Considered for Maintenance Treatment"

#convert data to long format
data <- gather(data = data, key = "Inticators Time", value = "value",-1)
data$`Inticators Time` <- gsub(" 20", "_20",data$`Inticators Time`)

##Move the word "real" in the real indicators
data$reals <- grepl("Real", data$`Inticators Time`, ignore.case = FALSE)
data$`Inticators Time` <- gsub(" Real", "", data$`Inticators Time`)
data <- separate(data, col = "Inticators Time", into = c("Indicator2", "Year"), sep = "_",remove = FALSE)
data[data$reals==TRUE, "Indicator2"] <- paste0(data[data$reals==TRUE, "Indicator2", drop = TRUE], "-Real")
data <- data[1:5]

##Remove teacher judgement indicators and SW4a
judg <- c( "CHN13c",  "CHN14c",
          "CHN15a", "CHN15b", "CHN15c", "CHN16a", "CHN16b", "CHN16c", "SW4a")
judgrws <- grepl(paste(judg, collapse = "|"),data$Indicator2)
data <- data[!judgrws,]
##Also remove historic data for CHN13a,b,14a,b
oldjudge <- c("CHN14a_2015-16","CHN14a_2016-17", "CHN14a_2017-18","CHN13a_2015-16","CHN13a_2016-17", "CHN13a_2017-18",
              "CHN14b_2015-16","CHN14b_2016-17", "CHN14b_2017-18","CHN13b_2015-16","CHN13b_2016-17", "CHN13b_2017-18")
oldjudgrws <- grepl(paste(oldjudge,collapse = "|"),data$`Inticators Time`)
data <- data[!oldjudgrws,]


##Allow merging by renaming nums/denoms in metadata
metadata[metadata$`Key indicator` == "No" & complete.cases(metadata$`Key indicator`),1] <-  metadata[metadata$`Key indicator` == "No" & complete.cases(metadata$`Key indicator`),2]

allData <- left_join(data, metadata, by = c("Indicator2" = "Variable name"))
allData <- left_join(allData, simdData, by = c("Local Authority"))

##rearrange columns to same as previous dataset
allData <- allData[c(1:2,5,4,3,6:27)]
colnames(allData)[3] <- "Value"
allData$Value <- as.numeric(allData$Value)
saveRDS(allData, "data-May21.rds")

write_csv(allData, "data-May21.csv")
