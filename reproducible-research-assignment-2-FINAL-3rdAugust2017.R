# Downloading Data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "C:/Users/rvai007/Documents/repdata%2Fdata%2FStormData.csv.bz2")
raw_dataset <- read.csv("C:/Users/rvai007/Documents/repdata%2Fdata%2FStormData.csv/repdata%2Fdata%2FStormData.csv", sep = ",", header = TRUE, stringsAsFactors=FALSE)
# Understanding the Structure of the Raw Dataset (37 Variables, 902297 cases)
class(raw_dataset)
dim(raw_dataset)
str(raw_dataset)
# Selecting the Required Variables (Measures on: Event Type, Loss/ damage to Human Life, Economic Repurcussions)
library(dplyr)
required_data <- raw_dataset %>% select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
# Exploratory Analysis (Initial Level - to explore the variables of interest)
mean(required_data$PROPDMG)
mean(required_data$CROPDMG)
mean(required_data$FATALITIES)
mean(required_data$INJURIES)
mean(required_data$INJURIES)
mean(required_data$PROPDMGEXP)
mean(required_data$CROPDMGEXP)
# Mean cannot be generated for the variables $PROPDMGEXP, CROPDMGEXP as these are chr. Will explore these variables further.
copy_required_data <- required_data
unique(copy_required_data$CROPDMGEXP)
## Finding the unique values of CROPDMGEXP i.e. [1] ""  "M" "K" "m" "B" "?" "0" "k" "2"
unique(copy_required_data$PROPDMGEXP)
## Finding the unique values of PROPDMGEXP i.e. [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-" "1" "8"
## The codebook specifies the values for K, M, B (as thousands, millions and Billions, but does not specifies the values for 1, 2, 4, etc.). These two variables are not suitable for analysis.
# Data Transformations - for CROPDMGEXP
## The instruction manual mentions that Alphabetical characters used to signify magnitude include "K" for thousands, "M" for millions, and "B" for billions.
## for CROPDMGEXP, I will replace these values with 1000, 100000, 1000000000 respectively. Since other numeric values are not specified, they remain as it is.
copy_required_data[copy_required_data$CROPDMGEXP == "K",]$CROPDMGEXP = 1000
copy_required_data[copy_required_data$CROPDMGEXP == "m",]$CROPDMGEXP = 100000
copy_required_data[copy_required_data$CROPDMGEXP == "M",]$CROPDMGEXP = 100000
copy_required_data[copy_required_data$CROPDMGEXP == "B",]$CROPDMGEXP = 1000000000
copy_required_data[copy_required_data$CROPDMGEXP == "k",]$CROPDMGEXP = 1000
unique(copy_required_data$CROPDMGEXP)
## [1] ""      "1e+05" "1000"  "1e+09" "?"     "0"     "2"  
## also there are blank values "". These will need to be replaced. I will replace them with the non missing average.
## Replacing the blanks
copy_required_data$CROPDMGEXP[copy_required_data$CROPDMGEXP==""] <- NA
unique(copy_required_data$CROPDMGEXP)
copy_required_data$CROPDMGEXP <- as.numeric(copy_required_data$CROPDMGEXP)
unique(copy_required_data$CROPDMGEXP)
mean(copy_required_data$CROPDMGEXP, na.rm=TRUE)
## [1] 33399.51 (This is the non missing average)
copy_required_data$CROPDMGEXP[is.na(copy_required_data$CROPDMGEXP)] <- 33399.51
unique(copy_required_data$CROPDMGEXP)
## The unique values are: [1] 3.339951e+04 1.000000e+05 1.000000e+03 1.000000e+09 0.000000e+00 2.000000e+00
# Data Transformations - for PROPDMGEXP
## The instruction manual mentions that Alphabetical characters used to signify magnitude include "K" for thousands, "M" for millions, and "B" for billions.
## "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-" "1" "8. Note that +, - will coerced as NA when this is converted to numeric.
copy_required_data[copy_required_data$PROPDMGEXP == "K",]$PROPDMGEXP = 1000
copy_required_data[copy_required_data$PROPDMGEXP == "M",]$PROPDMGEXP = 100000
copy_required_data[copy_required_data$PROPDMGEXP == "m",]$PROPDMGEXP = 100000
copy_required_data[copy_required_data$PROPDMGEXP == "B",]$PROPDMGEXP = 1000000000
copy_required_data[copy_required_data$PROPDMGEXP == "H",]$PROPDMGEXP = 100
copy_required_data[copy_required_data$PROPDMGEXP == "h",]$PROPDMGEXP = 100
unique(copy_required_data$PROPDMGEXP)
## this gives the result: [1] "1000"  "1e+05" ""      "1e+09" "+"     "0"     "5"     "6"     "?"     "4"     "2"     "3"    
## [13] "100"   "7"     "-"     "1"     "8". +, - and ? need to be replaced (assume these are missing).
copy_required_data$PROPDMGEXP[copy_required_data$PROPDMGEXP=="+"] <- NA
copy_required_data$PROPDMGEXP[copy_required_data$PROPDMGEXP=="-"] <- NA
copy_required_data$PROPDMGEXP[copy_required_data$PROPDMGEXP=="?"] <- NA
unique(copy_required_data$PROPDMGEXP)
copy_required_data$PROPDMGEXP <- as.numeric(copy_required_data$PROPDMGEXP)
mean(copy_required_data$PROPDMGEXP, na.rm=TRUE)
## [1] 95241.12 (This is the non missing average)
copy_required_data$PROPDMGEXP[is.na(copy_required_data$PROPDMGEXP)] <- 95241.12
str(copy_required_data)
## All variables are numeric

# Data Analysis:
## QUESTION 1: Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
### Fatalities
fatalities_splittedby_event <- tapply(copy_required_data$FATALITIES, copy_required_data$EVTYPE, sum)
fatalities_splittedby_event <- as.data.frame.table(fatalities_splittedby_event)
class(fatalities_splittedby_event)
dim(fatalities_splittedby_event)
head(fatalities_splittedby_event)
fatalities_splittedby_event <- setNames(fatalities_splittedby_event, c("Event_Type","Frequency"))
library(dplyr)
graph_fatalities_splittedby_event <- fatalities_splittedby_event %>% filter(Frequency != 0)
arrange(graph_fatalities_splittedby_event, desc(graph_fatalities_splittedby_event$Frequency))
graph_fatalities_splittedby_event_TOP20 <- graph_fatalities_splittedby_event %>% filter(Frequency >= 100)
## Clearly the Top 5 (i.e. worst 5) events are TORNADO, EXCESSIVE HEAT, FLASH FLOOD, HEAT and LIGHTENING.
## This is shown graphically below:
barplot (height = graph_fatalities_splittedby_event_TOP20$Frequency, names.arg = graph_fatalities_splittedby_event_TOP20$Event_Type, las = 2, cex.names = 0.7, col = rainbow (30, start=0, end=1))
### Injuries (i.e. are patterns consistent across both injuries and fatalities)
str(copy_required_data)
injuries_splittedby_event <- tapply(copy_required_data$INJURIES, copy_required_data$EVTYPE, sum)
injuries_splittedby_event <- as.data.frame.table(injuries_splittedby_event)
injuries_splittedby_event <- setNames(injuries_splittedby_event, c("Event_Type","Frequency"))
library(dplyr)
graph_injuries_splittedby_event <- injuries_splittedby_event %>% filter(Frequency != 0)
arrange(graph_injuries_splittedby_event, desc(graph_injuries_splittedby_event$Frequency))
## Clearly, the top 5 (i.e. worst 5) events are TORNADO, TSTM WIND, FLOOD, EXCESSIVE HEAT, LIGHTENING
library(dplyr)
graph_injuries_splittedby_event_TOP20 <- graph_injuries_splittedby_event %>% filter(Frequency >= 440)
barplot (height = graph_injuries_splittedby_event_TOP20$Frequency, names.arg = graph_injuries_splittedby_event_TOP20$Event_Type, las = 2, cex.names = 0.7, col = rainbow (30, start=0, end=1))
# Lets find out the events that are present in both FATALITIES and INJURIES  
common <- c("TORNADO", "EXCESSIVE HEAT", "FLASH FLOOD", "HEAT", "LIGHTENING", "TORNADO", "TSTM WIND", "FLOOD", "EXCESSIVE HEAT", "LIGHTENING")
unique(common)
# ANSWER: Q1 [1] "TORNADO" "EXCESSIVE HEAT" "FLASH FLOOD" "HEAT" "LIGHTENING" "TSTM WIND" "FLOOD"
## QUESTION 2: Across the United States, which types of events have the greatest economic consequences?
### Property Damage
propdmge_splittedby_event <- tapply(copy_required_data$PROPDMG, copy_required_data$EVTYPE, sum)
propdmge_splittedby_event <- as.data.frame.table(propdmge_splittedby_event)
propdmge_splittedby_event <- setNames(propdmge_splittedby_event, c("Event_Type","Frequency"))
library(dplyr)
graph_propdmge_splittedby_event <- propdmge_splittedby_event %>% filter(Frequency != 0)
arrange(graph_propdmge_splittedby_event, desc(graph_propdmge_splittedby_event$Frequency))
## Clearly, the top 5 (i.e. worst 5) events are TORNADO, FLASH FLOOD, TSTM WIND, FLOOD, THUNDERSTORM WIND
library(dplyr)
graph_propdmge_splittedby_event_TOP20 <- graph_propdmge_splittedby_event %>% filter(Frequency >= 26000)
barplot (height = graph_propdmge_splittedby_event_TOP20$Frequency, names.arg = graph_propdmge_splittedby_event_TOP20$Event_Type, las = 2, cex.names = 0.7, col = rainbow (30, start=0, end=1))
### Crop Damage
cropdmge_splittedby_event <- tapply(copy_required_data$CROPDMG, copy_required_data$EVTYPE, sum)
cropdmge_splittedby_event <- as.data.frame.table(cropdmge_splittedby_event)
cropdmge_splittedby_event <- setNames(cropdmge_splittedby_event, c("Event_Type","Frequency"))
library(dplyr)
graph_cropdmge_splittedby_event <- cropdmge_splittedby_event %>% filter(Frequency != 0)
arrange(graph_cropdmge_splittedby_event, desc(graph_cropdmge_splittedby_event$Frequency))
## Clearly, the top 5 (i.e. worst crop damagers) are HAIL, FLASH FLOOD, FLOOD, TSTM WIND, TORNADO
library(dplyr)
graph_cropdmge_splittedby_event_TOP20 <- graph_cropdmge_splittedby_event %>% filter(Frequency >= 3500)
barplot (height = graph_cropdmge_splittedby_event_TOP20$Frequency, names.arg = graph_cropdmge_splittedby_event_TOP20$Event_Type, las = 2, cex.names = 0.7, col = rainbow (30, start=0, end=1))
common_econmomic <- c("TORNADO", "FLASH FLOOD", "TSTM WIND", "FLOOD", "THUNDERSTORM WIND", "HAIL", "FLASH FLOOD", "FLOOD", "TSTM WIND", "TORNADO")
unique(common_econmomic)
# ANSWER Q2: [1] "TORNADO" "FLASH FLOOD" "TSTM WIND" "FLOOD" "THUNDERSTORM WIND" "HAIL". 
all_in_all <- c(common, common_econmomic)
unique(all_in_all)
## All in all, the worst events are: [1] "TORNADO" "EXCESSIVE HEAT" "FLASH FLOOD" "HEAT" "LIGHTENING" "TSTM WIND" "FLOOD" "THUNDERSTORM WIND" "HAIL"