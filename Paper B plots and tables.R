library(chron)
library(lubridate)
library(plyr)
library(dplyr) #for piping, summary stats
library(zoo) 
library(reshape2)
library(ggplot2)
library(lattice)
library(grid)
library(stringi) # for week of month

# Working in GMT to avoid having the labels shifted by 5 hours!
Sys.setenv(TZ='GMT')

setwd("~/Dropbox/VT coursework/Capstone/Analysis") # Dir for prepped data on John's computer
load("Arl_Webdata_Cleaned.Rda")
load("Averagedays.Rda")
load("RushCounts.Rda")
load("Averagedays_long.Rda")


##### summary statistics for paper
##### 

summary <- combineddata_Cleaned %>%
  group_by(counter_num,Weekend,Workday) %>%
  summarise(n_obs = n(), days = n_distinct(date) )
summary$daytype <- "other"
summary$daytype[summary$Workday] <- "Workday"
summary$daytype[summary$Weekend] <- "Weekend"
summary$daytype <- factor(summary$daytype,levels=c("Workday","Weekend","other"),ordered = TRUE) # Helps order columns later


##### table 1: overall counter info
#####
summary_n <- dcast(summary,counter_num ~ daytype,sum,value.var = "n_obs", margins = TRUE)
summary_days <- dcast(summary,counter_num ~ daytype,sum,value.var = "days", margins = TRUE)
names(summary_n)[2:5] <- paste0("n_",names(summary_n)[2:5]) # rename columns before merging
names(summary_days)[2:5] <- paste0("days_",names(summary_days)[2:5])

table_1 <- merge(summary_n,summary_days)
table_1 <- merge(counters,table_1)
# table_1 <- merge(table_1,firstDays)
# #table_1 <- table_1[,c("counter_num","name","trail_name","region","date","n_Workday","n_Weekend","n_(all)","days_other","days_Weekend","days_Workday","days_(all)")] # strip down to the variables we need to present
# names(table_1)[names(table_1) == "date"] <- "earliest_date"

#####  2015 info

summary2015 <- combineddata_Cleaned %>%
  filter(Year == 2015 ) %>%
  group_by(counter_num,direction,mode,dir_mode,Weekend,Workday) %>%
  summarise(days = n_distinct(date), total = sum(count),average = sum(count)/n_distinct(date) )
summary2015$daytype <- "other"
summary2015$daytype[summary2015$Workday] <- "Workday"
summary2015$daytype[summary2015$Weekend] <- "Weekend"
summary2015$daytype <- factor(summary2015$daytype,levels=c("Workday","Weekend","other"),ordered = TRUE) # Helps order columns later

summary2015_days <- dcast(summary2015,counter_num ~ daytype,sum,value.var = "days",margins = TRUE, subset = .(dir_mode == "inbound bicycle"))
summary2015_total <- dcast(summary2015,counter_num ~ daytype + mode,sum,value.var = "total",margins = TRUE)
summary2015_mean <- dcast(summary2015,counter_num ~ daytype + mode,sum,value.var = "average",margins = TRUE)

names(summary2015_days)[2:5] <- paste0("days_2015_", names(summary2015_days)[2:5])
names(summary2015_total)[2:11] <- paste0("total_2015_", names(summary2015_total)[2:11])
names(summary2015_mean)[2:11] <- paste0("mean_2015_", names(summary2015_mean)[2:11])

summary2015_combined <- merge(summary2015_days,summary2015_total)
summary2015_combined <- merge(summary2015_combined,summary2015_mean)

table_1 <- merge(table_1,summary2015_combined, by = "counter_num",all = TRUE)

write.csv(table_1, file = "Table 1.csv", row.names = FALSE)