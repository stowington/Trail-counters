library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)
library(grid)
library(stringi) # for week of month

# Working in GMT to avoid having the labels shifted by 5 hours!
Sys.setenv(TZ='GMT')

setwd("~/Dropbox/VT coursework/Capstone/Analysis") # Dir for prepped data on John's computer
load("Averagedays.Rda")
load("RushCounts.Rda")
load("Averagedays_long.Rda")



#Averagedays$times <- as.POSIXct(paste("2010-1-1 ",Averagedays$time.text,sep=""),format="%Y-%m-%d %H:%M",tz="EST")

# test plot: typical inbound bike traffic for counter 1 on normal workdays with no OPM action or other severe abnormalities
p <- ggplot(Averagedays[Averagedays$Workday & 
                          !Averagedays$Likely.abnormal & 
                          !Averagedays$OPM.action & 
                          Averagedays$counter_num == "1" &
                          Averagedays$direction == "I" &
                          Averagedays$mode == "P",],
                          aes(x = time, y = count)) + geom_line()
p + 
  scale_x_chron(format="%H:%M") + 
  ggtitle("Average workday traffic\ncounter 1, inbound pedestrians")

# test plot: typical weekday for each counter, arranged in facets
p <- ggplot(Averagedays[Averagedays$Workday & 
                          !Averagedays$Likely.abnormal & 
                          !Averagedays$OPM.action,],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average workday traffic\nby counter, both directions") + 
  facet_grid(counter_num ~ .)


# test plot: typical weekday traffic, arranged in facets by counter and year
p <- ggplot(Averagedays_year[Averagedays_year$Workday & 
                          !Averagedays_year$Likely.abnormal & 
                          !Averagedays_year$OPM.action,],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average workday traffic\nby counter, both directions") + 
  facet_grid(counter_num ~ Year)



# test plot: typical weekday traffic, arranged in facets by month and year, for Rosslyn Bikeometer (#28)
p <- ggplot(Averagedays_monthyear[Averagedays_monthyear$Workday & 
                               !Averagedays_monthyear$Likely.abnormal & 
                               !Averagedays_monthyear$OPM.action &
                               Averagedays_monthyear$counter_num == "28",],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average workday traffic\nby month and year, Rosslyn Bikeometer") + 
  facet_grid(Month ~ Year) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm"))


# test plot: typical weekend traffic, arranged in facets by month and year, for Rosslyn Bikeometer (#28)
p <- ggplot(Averagedays_monthyear[Averagedays_monthyear$Weekend & 
                                    !Averagedays_monthyear$Likely.abnormal & 
                                    !Averagedays_monthyear$OPM.action &
                                    Averagedays_monthyear$counter_num == "28",],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average weekend traffic\nby month and year, Rosslyn Bikeometer") + 
  facet_grid(Month ~ Year) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm"))


# test plot: typical weekday traffic, arranged in facets by month and year, for Bluemont Connector (#23)
p <- ggplot(Averagedays_monthyear[Averagedays_monthyear$Workday & 
                                    !Averagedays_monthyear$Likely.abnormal & 
                                    !Averagedays_monthyear$OPM.action &
                                    Averagedays_monthyear$counter_num == "23" &
                                    Averagedays_monthyear$Year > 2012,],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average workday traffic\nby month and year, Bluemont Connector") + 
  facet_grid(Month ~ Year) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm"))
ggsave("Counter 23 month year.pdf",width = 8.5,height = 11, units = "in")

# test plot: typical weekend traffic, arranged in facets by month and year, for Bluemont Connector (#23)
p <- ggplot(Averagedays_monthyear[Averagedays_monthyear$Weekend & 
                                    !Averagedays_monthyear$Likely.abnormal & 
                                    !Averagedays_monthyear$OPM.action &
                                    Averagedays_monthyear$counter_num == "23" &
                                    Averagedays_monthyear$Year > 2012,],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average weekend traffic\nby month and year, Bluemont Connector") + 
  facet_grid(Month ~ Year) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm"))
ggsave("Counter 23 month year weekend.pdf",width = 8.5,height = 11, units = "in")




# test plot: typical weekday traffic, arranged in facets by month and year, for Ballston Connector (#24)
p <- ggplot(Averagedays_monthyear[Averagedays_monthyear$Workday & 
                                    !Averagedays_monthyear$Likely.abnormal & 
                                    !Averagedays_monthyear$OPM.action &
                                    Averagedays_monthyear$counter_num == "24",],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average workday traffic\nby month and year, Ballston Connector") + 
  facet_grid(Month ~ Year) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm")) + coord_cartesian(ylim=c(0,25))
ggsave("Counter 24 month year.pdf",width = 8.5,height = 11, units = "in")

# test plot: typical weekend traffic, arranged in facets by month and year, for Ballston Connector (#24)
p <- ggplot(Averagedays_monthyear[Averagedays_monthyear$Weekend & 
                                    !Averagedays_monthyear$Likely.abnormal & 
                                    !Averagedays_monthyear$OPM.action &
                                    Averagedays_monthyear$counter_num == "24" ,],
            aes(x = time, y = count, color = dir_mode)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average weekend traffic\nby month and year, Ballston Connector") + 
  facet_grid(Month ~ Year) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm")) + coord_cartesian(ylim=c(0,25))
ggsave("Counter 24 month year weekend.pdf",width = 8.5,height = 11, units = "in")


###########
# Month/year grids for all counters, saved as individual files
###########

monthlyweekdays <- function(counternum) {
  p <- ggplot(Averagedays_monthyear[Averagedays_monthyear$Workday & 
                                      !Averagedays_monthyear$Likely.abnormal & 
                                      !Averagedays_monthyear$OPM.action &
                                      Averagedays_monthyear$counter_num == counternum,],
              aes(x = time, y = count, color = dir_mode)) + 
    geom_line() + 
    scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
  p + 
    ggtitle(paste0("Average workday traffic by month and year,\nCounter ",counternum,": ", counters[counters$counter_num == counternum,]$name)) + 
    facet_grid(Month ~ Year) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm")) + coord_cartesian(ylim=c(0,25))
  ggsave(paste0("Month year plot Counter ", counternum, ".pdf"),width = 8.5,height = 11, units = "in")
  
}

lapply(unique(Averagedays_monthyear$counter_num),monthlyweekdays)

# investigating potentially screwy data by examining a few graphs more closely:


yearmonthcountercheckaverage <- function(yearnum,monthname,counternum) {
  p <- ggplot(Averagedays_monthyear[(Averagedays_monthyear$Workday | Averagedays_monthyear$Weekend) &
                                      !Averagedays_monthyear$Likely.abnormal & 
                                      !Averagedays_monthyear$OPM.action &
                                      Averagedays_monthyear$Year == yearnum &
                                      Averagedays_monthyear$Month == monthname &
                                      Averagedays_monthyear$counter_num == counternum,],
              aes(x = time, y = count, color = dir_mode)) + 
    geom_line() + 
    scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
  p + 
    ggtitle(paste0("Average workday traffic for\nCounter ",counternum,": ", counters[counters$counter_num == counternum,]$name, " ", monthname, " ", yearnum)) + 
     facet_grid(Workday ~ .) + scale_colour_brewer(palette = "Set1") + theme(plot.margin = unit(rep(3,4),"mm")) 
 # ggsave(paste0("Data check ", yearnum, " ", monthname, " Counter ", counternum, " Weekday.pdf"),width = 8.5,height = 11, units = "in")
  
}
yearmonthcountercheckaverage(2015,"April",45)

#labels for the calendar based grids that follow
dates_in_data <- unique(subset(combineddata_Cleaned,select=c("date","Day","Week","Month","Quarter","Year","mweek")))
dates_in_data$daynum <- as.character(mday(dates_in_data$date))
dates_in_data$mweek <- as.integer(stri_datetime_format(dates_in_data$date, format = "W"))

# Turns out I didn't need all this.
# #dummy entries to fill in the rest of the calendar grid.
# dates_in_data2 <- with(dates_in_data, expand.grid(Day = levels(Day), Month = levels(Month), mweek = levels(as.factor(mweek)), Year = levels(as.factor(Year)))) 
# #dates_in_data2 <- ddply(dates_in_data, .(Day,Month,Year,mweek), numcolwise(function(x) {if(length(x)>0) x else NA}), .drop=F) # alternative, requires dplyr
# dates_in_data3 <- merge(dates_in_data,dates_in_data2,all = TRUE)
# dates_in_data3$filler <- is.na(dates_in_data3$daynum) #mark rows with missing date information as filler
# dates_in_data3$daynum[is.na(dates_in_data3$daynum)] <- "" #then convert them to empty strings so they don't get deleted when labeling
# deletions <- aggregate(filler ~ Year + Month + mweek, data = dates_in_data3, sum) #count fillers in each year, month, week combination
# deletions <- rename(deletions,c("filler"="extras"))
# dates_in_data3 <- merge(dates_in_data3,deletions,all = TRUE) #attach the counts back to the main list
# dates_in_data3 <- dates_in_data3[dates_in_data3$extras < 7,] # if a week has seven NA values, we don't need it to label plots (avoiding some warnings)
# #dates_in_data3 <- dates_in_data3[order(dates_in_data3$Year,dates_in_data3$Month,dates_in_data3$mweek,dates_in_data3$Day),] #sort the labels before graphing to ensure the labels will be in order

yearmonthcountercheckraw <- function(yearnum,monthname,counternum) {
  plotdata <- combineddata_Cleaned[combineddata_Cleaned$Year == yearnum &
                                     combineddata_Cleaned$Month == monthname &
                                     combineddata_Cleaned$counter_num == counternum,]
  labeldata <- subset(dates_in_data, Year == yearnum & Month == monthname)
  max_y <- max(plotdata$count)
  p <- ggplot(plotdata,
              aes(x = time, y = count, group = dir_mode)) + 
    geom_line(aes(color = dir_mode)) + 
    scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
  p + 
    ggtitle(paste0("Average workday traffic for\nCounter ",counternum,": ", counters[counters$counter_num == counternum,]$name, " ", monthname, " ", yearnum)) + 
    facet_grid(mweek ~ Day) + 
    scale_colour_brewer(palette = "Set1") + 
    theme(plot.margin = unit(rep(3,4),"mm"))  +  
    geom_text(aes(x=.25,y=max_y * 0.75, label=daynum, group=NULL), data=labeldata) #label each facet with the day
  
  # ggsave(paste0("Data check ", yearnum, " ", monthname, " Counter ", counternum, " Daily.pdf"),width = 11,height = 8.5, units = "in")
  
}

#yearmonthcountercheckraw(2015,"April",45) #test

further_investigation_table <- read.csv("further investigation list.csv")

previouspath <- getwd()
setwd("~/Dropbox/VT coursework/Capstone/Analysis/Plots/Datacheck")
apply(further_investigation_table,1,function(params)yearmonthcountercheckraw(params[1],params[2],params[3]))

setwd(previouspath)

####BROKEN BELOW HERE

# test plot: typical weekday for each counter, arranged in facets, divided by direction/type
p <- ggplot(Averagedays_long[Averagedays_long$Workday & 
                          !Averagedays_long$Likely.abnormal & 
                          !Averagedays_long$OPM.action,],
            aes(x = time, y = value, color = dir_mode)) + geom_line()+ scale_x_chron(format="%H:%M",n=8) 
p + 
  ggtitle("Average workday traffic\nby counter") + 
  facet_grid(Location ~ .)

