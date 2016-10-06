library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)

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

# test plot: typical weekday for each counter, arranged in facets, divided by direction/type
p <- ggplot(Averagedays_long[Averagedays_long$Workday & 
                          !Averagedays_long$Likely.abnormal & 
                          !Averagedays_long$OPM.action,],
            aes(x = time, y = value, color = dir_mode)) + geom_line()+ scale_x_chron(format="%H:%M",n=8) 
p + 
  ggtitle("Average workday traffic\nby counter") + 
  facet_grid(Location ~ .)

