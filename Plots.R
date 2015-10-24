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

# test plot: typical weekday for the Rosslyn counter on normal workdays with no OPM action or other severe abnormalities
p <- ggplot(Averagedays[Averagedays$Workday & 
                          !Averagedays$Likely.abnormal & 
                          !Averagedays$OPM.action & 
                          Averagedays$Location == "Custis_Rosslyn",],
                          aes(x = time, y = Total)) + geom_line()
p + 
  scale_x_chron(format="%Y-%m-%d %H:%M, %Z") + 
  ggtitle("Average workday traffic\nCustis_Rosslyn, both directions")

# test plot: typical weekday for each counter, arranged in facets
p <- ggplot(Averagedays[Averagedays$Workday & 
                          !Averagedays$Likely.abnormal & 
                          !Averagedays$OPM.action,],
            aes(x = time, y = Total)) + 
  geom_line() + 
  scale_x_chron(format="%H:%M",n=8, minor_breaks=seq(0,1,1/24))
p + 
  ggtitle("Average workday traffic\nby counter, both directions") + 
  facet_grid(Location ~ .)

# test plot: typical weekday for each counter, arranged in facets, divided by direction/type
p <- ggplot(Averagedays_long[Averagedays_long$Workday & 
                          !Averagedays_long$Likely.abnormal & 
                          !Averagedays_long$OPM.action &
                          Averagedays_long$variable %in% c("PedIN","PedOUT","BikeIN","BikeOUT","PedTOTAL","BikeTOTAL","Total"),],
            aes(x = time, y = value, color = variable)) + geom_line()+ scale_x_chron(format="%H:%M",n=8) 
p + 
  ggtitle("Average workday traffic\nby counter") + 
  facet_grid(Location ~ .)

