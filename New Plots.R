library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)
library(grid)

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

####BROKEN BELOW HERE

# test plot: typical weekday for each counter, arranged in facets, divided by direction/type
p <- ggplot(Averagedays_long[Averagedays_long$Workday & 
                          !Averagedays_long$Likely.abnormal & 
                          !Averagedays_long$OPM.action,],
            aes(x = time, y = value, color = dir_mode)) + geom_line()+ scale_x_chron(format="%H:%M",n=8) 
p + 
  ggtitle("Average workday traffic\nby counter") + 
  facet_grid(Location ~ .)

