library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)

setwd("~/Dropbox/VT coursework/Capstone/Analysis") # Dir for prepped data on John's computer
load("Averagedays.Rda")
load("RushCounts.Rda")




# test plot: typical weekday for the Rosslyn counter on normal workdays with no OPM action or other severe abnormalities
p <- ggplot(Averagedays[Averagedays$Workday & 
                          !Averagedays$Likely.abnormal & 
                          !Averagedays$OPM.action & 
                          Averagedays$Location == "Custis_Rosslyn",],
            aes(x = time, y = Total)) + geom_line()
p + scale_x_chron(format="%H:%M")


Melted_Workdays <- Arl_MUT_Melt[Arl_MUT_Melt$Workday==TRUE,]
p <- ggplot(Melted_Workdays[Averagedays$Location == "Custis_Rosslyn",],aes(x=datetime,y=value,group=variable))
p + geom_line()

p <- ggplot(Melted_Workdays[Averagedays$Location == "Custis_Rosslyn",], aes(x = datetime)) + geom_bar(binwidth=1/24/60)
p + scale_x_chron(format="%H:%M")
