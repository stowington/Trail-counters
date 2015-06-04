library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)


#now to import the CSV as data, please make sure this file is in the same folder as the CSV
#set working directory to same folder where data is stored to keep code tidy
# setwd("/Users/Jos\h/Desktop/Dropbox/Processed Counter Data & R code/")
# setwd("~/Dropbox/Processed Counter Data & R code/") # Shared Dir on John's computer

setwd("~/Dropbox/VT coursework/Capstone/Counter data") # Data dir on John's computer
load("Arl_MUT_Combined.Rda")

#import big CSV 
# Arl_MUT_Combined <- read.csv("Arl_MUT_Combined.csv")
#import CSV with holidays and other dates of concern
holidays <- read.csv("Holidays.csv")

# done importing files, switch to Analysis dir
setwd("~/Dropbox/VT coursework/Capstone/Analysis")

#now to set the dates to something understood as that in R, using Chron, lubridate (ugh) etc..
Arl_MUT_Combined$Date <- as.Date(Arl_MUT_Combined$Date,format="%d/%m/%Y")
#text version might still be useful, reverse it though so sorting works as expected
Arl_MUT_Combined$Date.text <- format(Arl_MUT_Combined$Date,format="%Y/%m/%d")

#and the same with the holidays file
holidays$Date <- as.Date(holidays$Date,format="%d/%m/%Y")
# holidays$Date.text <- format(holidays$Date,format="%Y/%m/%d")

#setting the time to actual time of day, including adding seconds.. cuz. we can? -- Because the time conversion didn't work without it! -John
Arl_MUT_Combined$time.text <- Arl_MUT_Combined$time
Arl_MUT_Combined$time <- times(paste(Arl_MUT_Combined$time,":00",sep=""),format="h:m:s")
#Arl_MUT_Combined$time <- as.POSIXct(Arl_MUT_Combined$time,format="%H:%M",tz="EST")
Arl_MUT_Combined <- Arl_MUT_Combined[!is.na(Arl_MUT_Combined$time),]

#Create a combined datetime field, useful for plotting over ranges like weeks
Arl_MUT_Combined$datetime <- as.POSIXct(paste(Arl_MUT_Combined$Date, Arl_MUT_Combined$time), format="%d/%m/%Y %H:%M:%S",tz="EST")

#Arl_MUT_Combined$time <- strptime(Arl_MUT_Combined$time, "%H:%M:%S", tz = "")
#using awesome weekdays function to appropriately tag dates with days of the week and create a new column for that
Arl_MUT_Combined$Day <- weekdays(Arl_MUT_Combined$Date)
Arl_MUT_Combined$Week <- week(Arl_MUT_Combined$Date)
Arl_MUT_Combined$Month <- months(Arl_MUT_Combined$Date)
Arl_MUT_Combined$Quarter <- quarters(Arl_MUT_Combined$Date)

# time for cleanup, dump data we can tell is bad
# no deletions yet, how many rows do we have?
TotalData <- nrow(Arl_MUT_Combined)

#marker variable for deletions
Arl_MUT_Combined$DeleteMe <- FALSE
#marker variable for weeks with deletions
Arl_MUT_Combined$SkipWeek <- FALSE

# mark first day of each counter's data to remove unusual activity due to setup/calibration 

# mark days and weeks where there are data gaps in particular counters, we don't want screwy data

#deal with other 
counters <- levels(Arl_MUT_Combined$CounterID)
baddates <- list()
badweeks <- list()
#get lists of days with missing data for all the counters
for (i in counters) baddates[[i]] <- unique(Arl_MUT_Combined$Date[is.na(Arl_MUT_Combined$Total) 
                                                            & Arl_MUT_Combined$CounterID == i])
#mark all data from those days for deletion
for (i in counters) Arl_MUT_Combined$DeleteMe[Arl_MUT_Combined$CounterID == i & 
                                                Arl_MUT_Combined$Date %in% baddates[[i]]] <- TRUE
#also mark the first day of data for each counter (though it probably got caught by the above already)
for (i in counters) Arl_MUT_Combined$DeleteMe[Arl_MUT_Combined$CounterID == i & 
                                                Arl_MUT_Combined$Date == min(Arl_MUT_Combined$Date[
                                                  Arl_MUT_Combined$CounterID == i])]<- TRUE
#what are those first days? It will be useful for the report
firstDays <- aggregate(Date ~ CounterID, data= Arl_MUT_Combined[!is.na(Arl_MUT_Combined$Total),],min)
#find weeks where data will be deleted for each counter
for (i in counters) badweeks[[i]] <- unique(Arl_MUT_Combined$Week[Arl_MUT_Combined$DeleteMe == TRUE 
                                                                  & Arl_MUT_Combined$CounterID == i])
#mark the other data in those weeks for each counter
for (i in counters) Arl_MUT_Combined$SkipWeek[Arl_MUT_Combined$CounterID == i & 
                                                Arl_MUT_Combined$Week %in% badweeks[[i]]] <- TRUE
#how much data are we deleting?
RemovedDataPoints <- nrow(Arl_MUT_Combined[Arl_MUT_Combined$DeleteMe == TRUE,])
RemovedDataPct <- RemovedDataPoints/TotalData



#### THE BIG DELETE! well, don't actually delete, in case we need to go back and fix something.
# So we'll move to a new data frame instead.
Arl_MUT_Cleaned <- Arl_MUT_Combined[Arl_MUT_Combined$DeleteMe == FALSE,]
                          
#how much data do we have per counter?
nPerCounter <- count(Arl_MUT_Cleaned, vars="CounterID")
nPerCounter$days = nPerCounter$freq / 96


Arl_MUT_Cleaned <- merge(Arl_MUT_Cleaned, holidays, by = "Date", all = TRUE)
# This tells us when holidays ARE, but not when they AREN'T
# replace NA's as appropriate so we can do things like logical tests later
Arl_MUT_Cleaned$Holiday[is.na(Arl_MUT_Cleaned$Holiday)] <- FALSE
Arl_MUT_Cleaned$Likely.abnormal[is.na(Arl_MUT_Cleaned$Likely.abnormal)] <- FALSE
Arl_MUT_Cleaned$OPM.action[is.na(Arl_MUT_Cleaned$OPM.action)] <- FALSE

# Some of the holidays might not be represented in the data, remove the rows created in the merge
Arl_MUT_Cleaned <- Arl_MUT_Cleaned[!is.na(Arl_MUT_Cleaned$Total),]


#categorizing days by testing for weekend or weekday through "if else" statement using sunday and saturday matching test
#if the day is Saturday or Sunday, then it gets a True entry in new weekend column
Arl_MUT_Cleaned$Weekend <- ifelse((Arl_MUT_Cleaned$Day == "Sunday") | (Arl_MUT_Cleaned$Day == "Saturday"),TRUE,FALSE)

#if the day is a weekend or a federal holiday then it is not a workday
Arl_MUT_Cleaned$Workday <- ifelse(Arl_MUT_Cleaned$Weekend | Arl_MUT_Cleaned$Holiday, FALSE, TRUE)

######### SQUASH INTO DIFFERENT FORMATS FOR DIFFERENT PURPOSES #########

# melt in case we need a truly "vertical" data frame.
# as.data.frame bit is a hack to get around a bug where melt breaks if ID vars have attributes attached
Arl_MUT_Melt <- melt(as.data.frame(lapply(Arl_MUT_Cleaned,as.vector)), measure.vars = c("PedIN","PedOUT","BikeIN","BikeOUT"),
                     id.vars=c("Date","time","datetime","Date.text","time.text","Location",
                               "Day","Holiday","Likely.abnormal","OPM.action","Other.events",
                               "Weekend","Workday"))
Arl_MUT_Melt$Mode <- ifelse(grepl("Ped",Arl_MUT_Melt$variable),"Ped","Bike")
Arl_MUT_Melt$Dir <- ifelse(grepl("IN",Arl_MUT_Melt$variable),"In","Out")

write.csv(Arl_MUT_Melt, "Arl_MUT_Melt.csv")
save(Arl_MUT_Melt, file="Arl_MUT_Melt.Rda")

# aggregate over periods of the day, using rush periods as defined by WMATA bike restrictions
rushbins <- times(c("00:00:00","07:00:00","10:00:00","16:00:00","19:00:00","23:59:59"))
Arl_MUT_Cleaned$cuts <- cut(Arl_MUT_Cleaned$time, rushbins,right=FALSE)
RushCounts <- aggregate(cbind(Total,PedIN,PedOUT,BikeIN,BikeOUT,PedTOTAL,BikeTOTAL,TotalOUT,TotalIN)
 	~ Date+cuts+Day+Weekend+Workday+Location+CounterID+Holiday+Likely.abnormal+OPM.action+Other.events,
	data=Arl_MUT_Cleaned,sum,na.action = na.pass
	)
write.csv(RushCounts, "RushCounts.csv")
save(RushCounts, file="RushCounts.Rda")

# get the average time course at each location for weekdays vs. weekends vs. holidays vs. OPM closures
Averagedays <- aggregate(cbind(Total,PedIN,PedOUT,BikeIN,BikeOUT,PedTOTAL,BikeTOTAL,TotalOUT,TotalIN)
 	~ time+time.text+Weekend+Workday+Location+CounterID+Holiday+Likely.abnormal+OPM.action+Other.events,
	data=Arl_MUT_Cleaned,mean,na.action = na.pass
	)
write.csv(Averagedays, "Averagedays.csv")
save(Averagedays, file="Averagedays.Rda")
# break out by weekday, for workdays only
Average.weekdays <- aggregate(cbind(Total,PedIN,PedOUT,BikeIN,BikeOUT,PedTOTAL,BikeTOTAL,TotalOUT,TotalIN)
                         ~ time+time.text+Day+Location+CounterID+Likely.abnormal+OPM.action+Other.events,
                         data=Arl_MUT_Cleaned[Arl_MUT_Cleaned$Workday == TRUE,],mean,na.action = na.pass
)

# ... and melted, averaged
Averagedays_long <- melt(as.data.frame(lapply(Averagedays,as.vector)), measure.vars = c("PedIN","PedOUT","BikeIN","BikeOUT","Total","PedTOTAL","BikeTOTAL","TotalOUT","TotalIN"),
                     id.vars=c("time","time.text","Location",
                               "Holiday","Likely.abnormal","OPM.action","Other.events",
                               "Weekend","Workday"))
Averagedays_long$Mode <- ifelse(grepl("Ped",Averagedays_long$variable),"Ped","Bike")
Averagedays_long$Dir <- ifelse(grepl("IN",Averagedays_long$variable),"In","Out")

write.csv(Averagedays_long, "Averagedays_Melt.csv")
save(Averagedays_long, file="Averagedays_Melt.Rda")
