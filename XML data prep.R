library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)

setwd("~/Dropbox/VT coursework/Capstone/Counter data") # Data dir on John's computer
# load("Arl_Webdata_Combined.Rda") # loaded data frame is called combineddata

#import manually constructed CSV with holidays and other dates of concern
holidays <- read.csv("Holidays.csv")

# done importing files, switch to Analysis dir
setwd("~/Dropbox/VT coursework/Capstone/Analysis")

#Transform the count vector from charactar to integer
combineddata$count <- type.convert(combineddata$count,as.is = TRUE) #as.is = TRUE prevents it becoming a factor variable, which shouldn't happen anyway

#now to set the dates to something understood as that in R, using Chron, lubridate (ugh) etc..
combineddata$date <- as.Date(combineddata$date,format="%m/%d/%Y")
#text version might still be useful, reverse it though so sorting works as expected
combineddata$date.text <- format(combineddata$date,format="%Y/%m/%d")

#and the same with the holidays file
holidays$date <- as.Date(holidays$Date,format="%d/%m/%Y")
# holidays$date.text <- format(holidays$date,format="%Y/%m/%d")


#setting the time to actual time of day, including adding seconds, because the time conversion didn't work without it
combineddata$time.text <- paste(combineddata$hour,combineddata$minute,"00",sep = ":")
combineddata$time <- times(combineddata$time.text,format="h:m:s")
#combineddata$time <- as.POSIXct(combineddata$time,format="%H:%M",tz="EST")
combineddata <- combineddata[!is.na(combineddata$time),]

#Create a combined datetime field, useful for plotting over ranges like weeks
combineddata$datetime <- as.POSIXct(paste(combineddata$date.text, combineddata$time.text, sep=" "), format="%Y/%m/%d %H:%M:%S",tz="EST")

#combineddata$time <- strptime(combineddata$time, "%H:%M:%S", tz = "")
#using awesome weekdays function to appropriately tag dates with days of the week and create a new column for that
combineddata$Day <- weekdays(combineddata$date)
combineddata$Week <- week(combineddata$date)
combineddata$Month <- months(combineddata$date)
combineddata$Quarter <- quarters(combineddata$date)
combineddata$Year <- year(combineddata$date)

# time for cleanup, dump data we can tell is bad
# no deletions yet, how many rows do we have?
TotalData <- nrow(combineddata)

#marker variable for deletions
combineddata$DeleteMe <- FALSE
#marker variable for weeks with deletions
combineddata$SkipWeek <- FALSE


# mark first day of each counter's data to remove unusual activity due to setup/calibration 

# mark days and weeks where there are data gaps in particular counters, we want to avoid screwy data from failing batteries, water intrusion etc.
## initialize
countersindata <- unique(combineddata$counter_num)
baddates <- as.list(rep(NA, length(countersindata)))
names(baddates) <- countersindata
badweeks <- baddates

## unlike the .csv files provided by David Patton, the XML download does not contain rows with NA counts - they just won't show up - so we start by enumerating the complete range
checktimes <- seq(min(combineddata$datetime),max(combineddata$datetime),by = "15 min")

## dates with missing data points per counter
for (i in countersindata) baddates[[i]] <- unique(date(as.POSIXlt(setdiff(checktimes,combineddata$datetime[combineddata$counter_num == i]),origin=origin)))
## mark all data points on each of those days
for (i in countersindata) combineddata$DeleteMe[combineddata$counter_num == i & 
                                                  combineddata$date %in% baddates[[i]]] <- TRUE

#also mark the first day of data for each counter (though it probably got caught by the above already)
for (i in countersindata) combineddata$DeleteMe[combineddata$counter_num == i & 
                                                combineddata$date == min(combineddata$date[
                                                  combineddata$counter_num == i])]<- TRUE
#what are those first days? It will be useful for the report
firstDays <- aggregate(date ~ counter_num, data= combineddata[!is.na(combineddata$count),],min)

#find weeks where data will be deleted for each counter
combineddata$YearWeek <- paste(combineddata$Year,combineddata$Week, sep = ".")
for (i in countersindata) badweeks[[i]] <- unique(combineddata$YearWeek[combineddata$DeleteMe == TRUE &
                                                                    combineddata$counter_num == i])
#mark the other data in those weeks for each counter
for (i in countersindata) combineddata$SkipWeek[combineddata$counter_num == i & 
                                                combineddata$YearWeek %in% badweeks[[i]]] <- TRUE
#how much data are we deleting?
RemovedDataPoints <- nrow(combineddata[combineddata$DeleteMe == TRUE,])
RemovedDataPct <- RemovedDataPoints/TotalData
RemovedWeekPoints <- nrow(combineddata[combineddata$SkipWeek == TRUE,])
RemovedWeekPct <- RemovedWeekPoints/TotalData


#### THE BIG DELETE! well, don't actually delete, in case we need to go back and fix something.
# So we'll move to a new data frame instead.
combineddata_Cleaned <- combineddata[combineddata$DeleteMe == FALSE,]

#how much data do we have per counter?
nPerCounter <- rename(count(combineddata_Cleaned, vars="counter_num"), c("freq" = "observations"))
nPerCounter$days = nPerCounter$observations / 96


combineddata_Cleaned <- merge(combineddata_Cleaned, holidays, by = "date", all = TRUE)
# This tells us when holidays ARE, but not when they AREN'T
# replace NA's as appropriate so we can do things like logical tests later
combineddata_Cleaned$Holiday[is.na(combineddata_Cleaned$Holiday)] <- FALSE
combineddata_Cleaned$Likely.abnormal[is.na(combineddata_Cleaned$Likely.abnormal)] <- FALSE
combineddata_Cleaned$OPM.action[is.na(combineddata_Cleaned$OPM.action)] <- FALSE

# Some of the holidays might not be represented in the data, remove the rows created in the merge
combineddata_Cleaned <- combineddata_Cleaned[!is.na(combineddata_Cleaned$count),]


#categorizing days by testing for weekend or weekday through "if else" statement using sunday and saturday matching test
#if the day is Saturday or Sunday, then it gets a True entry in new weekend column
combineddata_Cleaned$Weekend <- ifelse((combineddata_Cleaned$Day == "Sunday") | (combineddata_Cleaned$Day == "Saturday"),TRUE,FALSE)

#if the day is a weekend or a federal holiday then it is not a workday
combineddata_Cleaned$Workday <- ifelse(combineddata_Cleaned$Weekend | combineddata_Cleaned$Holiday, FALSE, TRUE)

######## MARK ROWS WITH TEXT LABEL FOR DIRECTION/MODE - looks like we need it in one variable for graphing ########
combineddata_Cleaned$dir_mode[combineddata_Cleaned$direction == "I"] <- "inbound"
combineddata_Cleaned$dir_mode[combineddata_Cleaned$direction == "O"] <- "outbound"
combineddata_Cleaned$dir_mode[combineddata_Cleaned$mode == "B"] <- paste(combineddata_Cleaned$dir_mode, "bicycle",sep = " ")
combineddata_Cleaned$dir_mode[combineddata_Cleaned$mode == "P"] <- paste(combineddata_Cleaned$dir_mode, "pedestrian",sep = " ")

#working directory sanity check, should already be set from above
setwd("~/Dropbox/VT coursework/Capstone/Analysis")
#### save R file
save(combineddata_Cleaned, file="Arl_Webdata_Cleaned.Rda")

# get the average time course at each location for weekdays vs. weekends vs. holidays vs. OPM closures
Averagedays <- aggregate(cbind(count)
                         ~ time+time.text+direction+mode+dir_mode+counter_num+Weekend+Workday+Holiday+Likely.abnormal+OPM.action,
                         data=combineddata_Cleaned,mean,na.action = na.pass
)


save(Averagedays, file="Averagedays.Rda")


# aggregate over periods of the day, using rush periods as defined by WMATA bike restrictions
rushbins <- times(c("00:00:00","07:00:00","10:00:00","16:00:00","19:00:00","23:59:59"))
combineddata_Cleaned$cuts <- cut(combineddata_Cleaned$time, rushbins,right=FALSE)
RushCounts <- aggregate(cbind(count)
                        ~ cuts+direction+mode+counter_num+Day+Weekend+Workday+Holiday+Likely.abnormal+OPM.action+Other.events,
                        data=combineddata_Cleaned,sum,na.action = na.pass
)
save(RushCounts, file="RushCounts.Rda")
# Same thing but calculate means
RushMeans <- aggregate(cbind(count)
                        ~ cuts+direction+mode+counter_num+Day+Weekend+Workday+Holiday+Likely.abnormal+OPM.action+Other.events,
                        data=combineddata_Cleaned,mean,na.action = na.pass
)
save(RushMeans, file="RushMeans.Rda")
