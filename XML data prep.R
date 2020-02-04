library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)
library(stringi)

setwd("~/Google Drive (jstowe@vt.edu)/VT coursework/Capstone/Counter data") # Data dir on John's computer
 load("Arl_Webdata_Combined_newattempt.Rda") # loaded data frame is called combineddata
 load("counters.Rda") # loaded data frame is called counters

#import manually constructed CSV with holidays and other dates of concern
holidays <- read.csv("Holidays.csv")


# done importing files, switch to Analysis dir
setwd("~/Google Drive (jstowe@vt.edu)/VT coursework/Capstone/Analysis")

#The website's trail information is incomplete
#first, define factor levels to avoid potential mismatches and improve sorting
counters$counter_num <- factor(counters$counter_num, levels = c(1:max(as.integer(paste(counters$counter_num)))),ordered = TRUE) # might as well do it for the counters table as well

trails <- data.frame(trail_id = c(1:14), trail_name = c("W&OD","Custis","Four Mile Run","Mount Vernon","Bluemont Connector","Arlington Mill","110 Trail","Holmes Run","Eisenhower","Potomac Yard","Four Mile Park","Met Branch","Capital Crescent","Key Bridge"))
counters$trail_id <- factor(counters$trail_id,levels = c(1:max(trails$trail_id)),ordered = TRUE)
counters$trail_id[counters$counter_num %in% c(25)] <- "1" # Mark additional W&OD counters
counters$trail_id[counters$counter_num %in% c(24,28)] <- "2" # Mark additional Custis counters
counters$trail_id[counters$counter_num %in% c(5,6)] <- "3" # Mark additional FMR counters
counters$trail_id[counters$counter_num %in% c(9,11,30,31,34,36,41)] <- "4" # Mark additional MVT counters
counters$trail_id[counters$counter_num %in% c(23)] <- "5" # Mark Bluemont Connector counters
counters$trail_id[counters$counter_num %in% c(32)] <- "6" # Mark Arlington Mill counters
counters$trail_id[counters$counter_num %in% c(33)] <- "7" # Mark 110 Trail counters
counters$trail_id[counters$counter_num %in% c(37)] <- "8" # Mark Holmes Run counters
counters$trail_id[counters$counter_num %in% c(38)] <- "9" # Mark Eisenhower counters
counters$trail_id[counters$counter_num %in% c(39)] <- "10" # Mark Potomac Yard counters
counters$trail_id[counters$counter_num %in% c(42)] <- "11" # Mark Four Mile Park counters
counters$trail_id[counters$counter_num %in% c(45)] <- "12" # Mark MBT counters
counters$trail_id[counters$counter_num %in% c(47,48)] <- "13" # Mark CCT counters
counters$trail_id[counters$counter_num %in% c(7,8)] <- "14" # Mark CCT counters

counters <- merge(counters,trails, by = c("trail_id"),suffixes = c("_delete",""),all = TRUE) # mark the original trail name column for deletion
refcols <- c("counter_num","name","trail_id","trail_name")
counters <- counters[, c(refcols, setdiff(names(counters), refcols))] # reorder columns
counters <- counters[, !(names(counters) %in% c("trail_name_delete"))]

save(counters, file="Counters_processed.Rda")



#Transform the count vector from charactar to integer
if(is.character(combineddata$count)) combineddata$count <- type.convert(combineddata$count,as.is = TRUE) #as.is = TRUE prevents it becoming a factor variable, which shouldn't happen anyway

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
combineddata$mweek <- as.integer(stri_datetime_format(combineddata$date, format = "W")) # week of the month (for plotting as a calendar)

#For text variables in the above, sorting will default to alphabetical. Let's fix that by converting to factors.
combineddata$Day <- factor(combineddata$Day, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),ordered = TRUE)
combineddata$Month <- factor(combineddata$Month, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"), ordered = TRUE)
combineddata$counter_num <- factor(combineddata$counter_num, levels = c(1:max(as.integer(counters$counter_num))),ordered = TRUE) # defining the levels as all of the possible integer assignments to avoid potentially confusing mismatches
combineddata$direction <- factor(combineddata$direction)
combineddata$mode[combineddata$counter_num == "5"] <- "B" # some of this bike-only counter's data was mislabeled as "A" in the database
combineddata$mode <- factor(combineddata$mode)

## Counter 6, FMR pyro, counts bike/ped combined, but the ped counts can be recovered 
## by subtracting the bike counts from counter 5, FMR piezo 
# 
# commondt56 <- union(combineddata$datetime[combineddata$counter_num == "5"], 
#                     combineddata$datetime[combineddata$counter_num == "6"])
# counter6_ped <- merge(combineddata[combineddata$counter_num == "6" &
#                                      combineddata$datetime %in% commondt56,],
#                       combineddata[combineddata$counter_num == "5" &
#                                      combineddata$datetime %in% commondt56,
#                                    c("datetime","direction","count")],
#                       by = c("datetime","direction"),
#                       suffixes = c("_6","_5"))
# counter6_ped$count <- counter6_ped$count_6 - counter6_ped$count_5
## nearly 18% of the result was negative, so I'm commenting this section out and dropping counter 6 entirely.

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
for (i in countersindata) baddates[[as.character(i)]] <- unique(date(as.POSIXlt(setdiff(checktimes,combineddata$datetime[combineddata$counter_num == i]),origin=origin)))
## mark all data points on each of those days
for (i in countersindata) combineddata$DeleteMe[combineddata$counter_num == i & 
                                                  combineddata$date %in% baddates[[as.character(i)]]] <- TRUE

#also mark the first day of data for each counter (though it probably got caught by the above already)
for (i in countersindata) combineddata$DeleteMe[combineddata$counter_num == i & 
                                                combineddata$date == min(combineddata$date[
                                                  combineddata$counter_num == i])]<- TRUE
#what are those first days? It will be useful for the report and for checking integrity of the download
firstDays <- aggregate(date ~ counter_num, data= combineddata[!is.na(combineddata$count),],min)
counters <- merge(counters,firstDays)
names(counters)[names(counters) == "date"] <- "min_date_data"

#find weeks where data will be deleted for each counter
combineddata$YearWeek <- paste(combineddata$Year,combineddata$Week, sep = ".")
for (i in countersindata) badweeks[[as.character(i)]] <- unique(combineddata$YearWeek[combineddata$DeleteMe == TRUE &
                                                                    combineddata$counter_num == i])
#mark the other data in those weeks for each counter, in case we decide to dump it later
for (i in countersindata) combineddata$SkipWeek[combineddata$counter_num == i & 
                                                combineddata$YearWeek %in% badweeks[[as.character(i)]]] <- TRUE
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
combineddata_Cleaned$dir_mode[combineddata_Cleaned$mode == "B"] <- paste(combineddata_Cleaned$dir_mode[combineddata_Cleaned$mode == "B"], "bicycle",sep = " ")
combineddata_Cleaned$dir_mode[combineddata_Cleaned$mode == "P"] <- paste(combineddata_Cleaned$dir_mode[combineddata_Cleaned$mode == "P"], "pedestrian",sep = " ")
# This should be a factor, and let's specify the levels in order so that bikes and peds are grouped together in tables and graphs
combineddata_Cleaned$dir_mode <- factor(combineddata_Cleaned$dir_mode, levels = c("inbound bicycle","outbound bicycle","inbound pedestrian","outbound pedestrian"))

combineddata_Cleaned$Daytype <- factor(ifelse(combineddata_Cleaned$Workday,"Weekday",
                                          ifelse(combineddata_Cleaned$Holiday,"Holiday",
                                                 ifelse(combineddata_Cleaned$Weekend,"Weekend",
                                                        ifelse(combineddata_Cleaned$Likely.abnormal,"Likely abnormal",
                                                               ifelse(combineddata_Cleaned$OPM.action,"OPM action","Other.events")))))
                                   ,levels = c("Weekday","Weekend","Holiday","Likely abnormal","OPM action","Other.events"),ordered = TRUE)
#working directory sanity check, should already be set from above
setwd("~/Google Drive (jstowe@vt.edu)/VT coursework/Capstone/Analysis")
#### save R file
save(combineddata_Cleaned, file="Arl_Webdata_Cleaned.Rda")
save(counters, file = "Counters_processed.Rda")

# get the average time course at each location for weekdays vs. weekends vs. holidays vs. OPM closures
Averagedays <- aggregate(cbind(count)
                         ~ time+time.text+direction+mode+dir_mode+counter_num+Weekend+Workday+Holiday+Likely.abnormal+OPM.action,
                         data=combineddata_Cleaned,mean,na.action = na.pass
)


save(Averagedays, file="Averagedays.Rda")

# deviation/confidence intervals assume Poisson distribution for the counts
AveragedaysSE <- ddply(combineddata_Cleaned,
                       c("time","time.text","direction","mode","dir_mode","counter_num","Weekend","Workday","Holiday","Likely.abnormal","OPM.action"),
                       summarise,
                       N = length(count),
                       count_mean = mean(count),
                       sd = sqrt(count_mean),
                       ci_95 = 1.96 * sqrt(count_mean/N)
)


# get the average time course, by year, at each location for weekdays vs. weekends vs. holidays vs. OPM closures
Averagedays_year <- aggregate(cbind(count)
                         ~ time+time.text+direction+mode+dir_mode+counter_num+Weekend+Workday+Holiday+Likely.abnormal+OPM.action+Year,
                         data=combineddata_Cleaned,mean,na.action = na.pass
)

# Averagedays_year$Daytype <- factor(ifelse(Averagedays_year$Workday,"Weekday",
#                                           ifelse(Averagedays_year$Holiday,"Holiday",
#                                                  ifelse(Averagedays_year$Weekend,"Weekend",
#                                                         ifelse(Averagedays_year$Likely.abnormal,"Likely abnormal",
#                                                                ifelse(Averagedays_year$OPM.action,"OPM action","Weekend")))))
#                                    ,levels = c("Weekday","Weekend","Holiday","Likely abnormal","OPM action"),ordered = TRUE)

save(Averagedays_year, file="Averagedays_year.Rda")

# get the average time course, by month and year, at each location for weekdays vs. weekends vs. holidays vs. OPM closures
Averagedays_monthyear <- aggregate(cbind(count)
                              ~ time+time.text+direction+mode+dir_mode+counter_num+Weekend+Workday+Holiday+Likely.abnormal+OPM.action+Month+Year,
                              data=combineddata_Cleaned,mean,na.action = na.pass
)
# Averagedays_monthyear$Daytype <- factor(ifelse(Averagedays_monthyear$Workday,"Weekday",
#                                                ifelse(Averagedays_monthyear$Holiday,"Holiday",
#                                                       ifelse(Averagedays_monthyear$Weekend, "Weekend",
#                                                              ifelse(Averagedays_monthyear$Likely.abnormal,"Likely abnormal",
#                                                                     ifelse(Averagedays_monthyear$OPM.action,"OPM action","Weekend")))))
#                                         ,levels = c("Weekday","Weekend","Holiday","Likely abnormal","OPM action"),ordered = TRUE)

save(Averagedays_monthyear, file="Averagedays_monthyear.Rda")


# aggregate over periods of the day, using rush periods as defined by WMATA bike restrictions
rushbins <- times(c("00:00:00","07:00:00","10:00:00","16:00:00","19:00:00","23:59:59"))
combineddata_Cleaned$cuts <- cut(combineddata_Cleaned$time, rushbins,right=FALSE)
RushCounts <- aggregate(cbind(count)
                        ~ cuts+direction+mode+counter_num+Weekend+Workday+Holiday+Likely.abnormal+OPM.action+Other.events,
                        data=combineddata_Cleaned,sum,na.action = na.pass
)
# RushCounts$Daytype <- factor(ifelse(RushCounts$Workday,"Weekday",
#                                           ifelse(RushCounts$Holiday,"Holiday",
#                                                  ifelse(RushCounts$Weekend,"Weekend",
#                                                         ifelse(RushCounts$Likely.abnormal,"Likely abnormal",
#                                                                ifelse(RushCounts$OPM.action,"OPM action",
#                                                                       ifelse(RushCounts$Other.events,"Other events","Weekend"))))))
#                              ,levels = c("Weekday","Weekend","Holiday","Likely abnormal","OPM action","Other events"),ordered = TRUE)

save(RushCounts, file="RushCounts.Rda")
# Same thing but calculate means
RushMeans <- aggregate(cbind(count)
                        ~ cuts+direction+mode+counter_num+Weekend+Workday+Holiday+Likely.abnormal+OPM.action+Other.events,
                        data=combineddata_Cleaned,mean,na.action = na.pass
)

# RushMeans$Daytype <- factor(ifelse(RushMeans$Workday,"Weekday",
#                                           ifelse(RushMeans$Holiday,"Holiday",
#                                                  ifelse(RushMeans$Weekend,"Weekend",
#                                                         ifelse(RushMeans$Likely.abnormal,"Likely abnormal",
#                                                                ifelse(RushMeans$OPM.action,"OPM action",
#                                                                       ifelse(RushMeans$Other.events,"Other events","Weekend"))))))
#                                    ,levels = c("Weekday","Weekend","Holiday","Likely abnormal","OPM action","Other events"),ordered = TRUE)
save(RushMeans, file="RushMeans.Rda")

# and by month/year
RushMeans_monthyear <- aggregate(cbind(count)
                       ~ cuts+direction+mode+counter_num+Weekend+Workday+Holiday+Likely.abnormal+OPM.action+Month+Year,
                       data=combineddata_Cleaned,mean,na.action = na.pass
)

# RushMeans_monthyear$Daytype <- factor(ifelse(RushMeans_monthyear$Workday,"Weekday",
#                                    ifelse(RushMeans_monthyear$Holiday,"Holiday",
#                                           ifelse(RushMeans_monthyear$Weekend, "Weekend",
#                                                  ifelse(RushMeans_monthyear$Likely.abnormal,"Likely abnormal",
#                                                         ifelse(RushMeans_monthyear$OPM.action,"OPM action",
#                                                                ifelse(RushMeans_monthyear$Other.events,"Other events","Weekend"))))))
#                             ,levels = c("Weekday","Weekend","Holiday","Likely abnormal","OPM action","Other events"),ordered = TRUE)
save(RushMeans_monthyear, file="RushMeans_monthyear.Rda")

# Info for drawing rush hour rectangles on the graphs
# Include the following in ggplot2 statements:
# geom_rect(data=RushRects, mapping = aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2)
RushRects <- data.frame(xmin = c(7/24, 16/24),xmax = c(10/24,19/24),ymin = -Inf, ymax = Inf, Daytype = factor(c("Weekday","Weekday"), levels = c("Weekday","Weekend","Holiday","Likely abnormal","OPM action","Other events")))
save(RushRects, file="RushRects.Rda")