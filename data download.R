library(XML)
library(stringr) # for str_match
library(data.table) # for rbindlist
library(lubridate) # for date checking

# sample code from https://github.com/bfrickert/Bike_Arlington/blob/master/server.R   #link updated Feb. 29 2020
#url <- 'http://webservices.commuterpage.com/counters.cfc?wsdl&method=GetCountInDateRange&counterid=4&startDate=01/01/2005&endDate=10/31/2015&direction=&mode=B&interval=d'
#doc <- xmlTreeParse(url, useInternal=T)
#top <- xmlRoot(doc)
#df <- data.frame(matrix(unlist(xmlApply(top, xmlAttrs)), ncol=4, byrow=T),stringsAsFactors = F)
#names(df) <- c('count','date','direction', 'type')

failed_urls <- c()
downloadCounterXMLtoDF <- function(url,sleep = 0) {
  
  Sys.sleep(sleep) # wait since the server seems to choke with rapid large requests. Default is 300 seconds = 5 minutes
  
  doc <- try(xmlTreeParse(url, useInternal=T)) #downloads sometimes fail, we want to keep going with the rest
  if(inherits(doc,"try-error")) { 
    print(paste("Download failed on url ", url))
    failed_urls <<- append(failed_urls,url)
    #return empty data frame to keep batch downloads running
    return(data.frame(count=character(),
                      date=character(),
                      direction=character(),
                      hour=character(),
                      minute=character(),
                      mode=character(),
                      counter_num=character(),
                      stringsAsFactors = FALSE))
  }
  top <- xmlRoot(doc)
  df <- try(data.frame(matrix(unlist(xmlApply(top, xmlAttrs)), ncol=6, byrow=T),stringsAsFactors = F))
  if(inherits(df,"try-error")) { 
    print(paste("Parse failed on url ", url))
    failed_urls <<- append(failed_urls,url)
    #return empty data frame to keep batch downloads running
    return(data.frame(count=character(),
                      date=character(),
                      direction=character(),
                      hour=character(),
                      minute=character(),
                      mode=character(),
                      counter_num=character(),
                      stringsAsFactors = FALSE))
  }
  names(df) <- c('count','date','direction', 'hour','minute','mode')
  
  #add counter id from the URL
  r <- "&counterid=(\\d+)" 
  df$counter_num <- str_match(url,r)[[2]]
  # give an update on what we just downloaded
  print(paste("Downloaded counter:",df$counter_num[1],"direction:",df$direction[1],"mode:",df$mode[1],"date:",df$date[1],sep=" "))
  
  return(df)
  
}

downloadCounterMinDatestoDF <- function(counternum) {
  min_url <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&method=GetMinDates&CounterID=',counternum)
  
  doc <- try(xmlParse(min_url)) #downloads sometimes fail, we want to keep going with the rest
  if(inherits(doc,"try-error")) { 
    print(paste("Download failed on url ", url))
    #return empty data frame to keep batch downloads running
    mindate <- ""
  } else {
    d <- xmlToList(doc)
    mindate <- d[["data"]][["struct"]][["var"]][["string"]]
  }
  
  return(data.frame(counter_num=as.character(counternum),min_date_web=mindate,stringsAsFactors = FALSE))
  
  
}

downloadCounterMaxDatestoDF <- function(counternum) {
  min_url <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&method=GetMaxDates&CounterID=',counternum)
  
  doc <- try(xmlParse(min_url)) #downloads sometimes fail, we want to keep going with the rest
  if(inherits(doc,"try-error")) { 
    print(paste("Download failed on url ", url))
    #return empty data frame to keep batch downloads running
    maxdate <- ""
  } else {
    d <- xmlToList(doc)
    maxdate <- d[["data"]][["struct"]][["var"]][["string"]]
  }
  
  return(data.frame(counter_num=as.character(counternum),max_date_web=maxdate,stringsAsFactors = FALSE))
  
  
}

# df_test8 <- downloadCounterXMLtoDF('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=30&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m')


downloadcounterinfo <-function() {
  doc <- xmlTreeParse("http://webservices.commuterpage.com/counters.cfc?wsdl&method=GetAllCounters", useInternal=T)
  top <- xmlRoot(doc)
  counter_num <- xmlSApply(top,xmlAttrs) # extract counter id numbers stored as attributes of the child notes
  attrs <- xmlSApply(top, function(x) xmlSApply(x, xmlValue)) # extract counter information stored as tags in the child nodes
  df <- data.frame(t(rbind(counter_num,attrs)))
  df$region_num <- str_sub(df$region,-1,-1)
  df$region <- str_sub(df$region,1,-2)
  rownames(df) <- c() # clean up
  return(df)
}

# grab counter info (names, coordinates)
counters <- downloadcounterinfo()
# add min and max dates for each counter - useful for double-checking the data later, and for avoiding unnecessary download attempts
incomingdata <- lapply(counters$counter_num,downloadCounterMinDatestoDF)
counters <- merge(counters,rbindlist(incomingdata))
counters$min_date_web <- mdy(counters$min_date_web)

incomingdata <- lapply(counters$counter_num,downloadCounterMaxDatestoDF)
counters <- merge(counters,rbindlist(incomingdata))
counters$max_date_web <- mdy(counters$max_date_web)


# Which counters do we want? For now, only trails in Arlington
# WARNING! THESE NUMBERS DO NOT ALIGN WITH THE DATA SET SENT IN 2012 BY DAVID PATTON
# # 11/16/17: removed counter 4 (no direction information) and counter 6 (no mode information)
countersofinterest <- c(1,2,3,5,9,11,12,23,24,25,28,30,31,32,33,34,36,37,38,39,41,42,45,47,48) 


# dates for study: 1/1/2009 - 6/3/2016
# datesofinterest <- c("1/1/2010","6/30/2010")

##  MORE COMPACT VERSION, easier to adjust date ranges and intervals
startdate <- min(counters$min_date_web[counters$counter_num %in% countersofinterest]) # earliest start date of the counters we care about
enddate <- mdy("6/04/2016") # for paper: "6/03/2016" but code below subtracts a day so use 6/4
intermediate_dates <- c(seq(startdate,enddate,by = "6 months"),enddate) #adjust interval if necessary
counter_urls <- c() #initialize list of request URLs

### Assemble list of URLS, checking first to see if the counter was active during each time interval
for (i in 1:(length(intermediate_dates)-1)) {
  activecounters <- counters$counter_num[counters$counter_num %in% countersofinterest & 
                                           counters$min_date_web < intermediate_dates[i +1] &
                                           counters$max_date_web > intermediate_dates[i] - 1]
  datesofinterest <- c(format(intermediate_dates[i],format="%m/%d/%Y"),
                       format(intermediate_dates[i+1]-1,format="%m/%d/%Y"))
  
  # print(paste("i =",i,"activecounters =",paste(activecounters,collapse = ", "))) ## for debugging
  
  if (5 %in% activecounters){ # some of this bike-only counter's data was miscoded as "A" for "All Modes" and gets skipped if we ask for bike explicitly
    factors <- expand.grid(counterid = 5, direction = c("I","O"))
    counter_urls <- append(counter_urls,paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&interval=m'))
    activecounters <- activecounters[activecounters!=5]
  }
  
  if (length(activecounters) > 0){
    factors <- expand.grid(counterid = activecounters, direction = c("I","O"), mode = c("P","B"))
    counter_urls <- append(counter_urls,paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m'))
  }
  
}


#### multithread requests? Doesn't work and would probably overload the server anyway
# library(future)
# plan(multiprocess)
# downloadCounterXMLtoSingleDF <- function(urls,sleep = 300) {
#   incomingdata <- future_lapply(urls,downloadCounterXMLtoDF, sleep)
#   rbindlist(incomingdata)
# }
# combineddata <- downloadCounterXMLtoSingleDF(test_urls, sleep = 180)


#  now use lapply with our custom download function to do the downloading/prep on the counter_urls
system.time({incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF, sleep = 0)
combineddata <- rbindlist(incomingdata)})
# retry the downloads on the failed urls, waiting a longer time between requests
system.time({incomingdata <- lapply(failed_urls,downloadCounterXMLtoDF, sleep = 300)
retried <- rbindlist(incomingdata)})
if(nrow(retried) > 0) {
  combineddata <- rbind(combineddata,retried)
}

setwd("~/Google Drive (jstowe@vt.edu)/VT coursework/Capstone/Counter data")
#### save R file
# save(combineddata, file="Arl_Webdata_Combined.Rda")
# save(counters, file="counters.Rda")

## Clean up large temp variables
rm(list=c("incomingdata","piecemealdata"))

##### save CSV in shared dir
# setwd("~/Google Drive (jstowe@vt.edu)/Processed Counter Data & R code/") # Dir on John's computer
# write.csv(combineddata, "Arl_Webdata_Combined.csv")
# 
# ##### For appending additional data to existing dataset (USE WITH CAUTION!):
# # combineddata_new <- combineddata # rename combineddata before loading the file which is also called combineddata
# # setwd("~/Google Drive (jstowe@vt.edu)/VT coursework/Capstone/Counter data") # Data dir on John's computer
# # load("Arl_Webdata_Combined.Rda") # loaded data frame is called combineddata
# # combineddata <- unique(rbind(combineddata,combineddata_new)) # add the new stuff, skip duplicates
# # save(combineddata, file="Arl_Webdata_Combined.Rda")