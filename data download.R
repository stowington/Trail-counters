library(XML)
library(stringr) # for str_match
library(data.table) # for rbindlist
library(lubridate) # for date checking

# sample code from https://github.com/bfrickert/data_product_project/blob/master/data_product_project/index.Rmd
#url <- 'http://webservices.commuterpage.com/counters.cfc?wsdl&method=GetCountInDateRange&counterid=4&startDate=01/01/2005&endDate=10/31/2015&direction=&mode=B&interval=d'
#doc <- xmlTreeParse(url, useInternal=T)
#top <- xmlRoot(doc)
#df <- data.frame(matrix(unlist(xmlApply(top, xmlAttrs)), ncol=4, byrow=T),stringsAsFactors = F)
#names(df) <- c('count','date','direction', 'type')


downloadCounterXMLtoDF <- function(url,sleep = 300) {
  
  doc <- try(xmlTreeParse(url, useInternal=T)) #downloads sometimes fail, we want to keep going with the rest
  if(inherits(doc,"try-error")) { 
    print(paste("Download failed on url ", url))
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
  print(paste("Downloaded counter:",df$counter_num[1],"direction:",df$direction[1],"mode:",df$mode[1],sep=" "))
  
  Sys.sleep(sleep) # wait since the server seems to choke with rapid large requests. Default is 300 seconds = 5 minutes
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
# add min dates for each counter
incomingdata <- lapply(counters$counter_num,downloadCounterMinDatestoDF)
counters <- merge(counters,rbindlist(incomingdata))
counters$min_date_web <- mdy(counters$min_date_web)


# Which counters do we want? For now, only trails in Arlington
# WARNING! THESE NUMBERS DO NOT ALIGN WITH THE DATA SET SENT IN 2012 BY DAVID PATTON
countersofinterest <- c(1,2,3,5,9,11,12,23,24,25,28,30,31,32,33,34,36,37,38,39,41,42,45,47,48) # 11/16/17: removed counter 4 (no direction information) and counter 6 (no mode information)


# dates for study: 1/1/2009 - 6/3/2016
datesofinterest <- c("1/1/2010","6/30/2010")

# Assemble all the query URLs for the webserver
## HARD WAY with different factors
#factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
#counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
#counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',countersofinterest,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&interval=m')

#  now use lapply with our custom download function to do the downloading/prep on the counter_urls
#incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
#testdf <- downloadCounterXMLtoDF(counter_urls[1]) #worked

# Merge merge merge

## HARDEST WAY download in date range chunks so as not to overload the server
# piecemealdata <- list()
# 
# datesofinterest <- c("1/1/2009","6/30/2009")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["09a"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("7/1/2009","12/31/2009")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["09b"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("1/1/2010","6/30/2010")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["10a"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("7/1/2010","12/31/2010")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["10b"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("1/1/2011","6/30/2011")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["11a"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("7/1/2011","12/31/2011")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["11b"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("1/1/2012","6/30/2012")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["12a"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("7/1/2012","12/31/2012")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["12b"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("1/1/2013","6/30/2013")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["13a"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("7/1/2013","12/31/2013")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["13b"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("1/1/2014","6/30/2014")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["14a"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("7/1/2014","12/31/2014")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["14b"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("1/1/2015","6/30/2015")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["15a"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("7/1/2015","12/31/2015")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["15b"]] <- rbindlist(incomingdata)
# 
# datesofinterest <- c("1/1/2016","6/03/2016")
# factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
# counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m')
# incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF)
# piecemealdata[["16a"]] <- rbindlist(incomingdata)
# 
# combineddata <- rbindlist(piecemealdata)

##  MORE COMPACT VERSION, easier to adjust date ranges and intervals
startdate <- min(counters$min_date_web[counters$counter_num %in% countersofinterest]) # earliest start date of the counters we care about
enddate <- mdy("6/03/2016") # for paper: "6/03/2016"
intermediate_dates <- c(seq(startdate,enddate,by = "6 months"),enddate) #adjust interval if necessary
counter_urls <- c() #initialize list of request URLs
for (i in 1:(length(intermediate_dates)-1)) {
  activecounters <- counters$counter_num[counters$counter_num %in% countersofinterest & 
                                           counters$min_date_web < intermediate_dates[i +1]]
  datesofinterest <- c(format(intermediate_dates[i],format="%m/%d/%Y"),
                       format(intermediate_dates[i+1]-1,format="%m/%d/%Y"))
  
  factors <- expand.grid(counterid = activecounters, direction = c("I","O"), mode = c("P","B"))
  counter_urls <- append(counter_urls,paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=',datesofinterest[1],'&endDate=',datesofinterest[2],'&direction=',factors$direction,'&mode=',factors$mode,'&interval=m'))
  print(paste("i =",i,"activecounters =",paste(activecounters,collapse = ", ")))
}
incomingdata <- lapply(counter_urls,downloadCounterXMLtoDF, sleep = 180)
combineddatatest <- rbindlist(incomingdata)

setwd("~/Dropbox/VT coursework/Capstone/Counter data")
#### save R file
save(combineddata, file="Arl_Webdata_Combined.Rda")
save(counters, file="counters.Rda")

 ## Clean up large temp variables
 rm(list=c("incomingdata","piecemealdata"))

##### save CSV in shared dir
# setwd("~/Dropbox/Processed Counter Data & R code/") # Dir on John's computer
# write.csv(combineddata, "Arl_Webdata_Combined.csv")



