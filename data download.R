library(XML)
library(stringr)
library(data.table)

# sample code from https://github.com/bfrickert/data_product_project/blob/master/data_product_project/index.Rmd
#url <- 'http://webservices.commuterpage.com/counters.cfc?wsdl&method=GetCountInDateRange&counterid=4&startDate=01/01/2005&endDate=10/31/2015&direction=&mode=B&interval=d'
#doc <- xmlTreeParse(url, useInternal=T)
#top <- xmlRoot(doc)
#df <- data.frame(matrix(unlist(xmlApply(top, xmlAttrs)), ncol=4, byrow=T),stringsAsFactors = F)
#names(df) <- c('count','date','direction', 'type')


downloadCounterXMLtoDF <- function(url) {
  doc <- xmlTreeParse(url, useInternal=T)
  top <- xmlRoot(doc)
  df <- data.frame(matrix(unlist(xmlApply(top, xmlAttrs)), ncol=6, byrow=T),stringsAsFactors = F)
  names(df) <- c('count','date','direction', 'hour','minute','mode')
  
  #add counter id from the URL
  r <- "&counterid=(\\d+)" 
  df$counter_num <- str_match(url,r)[[2]]
  
  return(df)
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

# Which counters do we want? For now, only trails in Arlington
# WARNING! THESE NUMBERS DO NOT ALIGN WITH THE DATA SET SENT BY DAVID PATTON
countersofinterest <- c(1,2,3,4,5,6,9,11,12,23,24,25,28,30,31,32,33,34,36,37,38,39,41,42)

# Assemble all the query URLs for the webserver
factors <- expand.grid(counterid = countersofinterest, direction = c("I","O"), mode = c("P","B"))
counter_urls <- paste0('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=',factors$counterid,'&method=GetCountInDateRange&startDate=1/1/2009&endDate=06/03/2016&direction=',factors$direction,'&mode=',factors$mode,'&startTime=0:00&endTime=23:59&interval=m')

#  now use lapply with our custom download function to do the downloading/prep on the counter_urls
incomingdata <- lapply(counter_urls[1:2],downloadCounterXMLtoDF)
#testdf <- downloadCounterXMLtoDF(counter_urls[1]) #worked

# Merge merge merge
combineddata <- rbindlist(incomingdata)

setwd("~/Dropbox/VT coursework/Capstone/Counter data")
#### save R file
# save(combineddata, file="Arl_Webdata_Combined.Rda")


##### save CSV in shared dir
# setwd("~/Dropbox/Processed Counter Data & R code/") # Dir on John's computer
# write.csv(combineddata, "Arl_Webdata_Combined.csv")



