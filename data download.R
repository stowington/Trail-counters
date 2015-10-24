library(XML)

# sample code from https://github.com/bfrickert/data_product_project/blob/master/data_product_project/index.Rmd
url <- 'http://webservices.commuterpage.com/counters.cfc?wsdl&method=GetCountInDateRange&counterid=4&startDate=01/01/2005&endDate=10/31/2015&direction=&mode=B&interval=d'
doc <- xmlTreeParse(url, useInternal=T)
top <- xmlRoot(doc)
df <- data.frame(matrix(unlist(xmlApply(top, xmlAttrs)), ncol=4, byrow=T),stringsAsFactors = F)
names(df) <- c('count','date','direction', 'type')

urls <- c(
  
  'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=1&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
  'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=2&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m'#,
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=3&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=4&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=5&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=6&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=7&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=8&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=9&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=10&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=11&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=12&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=13&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=14&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=15&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=16&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=17&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=18&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=19&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=20&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=21&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=22&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=23&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=24&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=25&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=26&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=27&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=28&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=29&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=30&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   
#   
#   
#   
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=1&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=2&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=3&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=4&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=5&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=6&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=7&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=8&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=9&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=10&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=11&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=12&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=13&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=14&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=15&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=16&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=17&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=18&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=19&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=20&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=21&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=22&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=23&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=24&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=25&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=26&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=27&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=28&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=29&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=30&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=B&startTime=0:00&endTime=23:59&interval=m',
#   
#   
#   
#   
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=1&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=2&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=3&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=4&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=5&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=6&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=7&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=8&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=9&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=10&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=11&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=12&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=13&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=14&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=15&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=16&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=17&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=18&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=19&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=20&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=21&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=22&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=23&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=24&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=25&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=26&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=27&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=28&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=29&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=30&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   
#   
#   
#   
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=1&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=2&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=3&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=4&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=5&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=6&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=7&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=8&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=9&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=10&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=11&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=12&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=13&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=14&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=15&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=16&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=17&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=18&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=19&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=20&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=21&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=22&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=23&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=24&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=25&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=26&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=27&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=28&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=29&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m',
#   'http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=30&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=O&mode=P&startTime=0:00&endTime=23:59&interval=m'
)

incoming_data <-
for (i in seq_along(urls)){
  
  
}


downloadxmltotable6 <- function(url) {
  doc <- xmlTreeParse(url, useInternal=T)
  top <- xmlRoot(doc)
  df <- data.frame(matrix(unlist(xmlApply(top, xmlAttrs)), ncol=6, byrow=T),stringsAsFactors = F)
  names(df) <- c('count','date','direction', 'hour','minute','mode')
  return(df)
}

df_test <- downloadxmltotable6('http://webservices.commuterpage.com/counters.cfc?wsdl&counterid=1&method=GetCountInDateRange&startDate=1/1/2009&endDate=10/31/2014&direction=I&mode=B&startTime=0:00&endTime=23:59&interval=m')
  
  