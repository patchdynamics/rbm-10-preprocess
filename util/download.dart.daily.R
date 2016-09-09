

download.dart.daily = function(project, first.year, last.year){
  url = 'http://www.cbr.washington.edu/dart/cs/php/rpt/river_daily.php?sc=1&outputFormat=csv&year=YYYY&proj=PROJECT&span=no&startdate=1%2F1&enddate=12%2F31'
  
  ts.all = NULL
  for(year in first.year:last.year){
    print(year)
    url1 = str_replace(url, 'YYYY', year)
    url1 = str_replace(url1, 'PROJECT', project)
    print(url1)
    data = read.csv(url1, stringsAsFactors=FALSE)
    data = data[data$Date != "",]
    timestamps = as.POSIXct(data$Date, "Etc/GMT+8")
    ts.data = xts(data, order.by=timestamps)
    if(is.null(ts.all)){
      print('ye')
      ts.all = ts.data 
    } else {
      ts.all = rbind(ts.all, ts.data)
    }
  }
  ts.all$Temperature..C. = na.approx(ts.all$Temperature..C., na.rm=FALSE)
  ts.all$Outflow..kcfs. = na.approx(ts.all$Outflow..kcfs., na.rm=FALSE)
  return(ts.all)
}

#DWR = download.dart.daily('DWR', 2005, 2005)
#plot(DWO)
