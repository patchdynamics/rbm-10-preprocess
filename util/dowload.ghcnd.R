library('plyr')
library('rnoaa')
options(noaakey = 'NNuQYQtUsMgGjfTagsGAwyNUApAeIYvu')
library('xts')

is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}




download.ghcnd = function(station.id, first.year, last.year){
  t.all = NULL
  for(year in first.year:last.year){
    sdate = paste0(year,'-01-01')
    edate = paste0(year,'-12-31')
    
    # TMAX/TMIN in tenths of degress C
    out <- ncdc(datasetid='GHCND', 
            stationid=station.id,
            datatypeid=c('TMIN', 'TMAX'),
            startdate = sdate, enddate = edate,
            limit=1000 )
    #ncdc_plot(out)
    #out
    #out$data$date
    #str(out$data)
    #out$data
    #as.data.frame(out$data)

    out$data$value.scaled = out$data$value / 10
    tmax = out$data[out$data$datatype == 'TMAX',]
    tmax = xts(tmax, order.by=as.Date(tmax$date))
    tmax = tmax$value.scaled
    names(tmax) = c('TMAX')
    tmax = na.approx(tmax)
    #plot(tmax$TMAX)
    tmin = out$data[out$data$datatype == 'TMIN',]
    tmin = xts(tmin, order.by=as.Date(tmin$date))
    tmin = tmin$value.scaled
    names(tmin) = c('TMIN')
    tmin = na.approx(tmin)
    #lines(tmin$TMIN, col='blue')


    start = as.POSIXct(paste0(sdate, ' 00:00:00'),tz='UTC')
    if(is.leapyear(as.numeric(substr(sdate, 1,4)))){
      len = 366
    } else {
      len = 365
    }
    full <- seq(start, by="1 day", length=len)
    ts.full = xts(order.by=full)
    tmax = merge(ts.full, tmax, na.rm = FALSE)
    tmax = na.approx(tmax)
    tmin = merge(ts.full, tmin, na.rm = FALSE)
    tmin = na.approx(tmin)
    temps = merge(tmax, tmin)
    temps$avg = (as.numeric(temps$TMAX)+as.numeric(temps$TMIN)) / 2

    #lines(temps$avg, col='orange')
    if(is.null(t.all)){
      t.all = temps
    } else {
      t.all = rbind(t.all, temps)
    }
    print(year)
    print(nrow(temps))
    print(nrow(ts.full))
    print(nrow(tmax))
    print(nrow(tmin))
    print(nrow(t.all))
  }
  t.all = na.approx(t.all)
  return(t.all)
}


USC0045907 = download.ghcnd('GHCND:USC00459074', 2005, 2015)
USC00457015 = download.ghcnd('GHCND:USC00457015', 2005, 2015)
plot(USC00457015$avg)


