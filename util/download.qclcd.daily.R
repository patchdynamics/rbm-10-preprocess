library(stringr)

downloadload.qclcd.daily = function(start.year, end.year, wbans=NULL) {
  url.base.new = 'http://www.ncdc.noaa.gov/orders/qclcd/QCLCDYYYYMM.zip'
  url.base.old = 'http://www.ncdc.noaa.gov/orders/qclcd/YYYYMM.tar.gz'
  years = start.year:end.year
  data.period = NULL
  for(year in years){
    data.year = NULL
    for(month in 1:12){
      if(year < 2007 || (year == 2007 && month < 5) ){
        url.base = url.base.old
        type = 'old'
      } else {
        url.base = url.base.new
        type = 'new'
      }
      temp <- tempfile()
      date.string = paste0(year, sprintf('%.2i', month))
      url = str_replace(url.base,'YYYYMM', date.string)
      download.file(url,temp)
      filename = paste0(date.string, 'daily.txt')
      if(type == 'old'){
        untar(temp, file=filename)
        data.month = read.csv(filename)
        data.month = data.frame(
          'WBAN' = data.month$Wban.Number,
          'YearMonthDay' = data.month$YearMonthDay,
          'Tmax' = data.month$Max.Temp,
          'Tmin' = data.month$Min.Temp,
          'Tavg' = data.month$Avg.Temp,
          'DewPoint' = data.month$Avg.Dew.Pt,
          'WetBulb' = data.month$Avg.Wet.Bulb,
          'AvgSpeed' = data.month$Wind.Avg.Speed)
      } else {
        data.month = read.csv(unz(temp, filename))
        data.month = data.frame(
          'WBAN' = data.month$WBAN,
          'YearMonthDay' = data.month$YearMonthDay,
          'Tmax' = data.month$Tmax,
          'Tmin' = data.month$Tmin,
          'Tavg' = data.month$Tavg,
          'DewPoint' = data.month$DewPoint,
          'WetBulb' = data.month$WetBulb,
          'AvgSpeed' = data.month$AvgSpeed)
      }
      unlink(temp)
      data.year = rbind(data.year, data.month)
    }
    data.period = rbind(data.period, data.year)    
  }
  # filter by WBAN if provided
  if(!is.null(wbans)){
    data.period = data.period[data.period$WBAN %in% wbans,]
  }
  return(data.period)
}

#data.2014 = downloadload.qclcd.daily('2014', '2014', wbans=c(24149, 24243))
#data.2004 = downloadload.qclcd.daily('2004', '2004', wbans=c(24149, 24243))
#data.2007 = downloadload.qclcd.daily('2007', '2007', wbans=c(24149, 24243))
qclcd.2005.2015 = downloadload.qclcd.daily('2005', '2015', wbans=c(24149, 24243))

