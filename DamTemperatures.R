library(xts)
library(lubridate)

bon = read.csv('~/Downloads/wqmhourly_1469748047_697.csv')
bon = bon[!is.na(bon$Hour),]
hours = paste(as.numeric(bon$Hour) / 100, ':00', sep='')
dates = as.character(bon$Date)
o = apply(cbind(dates, hours), 1, paste, collapse=' ')
timestamps = as.POSIXct(o)
ts.bon = xts(bon, order.by=timestamps)

plot(ts.bon$Temperature..F.)
plot(ts.bon['2015-06-01/2015-09-01']$Temperature..F.)

plot(ts.bon['2015-06-01/2015-08-01']$Temperature..F.)

##
bon = read.csv('../Data/2015/BON_2015_BonnevilleForebay.csv')
dalles = read.csv('../Data/2015/TDDO_2015_TheDalles.csv')
dallesforebay = read.csv('../Data/2015/TD_2015_TheDallesForebay.csv')
ice = read.csv('../Data/2015/IDSW_2015_IceHarborTailwater.csv')
ice2016 = read.csv('../Data/2016/IDSW_2016_IceHarborTailwater.csv')
lgw = read.csv('../Data/2015/LWG_2015_LowerGraniteForebay.csv')
lgtail = read.csv('../Data/2015/LGNW_2015_LowerGraniteTailwater.csv')
lgtail2016 = read.csv('../Data/2016/LGNW_2016_LowerGraniteTailwater.csv')
dwo = read.csv('../Data/2015/DWQI_2015_DworshackTailwater.csv')
lmontail = read.csv('../Data/2015/LMNW_2015_LowerMonumentalTailwater.csv')
lmontail2016 = read.csv('../Data/2016/LMNW_2016_LowerMonumentalTailwater.csv')
lgstail = read.csv('../Data/2015/LGSW_2015_LittleGooseTailwater.csv')
lgstail2016 = read.csv('../Data/2016/LGSW_2016_LittleGooseTailwater.csv')
wells = read.csv('../Data/2015/WEL_2015_Wells.csv')
rockyreachtail = read.csv('../Data/2015/RRDW_2015_RockyReachTailwater.csv')

# load from URL
url = 'http://www.cbr.washington.edu/dart/cs/php/rpt/wqm_hourly.php?sc=1&outputFormat=csv&year=2015&proj=LMNW&startdate=01%2F01&days=365'
read.csv(url)


  data = data[!is.na(data$Hour),]
  #ata = data[1:8759,]
  hours = paste(sprintf("%02d", as.numeric(as.character(data$Hour))/100), ':00:00', sep='')
  dates = as.character(data$Date)
  o = apply(cbind(dates, hours), 1, paste, collapse=' ')
  timestamps = as.POSIXct(o, "Etc/GMT+8")
  head(timestamps)
  ts.data = xts(data, order.by=timestamps)

plot(ts.data$Temperature..F.)
plot(ts.data['2015-06-01/2015-09-01']$Temperature..F.)
plot(ts.data['2015-05-01/2015-10-01']$Temperature..F.)


plot(ts.data['2015-08-01/2015-08-10']$Temperature..F., typ='p')


temp.timeseries = function(data) {
  
  data = data[!is.na(data$Hour),]
  #ata = data[1:8759,]
  hours = paste(sprintf("%02d", as.numeric(as.character(data$Hour))/100), ':00:00', sep='')
  dates = as.character(data$Date)
  o = apply(cbind(dates, hours), 1, paste, collapse=' ')
  timestamps = as.POSIXct(o, "Etc/GMT+8")
  head(timestamps)
  ts.data = xts(data, order.by=timestamps)
  return(ts.data)
}

temp.timeseries.daily = function(data) {
  ts.data = temp.timeseries(data)
  ts.data$Day = .indexyday(ts.data)
  data.daily = ddply(as.data.frame(ts.data), c('Day'), summarize, daily.T = mean(Temperature..C.))
  return(data.daily)
}

ts.bon = temp.timeseries(bon)
ts.ice = temp.timeseries(ice)
ts.lgw = temp.timeseries(lgw)
ts.dwo = temp.timeseries(dwo)
ts.lgtail = temp.timeseries(lgtail)
ts.lmontail = temp.timeseries(lmontail)
ts.lgstail = temp.timeseries(lgstail)

plot(ts.ice['2015-05-01/2015-10-01']$Temperature..F., ylim=c(40,80), main='Temperature 2015')
lines(ts.lgw['2015-05-01/2015-10-01']$Temperature..F., col='blue')
lines(ts.dwo['2015-05-01/2015-10-01']$Temperature..F., col='green')
legend('topleft', c('Ice Harbor Tailwater', 'Lower Granite Forebay', 'Dworshak Tailwater'), col=c('black', 'blue', 'green'),
      lwd=1, cex=.7)

range = '2015-05-01/2015-10-01'
plot(ts.lgw[range]$Temperature..F.)
lines(ts.lgtail[range]$Temperature..F., col='blue')

plot(ts.ice['2015-05-01/2015-10-01']$Temperature..F., ylim=c(60,75))
lines(ts.lgw['2015-05-01/2015-10-01']$Temperature..F., col='blue')
legend('topleft', c('Ice Harbor Tailwater', 'Lower Granite Forebay'), col=c('black', 'blue'), lwd=1)

# summer
# range = .indexyday(ts.lgtail) >= 150 & .indexyday(ts.lgtail) <= 250

plot(ts.lgtail[range]$Temperature..F., ylim=c(55, 75), main='Temperature 2015')
lines(ts.lgstail[range]$Temperature..F., col='blue')
lines(ts.lmontail[range]$Temperature..F., col='green')
lines(ts.ice[range]$Temperature..F., col='orange')
legend('topleft', 
       c('Granite', 'Goose', 'Monumental', 'Ice'), 
       col=c('black', 'blue', 'green', 'orange'), lwd=1)


plot(ts.lgtail[range]$Temperature..C., ylim=c(0, 30), main='Temperature 2015')
lines(ts.lgstail[range]$Temperature..C., col='blue')
lines(ts.lmontail[range]$Temperature..C., col='green')
lines(ts.ice[range]$Temperature..C., col='orange')
legend('topleft', 
       c('Granite', 'Goose', 'Monumental', 'Ice'), 
       col=c('black', 'blue', 'green', 'orange'), lwd=1)




library(stringr)
url = 'http://www.cbr.washington.edu/dart/cs/php/rpt/wqm_hourly.php?sc=1&outputFormat=csv&year=YEAR&proj=PROJECT&startdate=01%2F01&days=365'
year = 2014
projects = c('LGNW', 'LGSW', 'LMNW', 'IDSW')
ts.all = NULL
for(p in 1:length(projects)){
  url1 = str_replace(url, 'YEAR', year)
  url1 = str_replace(url1, 'PROJECT', projects[p])
  data = read.csv(url1)
  ts = temp.timeseries(data)
  if(is.null(ts.all)){
    ts.all = ts$Temperature..F.
  } else {
    ts.all = merge(ts.all, ts$Temperature..F.)
  }
}
names(ts.all) = projects

range = paste0(year,'-05-01/', year,'-10-01')
plot(ts.all[range]$LGNW, ylim=c(55, 75), main=paste0('Temperature ',year))
lines(ts.all[range]$LGSW, col='blue')
lines(ts.all[range]$LMNW, col='green')
lines(ts.all[range]$IDSW, col='orange')
legend('topleft', 
       c('Granite', 'Goose', 'Monumental', 'Ice'), 
       col=c('black', 'blue', 'green', 'orange'), lwd=1)


ts.all.2014 = ts.all
#ts.all.2016 = ts.all

plot(ts.all.2014['2014-05-01/2014-10-01']$IDSW, ylim=c(55, 75), main='Temperature')
lines(
  xts(ts.ice['2015-05-01/2015-10-01']$Temperature..F., order.by = index(ts.all.2014['2014-05-01/2014-10-01']))
  , col='blue')
ts.temp = xts(ts.all.2016, order.by=index(ts.all.2016) - years(2))
lines(
  ts.temp$IDSW,
  col='red'
  )
legend('topleft', c('2014', '2015', '2016'), col=c('black', 'blue' ,'red'), lwd=1)
