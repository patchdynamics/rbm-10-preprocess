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
ice1995 = read.csv('../Data/1995/IDSW_IceHarbor_Tailwater.csv')
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
priesttail = read.csv('../Data/2015/PRXW_2015_PriestRapidsDownstream.csv')
priesttail.1995 = read.csv('../Data/1995/PRXW_PriestRapids_Tailwater.csv')

johndaytail = read.csv('../Data/2015/JHAW_2015_JohnDayTailwater.csv')
mcnarytail = read.csv('../Data/2015/MCPW_2015_McNaryTailwater.csv')
mcnarytail.1995 =read.csv('../Data/1995/MCPW_McNary_Tailwater.csv')
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


bon.daily = temp.timeseries.daily(bon)
lgtail.daily = temp.timeseries.daily(ts.lgtail)
lgstail.daily = temp.timeseries.daily(ts.lgstail)
lmontail.daily = temp.timeseries.daily(ts.lmontail)
ice.daily = temp.timeseries.daily(ts.ice)
mcnary.daily = temp.timeseries.daily(temp.timeseries(mcnarytail))
johnday.daily = temp.timeseries.daily(johndaytail)


mymonths <- c("Jun",
              "Jul","Aug","Sep"
              )
myticks = c(152, 182, 213, 244)

width = 480
height = 480
png(filename = "lg.obs.png", width = width, height = height)
par(mar = c(5, 5, 4, 2)) 
plot(lgtail.daily[150:250,]$daily.T*9/5 + 32, typ='l', ylim=c(55,75), 
     ylab='Temperature (F)', col='blue', lwd=2, xaxt='n',  xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Lower Granite'),
       lwd=2, col=c('blue'), cex=1.6
)
dev.off()

png(filename = "lgs.obs.png", width = width, height = height)
par(mar = c(5, 5, 4, 2)) 
plot(lgstail.daily[150:250,]$daily.T*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='green', lwd=2, xaxt='n', xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Little Goose'),
       lwd=2, col=c('green'), cex=1.6
)
dev.off()

png(filename = "lmon.obs.png", width = width, height = height)
par(mar = c(5, 5, 4, 2)) 
plot(lmontail.daily[150:250,]$daily.T*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='orange', lwd=2, xaxt='n', xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Lower Monumental'),
       lwd=2, col=c('orange2'), cex=1.6
)
dev.off()

png(filename = "ice.obs.png", width = width, height = height)
par(mar = c(5, 5, 4, 2)) 
plot(ice.daily[150:250,]$daily.T*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='red', lwd=2, xaxt='n', xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Ice Harbor'),
       lwd=2, col=c('red'), cex=1.6
)
dev.off()

png(filename = "all.obs.png", width = width, height = height)
par(mar = c(5, 5, 4, 2)) 
plot(lgtail.daily[150:250,]$daily.T*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='blue', lwd=2, xaxt='n', xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
lines(lgstail.daily[150:250,]$daily.T*9/5 + 32, lwd=2,  col='green')
lines(lmontail.daily[150:250,]$daily.T*9/5 + 32,lwd=2, col='orange')
lines(ice.daily[150:250,]$daily.T*9/5 + 32, lwd=2, col='red')
legend('bottomright', c('Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor'),
       lwd=2, col=c('blue', 'green', 'orange2', 'red'), cex=1.2
       )
dev.off()



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
