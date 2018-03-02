# Get flow from USGS
library('waterData')
library('plyr')
library('dataRetrieval')
library('lubridate')
library('forecast')
library('stringr')
library('xts')


# DWO vs DWR

gauges = c(
            #
            # Clearwater
            #
           '13340000', #CLEARWATER RIVER AT OROFINO ID                       1930-10-01 	2016-08-29
           # NF Clearwater at Ahsahka '13341000',   NO DISCHARGE, use DWO outflow
           #
           # Snake
           #
           '13334300', #SNAKE RIVER NEAR ANATONE, WA                         1958-07-22   2016-08-29
           '13344500', #TUCANNON RIVER NEAR STARBUCK, WA  - NO TEMP          1914-10-01   2016-08-29
           '13351000', #PALOUSE RIVER AT HOOPER, WA       - NO TEMP          1897-10-01   2016-08-29
           #
           # Columbia
           #
           # Grand Coulee Downstream from DART
           '12447200', # Okanogan                                            1965-12-18   2016-08-29
           '12449950', # Methow                                              1959-04-01   2016-08-29
           '12452500', # Chelan SPILLWAY - NO TEMP                           1903-11-01   2015-09-30   * Rocky Reach minus Wells ?
           '12462500', # Wenatchee - NO TEMP                                 1959-02-12   2016-05-31   
           '12472600', # Crab Creek CRAB CREEK NEAR BEVERLY                  1959-02-12   2016-05-31
           '12510500', # Yakima - NO TEMP                                    1905-10-01   2016-08-29
           '14018500', # Walla Walla - NO TEMP                               1951-10-01   2016-08-29
           '14048000', # John Day  - NO TEMP                                 1904-12-01   2016-08-29
           '14103000' # Deschutes                                            1897-10-01   2016-08-29
           )

temperature.gauges = c(
          #### Tribs
          #
          # Clearwater
          #
          '13340000', #CLEARWATER RIVER AT OROFINO ID                         1993-10-01   2016-08-29       
          '13341000', #NF Clearwater at Ahsahka                               2009-10-01   2016-08-30  * Use DWR
          #
          # Snake
          #
          '13334300', #SNAKE RIVER NEAR ANATONE, WA                           1959-10-01   2016-08-29
          # TUCANNON RIVER =  Washington DOE 35B060 = Continous?
          # PALOUSE RIVER = Washington DOE 34A070 = Continous?
          #
          # Columbia
          #
          # Grand Coulee Downstream from DART
          '12447200', # Okanogan                                              1969-11-12   2016-08-29
          # Methow = Washington DOE 48A070 = Continouse?
          # Chelan River = Washtington DOE 47A070, needs to be estimated
          # Wenatchee River = Washington DOE 45A070 = Continous?              2016-08-12 	2016-08-30 USGS
          # Crab Creek = Washtington DOE 41A070 = Continous
          # Yakima                                                            2004-04-20   2008-09-30 USGS
          # Walla Walla  USGS 14018500  - needs to be estimated               2002-08-15   2005-09-30 USGS
          # John Day River -Oregon DEQ 404065 ??? - probably needs to be estimated  1967-10-01   1981-10-14 USGS
          '14103000' # Deschutes                                              1954-10-26   2016-08-29
          )


# Washtington DOE
# Tucannon River 35B060 - Temperature
# Palouse River 34A070 - Temperature
# Crab Creek DOE 41A070 - Flow and Temperature
# 



# Get USGS Discharges

syear = '2005'
eyear = '2015'
sdate = paste0(syear, "-01-01")
edate = paste0(eyear, "-12-31")

discharges = c()
for(i in 1:length(gauges)){
  data = importDVs(gauges[i],code="00060", sdate=sdate, edate=edate)
  # should check registration of dates, but appears to be OK for current domain
  print(nrow(data))
  #plotParam(data)
  val = na.interp(data$val)
  length(val) = 4017   # THIS IS A HACK to make Chelan the right size and avoid looping
  val = na.locf(val)
  val = na.locf(val, fromLast = TRUE)
  discharges = cbind(discharges, val)
}
discharges.usgs = as.data.frame(discharges)
names(discharges.usgs) = gauges


#write.csv(discharges, 'discharges.2015.csv')


# Get DART Discharges

projects = c('DWR', 'GCGW')
discharges.dams = NULL
for(project in projects){
  data = download.dart.daily(project, syear, eyear)
  data$Outflow..kcfs = as.numeric(data$Outflow..kcfs) *1000
  # Fill/Interp Here
  if(is.null(discharges.dams)){
    discharges.dams = data$Outflow..kcfs
  } else {
    discharges.dams = cbind(discharges.dams, data$Outflow..kcfs)
  }
}
discharges.dams = as.data.frame(discharges.dams)
names(discharges.dams) = projects



############## Temperatures

# 1. Estimate tribs using Table 3-1 where necessary
# 2. Get temps from USGS stations
# 3. Synthesize continuous monitoring data from Washington DOE
# 4. Get temperature from Dworshak Tailwater and Grand Coulee Downstream

#
# 1.
#
#site =c('Tucannon', 'Tucannon')
#limb =c('rising',    'falling')
#start=c(1,           32*7)
#alpha=c(22,          22)
#beta= c(12.2640,     11.3406)
#gamma=c(.1698,       .1570)
#mu=   c(.4717,       .4362)
#coeffs.df = data.frame(site=site, limb=limb, start=start, alpha=alpha, beta=beta, gamma=gamma, mu=mu)

# Read coefficients from a file instead
coeffs.df = read.csv('WaterTemperatureApproximationCoefficients.csv')

approximate.stream.temperature.calc = function(coeffs.df, air.temperature.limb){
  T.stream = coeffs.df$mu + (coeffs.df$alpha - coeffs.df$mu) / (1 + exp( coeffs.df$gamma * ( coeffs.df$beta - air.temperature.limb)))
  return(T.stream)
}

approximate.stream.temperature = function(coeffs.df, data){
  
  coeffs.df.rising = coeffs.df[coeffs.df$limb == 'rising',]
  coeffs.df.falling = coeffs.df[coeffs.df$limb == 'falling',]
  
  air.temperature.rising = data$AirTemperature[data$Julian < coeffs.df.falling$start]
  air.temperature.falling = data$AirTemperature[data$Julian >= coeffs.df.falling$start]
  
  T.stream.rising = approximate.stream.temperature.calc(coeffs.df.rising, air.temperature.rising)
  T.stream.falling = approximate.stream.temperature.calc(coeffs.df.falling, air.temperature.falling)
  T.stream = c(T.stream.rising, T.stream.falling)
  return(T.stream)
}

lewiston = read.table('2005.2015/LEWISTON.HOT', header=FALSE, skip=4)
names(lewiston)=c('Julian', 'Solar', 'Atomospheric', 'AirTemperature', 'Wind', 'Bowen', 'VaporPressure', 'PhotoPeriod')
yakima = read.table('2005.2015/YAKIMA.HOT', header=FALSE, skip=4)
names(yakima)=c('Julian', 'Solar', 'Atomospheric', 'AirTemperature', 'Wind', 'Bowen', 'VaporPressure', 'PhotoPeriod')
wenatchee = read.table('2005.2015/WNATCHEE.HOT', header=FALSE, skip=4)
names(wenatchee)=c('Julian', 'Solar', 'Atomospheric', 'AirTemperature', 'Wind', 'Bowen', 'VaporPressure', 'PhotoPeriod')

stations.data = list(Lewiston = lewiston, Yakima = yakima, Wenatchee = wenatchee)

streams = c('Methow', 'Tucannon', 'John Day', 'Palouse', 'Walla Walla', 'Chelan', 'Wenatchee', 'Crab Creek', 'Yakima')
stations = c('Wenatchee', 'Lewiston', 'Lewiston', 'Yakima', 'Yakima', 'Wenatchee', 'Wenatchee', 'Wenatchee', 'Yakima')

T.streams.approx = c()
for(i in 1:length(streams)){
  T.stream.approx = approximate.stream.temperature(coeffs.df[coeffs.df$site==streams[i],],
                                                   stations.data[[stations[i]]])
  T.streams.approx = cbind(T.streams.approx, T.stream.approx)
}
df.approx.T = data.frame(T.streams.approx)
names(df.approx.T) = streams

write.csv(df.approx.T, 'ApproximateWaterTemperatures.2005.2015.csv')


#
# 2. Get temps from USGS Stations
# 

temperature.gauges = temperature.gauges[temperature.gauges != '13341000'] # North Fork not in the API

temperatures.usgs = c()
for(i in 1:length(temperature.gauges)){
  data = importDVs(temperature.gauges[i],code="00010", sdate=sdate, edate=edate)
  plotParam(data)
  ts = xts(data$val, order.by=data$dates)
  #
  #  GUESS WHAT??? Need to fill missing values..
  #
  #   full <- seq(start, by="15 min", length=(365+365+366+365+365+365)*24*4) 
  #  seq.POSIXt 
  ts.full = xts( order.by = seq(ISOdate(2005,1,1, 0, 0, 0), ISOdate(2015,12,31, 0, 0, 0), "1 day") )
  ts = merge(ts.full, ts)
  ts = na.locf(ts)
  ts = na.locf(ts, fromLast = TRUE)
  names(ts) = 'Temperature'
  ts = na.approx(ts)
  temperatures.usgs = cbind(temperatures.usgs, as.numeric(ts$Temperature))
}
temperatures.usgs = data.frame(temperatures.usgs)
names(temperatures.usgs) = temperature.gauges


#
# 2b Load data from North Fork of Salmon from instantaneous
#
uv <- readNWISuv(siteNumbers = '13341000',
                     parameterCd = "00010",
                     startDate = sdate,
                     endDate = edate)
ts.uv.1 = xts(uv$X_00010_00011, order.by=uv$dateTime)
ts.uv.1 = ts.uv.1['2010-01-01/2015-12-31']
names(ts.uv.1) = c('WaterTemperature')

start = as.POSIXct(paste0(sdate, ' 00:15:00'),tz='UTC')
ts.full = xts( order.by = seq(ISOdate(2010,1,1, 0, 0, 0), ISOdate(2015,12,31, 0, 0, 0), "15 mins") )
ts.uv = merge(ts.full, ts.uv.1)
ts.uv = na.locf(ts.uv)
ts.uv = na.locf(ts.uv, fromLast=TRUE)
≈ = na.approx(ts.uv$WaterTemperature)
ts.uv$Julian = .indexday(ts.uv)
df.uv = as.data.frame(ts.uv)
daily.water.temperature = ddply(df.uv, c('Julian'), summarize,
      WaterTemperature=mean(WaterTemperature)
)

# USGS sensor doesn't go back to 2005
# So we have to use DWR for 2005, 2006, 2008, 2009
data = download.dart.daily('DWR', 2005, 2009)
data.temperature.2005.2009 = as.numeric(data$Temperature..C.)
data.temperature.all = append(data.temperature.2005.2009, daily.water.temperature$WaterTemperature)

temperatures.usgs$"13341000" = data.temperature.all

write.csv(temperatures.usgs, 'USGSWaterTemperatures.2015.csv')


# Get DART Temperatures
projects = c('GCGW')
temperatures.dams = NULL
for(project in projects) {
  data = download.dart.daily(project, syear, eyear)
  
  if(is.null(discharges.dams)){
    temperatures.dams = as.numeric(data$Temperature..C.)
  } else {
    temperatures.dams = cbind(temperatures.dams, as.numeric(data$Temperature..C.))
  }
}
temperatures.dams = as.data.frame(temperatures.dams)
names(temperatures.dams) = projects



#
#  Organize all this data somehow and write out the advection file
#

# We'll build out a data frame in order
advection.df = data.frame( 
                          Clearwater = discharges.usgs$"13340000",
                           Clearwater.T = temperatures.usgs$"13340000",
                           Snake = discharges.usgs$"13334300",
                           Snake.T = temperatures.usgs$"13334300",
                           Columbia = discharges.dams$GCGW,
                           Columbia.T = temperatures.dams$GCGW,
                           NFClearwater = discharges.dams$DWR,
                           NFClearwater.T = temperatures.usgs$"13341000",
                           Potlatch = 62.2,
                           Potlatch.T = 33,
                           Tucannon = discharges.usgs$"13344500",
                           Tucannon.T = df.approx.T$Tucannon,
                           Palouse = discharges.usgs$"13351000",
                           Palouse.T = df.approx.T$Palouse,
                           Okanogan = discharges.usgs$"12447200",
                           Okanogan.T = temperatures.usgs$"12447200",
                           Methow = discharges.usgs$"12449950",
                           Methow.T = df.approx.T$Methow,
                           Chelan = discharges.usgs$"12452500",
                           Chelan.T = df.approx.T$Chelan,
                           Wenatchee = discharges.usgs$"12462500",
                           Wenatchee.T = df.approx.T$Wenatchee,
                           Crab = discharges.usgs$"12472600",
                           Crab.T = df.approx.T$"Crab Creek",
                           Yakima = discharges.usgs$"12510500",
                           Yakima.T = df.approx.T$Yakima,
                           WallaWalla = discharges.usgs$"14018500",
                           WallaWalla.T = df.approx.T$"Walla Walla",
                           JohnDay = discharges.usgs$"14048000",
                           JohnDay.T = df.approx.T$"John Day",
                           Deschutes = discharges.usgs$"14103000",
                           Deschutes.T = temperatures.usgs$"14103000"
)

advection.df =round(advection.df, digits=2)
write.csv(advection.df, 'Advection.2005.2015.csv')

#advection.df = read.csv('Advection.2015.csv')
#advection.df$WallaWalla.T[advection.df$WallaWalla.T > 20] = 20

#
# And now write the Advection File!
#
filename = paste0(sdate, '.adv')
filename = 'Advection.adv'
write(paste0('****************** INPUT FILE - ADVECTION - FOR RBM10 ****************'),
      file = filename, append=FALSE)
write(paste0('******************          ADVECTED SOURCE DATA      ****************'),
      file = filename, append=TRUE)
formatted.dates = sprintf("%10s%10s", str_replace_all(sdate, '-', ''), str_replace_all(edate, '-', '') )
write(formatted.dates, file=filename, append=TRUE)
day.index = 1
for(year in (year(sdate)):(year(edate))){
  days.in.year = 365
  if(is.leapyear(year)){
    days.in.year = days.in.year + 1
  }
  for(d in 1:days.in.year){
    advection = advection.df[day.index,]
    boundaries.numbers = as.numeric(cbind(year, d, advection[,c("Clearwater", "Clearwater.T", "Snake", "Snake.T", "Columbia", "Columbia.T")]))
    boundaries = sprintf('%5i%5i%10.0f%10.1f%10.0f%10.1f%10.0f%10.1f', 
                         year, d, advection$Clearwater, advection$Clearwater.T, advection$Snake, advection$Snake.T, advection$Columbia, advection$Columbia.T )
    write(boundaries, file=filename, append=TRUE)
    write(sprintf('%5i%10.1f%5.1f', 1, advection$NFClearwater, advection$NFClearwater.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 3, advection$Potlatch, advection$Potlatch.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 4, advection$Tucannon, advection$Tucannon.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 5, advection$Palouse, advection$Palouse.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 6, advection$Okanogan, advection$Okanogan.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 7, advection$Methow, advection$Methow.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 8, advection$Chelan, advection$Chelan.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 9, advection$Wenatchee, advection$Wenatchee.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 10, advection$Crab, advection$Crab.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 11, advection$Yakima, advection$Yakima.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 13, advection$WallaWalla, advection$WallaWalla.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 14, advection$JohnDay, advection$JohnDay.T), file = filename, append = TRUE)
    write(sprintf('%5i%10.1f%5.1f', 15, advection$Deschutes, advection$Deschutes.T), file = filename, append = TRUE)  
    day.index = day.index + 1
  }
  }

#
# Write Advection Files for Scenario2
#
Columbia.T.Max = 13
  filename = '2015.Scenario2b.adv'
write(paste0('****************** INPUT FILE - ADVECTION - FOR RBM10 ****************'),
      file = filename, append=FALSE)
write(paste0('******************          ADVECTED SOURCE DATA      ****************'),
      file = filename, append=TRUE)
formatted.dates = sprintf("%10s%10s", str_replace_all(sdate, '-', ''), str_replace_all(edate, '-', '') )
write(formatted.dates, file=filename, append=TRUE)
year =  year(sdate)
for(d in 1:365){
  advection = advection.df[d,]
  Columbia.T = advection$Columbia.T
  if(Columbia.T > Columbia.T.Max) {
    Columbia.T = Columbia.T.Max
  }
  boundaries.numbers = as.numeric(cbind(year, d, advection[,c('Clearwater', 'Clearwater.T', 'Snake', 'Snake.T', 'Columbia', 'Columbia.T')]))
  boundaries = sprintf('%5i%5i%10.0f%10.1f%10.0f%10.1f%10.0f%10.1f', 
                       year, d, advection$Clearwater, advection$Clearwater.T, advection$Snake, advection$Snake.T, advection$Columbia, Columbia.T )
  write(boundaries, file=filename, append=TRUE)
  write(sprintf('%5i%10.1f%5.1f', 1, advection$NFClearwater, advection$NFClearwater.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 3, advection$Potlatch, advection$Potlatch.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 4, advection$Tucannon, advection$Tucannon.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 5, advection$Palouse, advection$Palouse.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 6, advection$Okanogan, advection$Okanogan.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 7, advection$Methow, advection$Methow.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 8, advection$Chelan, advection$Chelan.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 9, advection$Wenatchee, advection$Wenatchee.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 10, advection$Crab, advection$Crab.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 11, advection$Yakima, advection$Yakima.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 13, advection$WallaWalla, advection$WallaWalla.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 14, advection$JohnDay, advection$JohnDay.T), file = filename, append = TRUE)
  write(sprintf('%5i%10.1f%5.1f', 15, advection$Deschutes, advection$Deschutes.T), file = filename, append = TRUE)  
}

