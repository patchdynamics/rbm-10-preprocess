# Get flow from USGS
library('waterData')
library('plyr')
library('dataRetrieval')

gauges = c(
            #
            # Clearwater
            #
           '13340000', #CLEARWATER RIVER AT OROFINO ID
           # NF Clearwater at Ahsahka '13341000',   NO DISCHARGE, use DWO outflow
           #
           # Snake
           #
           '13334300', #SNAKE RIVER NEAR ANATONE, WA
           '13344500', #TUCANNON RIVER NEAR STARBUCK, WA  - NO TEMP
           '13351000', #PALOUSE RIVER AT HOOPER, WA       - NO TEMP
           #
           # Columbia
           #
           # Grand Coulee Downstream from DART
           '12447200', # Okanogan
           '12449950', # Methow
           '12452500', # Chelan SPILLWAY - NO TEMP
           '12462500', # Wnatchee - NO TEMP
           '12472600', # Crab Creek CRAB CREEK NEAR BEVERLY 
           '12510500', # Yakima - NO TEMP
           '14018500', # Walla Walla - NO TEMP
           '14048000', # John Day  - NO TEMP
           '14103000' # Deschutes 
           )

temperature.gauges = c(
          #### Tribs
          #
          # Clearwater
          #
          '13340000', #CLEARWATER RIVER AT OROFINO ID
          '13341000', #NF Clearwater at Ahsahka
          #
          # Snake
          #
          '13334300', #SNAKE RIVER NEAR ANATONE, WA
          # TUCANNON RIVER =  Washington DOE 35B060 = Continous?
          # PALOUSE RIVER = Washington DOE 34A070 = Continous?
          #
          # Columbia
          #
          # Grand Coulee Downstream from DART
          '12447200', # Okanogan
          # Methow = Washington DOE 48A070 = Continouse?
          # Chelan River = Washtington DOE 47A070, needs to be estimated
          # Wnatchee River = Washington DOE 45A070 = Continous?
          # Crab Creek = Washtington DOE 41A070 = Continous
          # Walla Walla  USGS 14018500  - needs to be estimated
          # John Day River -Oregon DEQ 404065 ??? - probably needs to be estimated
          '14103000' # Deschutes 
          )


# Washtington DOE
# Tucannon River 35B060 - Temperature
# Palouse River 34A070 - Temperature
# Crab Creek DOE 41A070 - Flow and Temperature
# 



sdate = "2015-01-01"
edate = "2015-12-31"

discharges = c()
for(i in 1:length(gauges)){
  data = importDVs(gauges[i],code="00060", sdate=sdate, edate=edate)
  #plotParam(data)
  discharges = cbind(discharges, data$val)
}
#write.csv(discharges, 'discharges.2015.csv')
discharges.usgs = as.data.frame(discharges)
names(discharges.usgs) = gauges

dwo.discharge =as.numeric(ts.dwo$Outflow..kcfs.)
plot(dwo.discharge, typ='l')
#dwo.discharge = na.interp(dwo.discharge)
dwo.discharge = na.locf(dwo.discharge)
plot(dwo.discharge, typ='l')
dwo.dates = as.character(ts.dwo$Date)
df = data.frame(Date = dwo.dates, Discharge = dwo.discharge)
dwo.daily.discharges <- ddply(df, c("Date"), summarise,
               mean = mean(Discharge)
)
plot(dwo.daily.discharges$mean, typ='l')

prepare.dam.discharge = function(ts){
  discharges = as.numeric(ts$Outflow..kcfs.)
  plot(discharges, typ='l')
  #dwo.discharge = na.interp(dwo.discharge)
  discharges = na.locf(discharges)
  plot(discharges, typ='l')
  dates = as.character(ts$Date)
  df = data.frame(Date = dates, Discharge = discharges)
  daily.discharges <- ddply(df, c("Date"), summarise,
                                mean = mean(Discharge)
  )
  plot(daily.discharges$mean, typ='l')
  return(daily.discharges)
}

url = 'http://www.cbr.washington.edu/dart/cs/php/rpt/wqm_hourly.php?sc=1&outputFormat=csv&year=YEAR&proj=PROJECT&startdate=01%2F01&days=365'
year = 2015
projects = c('DWQI', 'GCGW')
discharges.all = 1:365
for(p in 1:length(projects)){
  url1 = str_replace(url, 'YEAR', year)
  url1 = str_replace(url1, 'PROJECT', projects[p])
  data = read.csv(url1)
  ts = temp.timeseries(data)
  daily.discharges = prepare.dam.discharge(ts)
  discharges.all = rbind(discharges.all, daily.discharges[,2]*1000)
}
discharges.all = t(discharges.all)
discharges.dams.df = as.data.frame(discharges.all)
names(discharges.dams.df) = c('Julian', projects)




# we don't need ice
#ice.discharge =as.numeric(ts.ice$Outflow..kcfs.)
#plot(ice.discharge, typ='l')
#ice.discharge = na.locf(ice.discharge)
#plot(ice.discharge, typ='l')
#ice.dates = as.character(ts.ice$Date)
#df = data.frame(Date = ice.dates, Discharge = ice.discharge)
#ice.daily.discharges <- ddply(df, c("Date"), summarise,
#                              mean = mean(Discharge)
#)
#plot(ice.daily.discharges$mean, typ='l')








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
  
  coeffs.df.rising = coeffs.df[limb == 'rising',]
  coeffs.df.falling = coeffs.df[limb == 'falling',]
  
  air.temperature.rising = data$AirTemperature[data$Julian < coeffs.df.falling$start]
  air.temperature.falling = data$AirTemperature[data$Julian >= coeffs.df.falling$start]
  
  T.stream.rising = approximate.stream.temperature.calc(coeffs.df.rising, air.temperature.rising)
  T.stream.falling = approximate.stream.temperature.calc(coeffs.df.falling, air.temperature.falling)
  T.stream = c(T.stream.rising, T.stream.falling)
  return(T.stream)
}

lewiston = read.table('RBM-10/LEWISTON.HOT', header=FALSE, skip=4)
names(lewiston)=c('Julian', 'Solar', 'Atomospheric', 'AirTemperature', 'Wind', 'Bowen', 'VaporPressure', 'PhotoPeriod')
yakima = read.table('RBM-10/YAKIMA.HOT', header=FALSE, skip=4)
names(yakima)=c('Julian', 'Solar', 'Atomospheric', 'AirTemperature', 'Wind', 'Bowen', 'VaporPressure', 'PhotoPeriod')
wenatchee = read.table('RBM-10/WNATCHEE.HOT', header=FALSE, skip=4)
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

write.csv(df.approx.T, 'ApproximateWaterTemperatures.2015.csv')

#
# 2. Get temps from USGS Stations
# 

temperature.gauges = temperature.gauges[temperature.gauges != '13341000'] # North Fork not in the API

temperatures.usgs = c()
for(i in 1:length(temperature.gauges)){
  data = importDVs(temperature.gauges[i],code="00010", sdate=sdate, edate=edate)
  plotParam(data)
  temperatures.usgs = cbind(temperatures.usgs, data$val)
}
temperatures.usgs = data.frame(temperatures.usgs)
names(temperatures.usgs) = temperature.gauges


#
# 2b Load data from North Fork of Salmon manually from csv
#
uv <- readNWISuv(siteNumbers = '13341000',
                     parameterCd = "00010",
                     startDate = sdate,
                     endDate = edate)
ts.uv = xts(uv$X_00010_00011, order.by=uv$dateTime)
names(ts.uv) = c('WaterTemperature')
start = as.POSIXct(paste0(sdate, ' 00:15:00'),tz='UTC')
full <- seq(start, by="15 min", length=364*24*4-1)
ts.full = xts(order.by=full)
ts.uv = merge(ts.full, ts.uv)
ts.uv = na.approx(ts.uv$WaterTemperature)
ts.uv$Julian = .indexyday(ts.uv)
df.uv = as.data.frame(ts.uv)
daily.water.temperature = ddply(df.uv, c('Julian'), summarize,
      WaterTemperature=mean(WaterTemperature)
)
temperatures.usgs$"13341000" = daily.water.temperature$WaterTemperature

write.csv(temperatures.usgs, 'USGSWaterTemperatures.2015.csv')


# NOTE: This function not written yet.
prepare.dam.temperature = function(ts){
  temperatures = as.numeric(ts$Temperature..C.)
  plot(temperatures, typ='l')
  temperatures = na.locf(temperatures)
  plot(temperatures, typ='l')
  dates = as.character(ts$Date)
  df = data.frame(Date = dates, Temperature = temperatures)
  daily.temperatures <- ddply(df, c("Date"), summarise,
                            mean = mean(Temperature)
  )
  plot(daily.temperatures$mean, typ='l')
  return(daily.temperatures)
}

url = 'http://www.cbr.washington.edu/dart/cs/php/rpt/wqm_hourly.php?sc=1&outputFormat=csv&year=YEAR&proj=PROJECT&startdate=01%2F01&days=365'
year = 2015
projects = c('GCGW')
temperatures.all = 1:365
for(p in 1:length(projects)){
  url1 = str_replace(url, 'YEAR', year)
  url1 = str_replace(url1, 'PROJECT', projects[p])
  data = read.csv(url1)
  ts = temp.timeseries(data)
  daily.temperatures = prepare.dam.temperature(ts)
  temperatures.all = rbind(temperatures.all, daily.temperatures[,2])
}
dam.temperatures.all = temperatures.all
dam.temperatures.df = as.data.frame(dam.temperatures.all)
names(dam.temperatures.df) = projects


#
#  Organize all this data somehow and write out the advection file
#

# We'll build out a data frame in order
advection.df = data.frame( Clearwater = discharges.usgs$"13340000",
                           Clearwater.T = temperatures.usgs$"13340000",
                           Snake = discharges.usgs$"13334300",
                           Snake.T = temperatures.usgs$"13334300",
                           Columbia = discharges.dams.df$GCGW,
                           Columbia.T = dam.temperatures.df$GCGW,
                           NFClearwater = discharges.dams.df$DWQI,
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
write.csv(advection.df, 'Advection.2015.csv')


#
# And now write the Advection File!
#
filename = paste0(sdate, '.adv')
write(paste0('****************** INPUT FILE - ADVECTION - FOR RBM10 ****************'),
      file = filename, append=FALSE)
write(paste0('******************          ADVECTED SOURCE DATA      ****************'),
      file = filename, append=TRUE)
write(str_replace_all(paste(sdate, edate), '-', ''), file=filename, append=TRUE)
year =  year(sdate)
for(d in 1:365){
  advection = advection.df[d,]
  write.table(cbind(year, d, advection[,c('Clearwater', 'Clearwater.T', 'Snake', 'Snake.T', 'Columbia', 'Columbia.T')]),
              file = filename, append = TRUE, sep='  ', col.name=FALSE, quote=FALSE, row.names=FALSE)
  write(paste0('    ', 1, '    ', advection$NFClearwater, advection$NFClearwater.T), file = filename, append = TRUE)
  write(paste0('    ', 3, '    ', advection$NFClearwater, advection$NFClearwater.T), file = filename, append = TRUE)
  write(paste0('    ', 4, '    ', advection$Potlatch, advection$Potlatch.T), file = filename, append = TRUE)
  write(paste0('    ', 5, '    ', advection$Tucannon, advection$Tucannon.T), file = filename, append = TRUE)
  write(paste0('    ', 6, '    ', advection$Palouse, advection$Palouse.T), file = filename, append = TRUE)
  write(paste0('    ', 7, '    ', advection$Okanogan, advection$Okanogan.T), file = filename, append = TRUE)
  write(paste0('    ', 8, '    ', advection$Methow, advection$Methow.T), file = filename, append = TRUE)
  write(paste0('    ', 9, '    ', advection$Chelan, advection$Chelan.T), file = filename, append = TRUE)
  write(paste0('    ', 10, '    ', advection$Crab, advection$Crab.T), file = filename, append = TRUE)
  write(paste0('    ', 11, '    ', advection$Yakima, advection$Yakima.T), file = filename, append = TRUE)
  write(paste0('    ', 13, '    ', advection$WallaWalla, advection$WallaWalla.T), file = filename, append = TRUE)
  write(paste0('    ', 13, '    ', advection$JohnDay, advection$JohnDay.T), file = filename, append = TRUE)
  write(paste0('    ', 13, '    ', advection$Deschutes, advection$Deschutes.T), file = filename, append = TRUE)  
}

