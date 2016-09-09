library(forecast)
library(raster)

#qclcd = read.csv('../Data/QCLCD/daily/2015.txt')
qclcd = qclcd.2005.2015  # util/downloadload.qclcd.daily.R
names(qclcd)
lewiston = qclcd[qclcd$WBAN==24149,]
head(lewiston)
yakima = qclcd[qclcd$WBAN==24243,]
head(yakima)
spokane = qclcd[qclcd$WBAN==24157,]
head(spokane)

first.year = 2005
last.year = 2015

# extract net solar radiation from rasters

url.dswrf = 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/dswrf.YYYY.nc'
url.uswrf = 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/uswrf.sfc.YYYY.nc'
url.clouds = 'ftp://ftp.cdc.noaa.gov/Datasets/NARR/Dailies/monolevel/tcdc.YYYY.nc'

crs.narr = '+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50 +lat_2=50 +ellps=WGS84'
station.locations = shapefile('../GIS/WeatherStationsUsed.shp')
station.locations = spTransform(station.locations, crs.narr)
crs(station.locations)

values = NULL
for(year in first.year:last.year){
  download.file(str_replace(url.dswrf, 'YYYY', year), 'dswrf.nc')
  download.file(str_replace(url.uswrf, 'YYYY', year), 'uswrf.nc')
  dswrf = brick('dswrf.nc')
  uswrf.sfc = brick('uswrf.nc')
  netswr = dswrf - uswrf.sfc
  extracted.values = raster::extract(netswr, station.locations)  
  if(is.null(values)){
    values = extracted.values
  } else {
    values = cbind(values, extracted.values)
  }
}
# NARR: W/m^2  (per day)
# RBM-10 uses kcal/meter^2/second
# 1 Watt equals .86 kcal/hr
netswr.values = values * .86 / 3600

save(netswr.values, file='netswr.2005.2015.Rdata')

clouds = NULL
for(year in first.year:last.year){
  download.file(str_replace(url.clouds, 'YYYY', year), 'clouds.nc')
  clouds.narr = brick('clouds.nc')
  extracted.values = raster::extract(clouds.narr, station.locations)  
  if(is.null(clouds)){
    clouds = extracted.values
  } else {
    clouds = cbind(clouds, extracted.values)
  }
}
save(clouds, file='clouds.2005.2015.Rdata')


#uswrf.sfc = brick('../GIS/NARR/uswrf.sfc.2015.nc')
#dswrf = brick('../GIS/NARR/dswrf.2015.nc')
#netswr = dswrf - uswrf.sfc
#plot(netswr[[5]])


#plot(netswr[[100]])
#plot(station.locations, add=T)
#animate(netswr)

#values = raster::extract(netswr, station.locations)  

# NARR: W/m^2  (per day)
# RBM-10 uses kcal/meter^2/second
# 1 Watt equals .86 kcal/hr
#netswr.values = values * .86 / 3600
#station.locations$STAID
#plot(netswr.values[1,])

#clouds = brick('../GIS/NARR/tcdc.2015.nc')
#plot(clouds[[1]])
#animate(clouds)
#clouds.values = raster::extract(clouds, station.locations)  


#
# 1: process first order data into HOT file format
#


write.hot.file = function(df, filename, station.name, station.elevation, station.latitude, station.longitude, start, end) {
  write(paste0('****************** INPUT FILE - ',filename,' - FOR RBM10 ****************'),
        file = filename, append=FALSE)
  write(paste0('******************           WEATHER STATION DATA        ****************'),
        file = filename, append=TRUE)
  write(station.name, file = filename, append=TRUE)
  station.info = c(1,station.elevation, station.latitude, station.longitude, start, end)
  formatted.station.info = sprintf("%10s", station.info)
  write.table(t(formatted.station.info), file = filename, append = TRUE, sep='', col.name=FALSE, quote=FALSE, row.names=FALSE)
  formatted.df = sprintf("%5i%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f%10.5f", df$Julian, df$Solar, df$Atmospheric, df$AirTemperature, df$Wind, df$Bowen, df$VaporPressure, df$PhotoPeriod)
  write(formatted.df, file=filename, append=TRUE)
}


primary.stations = list()

for(station.index in 1:2) {
  
  # 1: lewiston 
  if(station.index == 1) {
    station.elevation = 1436 # ft  file says 740
    station.data = lewiston
    
    filename = 'LEWISTON.HOT'
    station.name = 'LEWISTON WBAN24149'
    station.latitude = 46
    station.longitude = 117
    start = '20050101'
    end = '20151231'
    
  } else if(station.index == 2) {
    station.elevation = 1068 # ft  file says 740
    station.data = yakima
    
    filename = 'YAKIMA.HOT'
    station.name = 'YAKIMA WBAN24343'
    station.latitude = 47
    station.longitude = 121
    start = '20050101'
    end = '20151231'  
    
  }
  
  # elevations
  # yakima = 1068 ft file says 1063
  # richland = 373 ft , plus 15 ft possibly? file says 2000
  # WNATCHEE = 626 ft   file says 1000
  
  air.temperature = as.numeric(levels(station.data$Tavg))[station.data$Tavg]
  air.temperature = na.interp(air.temperature)
  air.temperature.C = (air.temperature-32)*(5/9)
  
  
  # * CALCULATING VARIABLES NEEDED FOR WEATHER FILE
  # Taken from ReadWeat.for
  
  df = as.data.frame(1:nrow(station.data))
  # solar radiation
  df$Solar = netswr.values[station.index,]
  
  # atmospheric radiation
  # (Ha -Hra)= (1-?ar)1.23 x 10-16 (1.0 + 0.17 C^2)(TDB + 273.)^6
  A1 = (1-0.03)*(1.23E-16)*(1.0+0.17*((clouds[station.index,]/100)**2.0))
  A2 = (air.temperature.C+273.)**6.0
  df$Atmospheric = A1*A2
  
  # air temperature (C)
  df$AirTemperature = air.temperature.C
  
  # wind speed
  # QCLCD reported as 'Average Speed'  MPH
  # NOT the 'Resultant Speed' which is a vector average
  wind = as.numeric(levels(station.data$AvgSpeed))[station.data$AvgSpeed]  
  wind = na.interp(wind)
  df$Wind = wind
  
  # Bowen Ratio
  # ReadWeat.for equations
  # A3 = 0.24*(1013.3*(((288.15-(0.0065*ELEV))/288.15)**5.258))  # note that 5.258 is slightly in error
  # A4 = 0.622*((597.3-0.564*((ATEMP(I)-32.0)*(5.0/9.0))))
  # BOWEN(I) = A3/A4
  # BowenRatio =(ca P) / (0.622 * lambda)
  # P = Barometric Formula  https://en.wikipedia.org/wiki/Barometric_formula
  # lambda = 597.3 – (0.564 Td)  Td in Celcius
  bowen.ratio = function(station.elevation, air.temperature.C) {
    Ca = .24 # heat capacity of air, cal/g/C, 0.24 
    P = 1013.3 *( 288.15 / (288.15 - .0065 * station.elevation  )  ) ^ -5.2558
    lambda = 597.3 - (0.564 * air.temperature.C)
    Bowen = Ca * P / (.622 * lambda)
    return(Bowen)
  }
  df$Bowen = bowen.ratio(station.elevation, air.temperature.C)
  
  # Vapor Pressure
  # ReadWeat.for equations
  #  A5 = (DEWP(I)-32.0)*(5.0/9.0)
  #  A6 = (17.27*A5)/(237.3+A5)
  #  VAPOR(I) = 6.11*EXP(A6)
  #  From ea = 6.11 EXP (17.27 Tdew) / (237.3 + Tdew)
  dewpoint = as.numeric(levels(station.data$DewPoint))[station.data$DewPoint]
  dewpoint = na.interp(dewpoint)
  dewpoint = (dewpoint-32) *(5/9)
  df$VaporPressure = 6.11 * exp( 17.27 * dewpoint / (237.3 + air.temperature.C))
  
  df$PhotoPeriod = 0 # Not used in the model.
  
  
  primary.stations[[station.index]] = df
  n = names(df)
  n[1] = 'Julian'
  names(df) = n
  
  write.hot.file(df, filename, station.name, station.elevation, station.latitude, station.longitude, start, end)
}

# 2: create average T from Tmax and Tmin for non-first order stations
# 3: synthesize data for non-first order stations with first order data

for(station.index in 3:4){
  
  if(station.index == 3){
    
    # wnatchee  
    #data.filename = '../Data/GHCND/USC00459074.2015.csv'
    station.data = USC0045907
    primary.station.index = 2
    station.elevation = 626 # data file says 190.8, station data on ncdc says 626
    filename = 'WNATCHEE.HOT'
    station.name = 'WNATCHEE USC00459074, COOP'
    station.latitude = 47
    station.longitude = 120
    start = '20050101'
    end = '20151231'  
  } else if(station.index == 4){
    # richland  
    #data.filename = '../Data/GHCND/USC00457015.2015.csv'
    station.data = USC00457015
    primary.station.index = 2
    station.elevation = 373
    filename = 'RICHLAND.HOT'
    station.name = 'RICHLAND USC00457015, COOP'
    station.latitude = 46
    station.longitude = 119
    start = '20050101'
    end = '20151231'  
  }
  
  # prepare
  station.data$TMAX = na.interp(station.data$TMAX)
  station.data$TMIN = na.interp(station.data$TMIN)
  tavg = (station.data$TMAX + station.data$TMIN)/2
  air.temperature.C = tavg
  primary.station = primary.stations[[primary.station.index]]
  
  df = primary.station
  # Solar radiation comes from NARR
  df$Solar = netswr.values[station.index,]
  # Atmospheric can still be calculated per station since we have cloud cover from NARR
  A1 = (1-0.03)*(1.23E-16)*(1.0+0.17*((clouds[station.index,]/100)**2.0))
  A2 = (air.temperature.C+273.)**6.0
  df$Atmospheric = A1*A2
  # Air Temp
  df$AirTemperature = air.temperature.C
  # Wind Speed is Synthesized, do nothing
  # Bowen Ratio can be calculated
  df$Bowen = bowen.ratio(station.elevation, air.temperature.C)
  # Vapor Pressure has to be synthesized, since we don't have Wet Bulb T, so do nothing
  # the synthesis of vapor pressure from 1st order data might not be reliable, 
  # since temperatures between stations can be quite different
  
  n = names(df)
  n[1] = 'Julian'
  names(df) = n
  
  write.hot.file(df, filename, station.name, station.elevation, station.latitude, station.longitude, start, end)
  
}


data.filename = '../Data/GHCND/USC00459074.2015.csv'
wn = read.csv(data.filename)
data.filename = '../Data/GHCND/USC00457015.2015.csv'
ri = read.csv(data.filename)
