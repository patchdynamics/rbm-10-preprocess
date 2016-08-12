# Spot Check the Water Temperature Approximation Scripts

Tucannon = read.table('../Data/WashingonDOE/Tucannon.35B060.txt', skip=2, header=FALSE, fill=TRUE, 
                      as.is=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
                      col.names=c('Trip', 'Station', 'Date', 'Time', 'AMPM', 'WaterTemperature', 'AirTemperature') )
head(Tucannon)
daily = ddply(Tucannon, c('Date'), summarize, WaterTemperature=mean(WaterTemperature), AirTemperature=mean(AirTemperature))
ts.daily = xts(daily, order.by=as.Date(daily$Date, '%m/%d/%Y'))
plot(ts.daily)
plot(.indexyday(ts.daily), as.numeric(ts.daily$WaterTemperature))
plot(.indexyday(ts.daily), as.numeric(ts.daily$AirTemperature))

data = data.frame(Julian=.indexyday(ts.daily), AirTemperature=as.numeric(ts.daily$AirTemperature))
fill = 1:min(data$Julian)
data = rbind(data, data.frame(Julian=fill, AirTemperature=0))
fill = max(data$Julian):365
data = rbind(data, data.frame(Julian=fill, AirTemperature=0))
data = data[order(data$Julian),]
T.stream = approximate.stream.temperature(coeffs.df, data)
plot(.indexyday(ts.daily), as.numeric(ts.daily$WaterTemperature), col='blue', typ='l', ylim=c(0,25))
lines(T.stream, typ='l',  col='red')
lines(T.stream.approx, typ='l', col='magenta')
legend('topright', c('Measured', 'Colocated Approx', 'Station Approx'), col=c('blue', 'red', 'magenta'), lwd=1)

# library(metrics)
# rmse()
# if we need to,check each one's RMSE to find the really bad ones