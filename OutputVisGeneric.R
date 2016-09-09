scenario = 'Calibration'


factor2numeric <- function(f)
{
  if(!is.factor(f)) stop("the input must be a factor")
  as.numeric(levels(f))[as.integer(f)]
}


output.calibration = read.csv('../ModelOutputs/Calibration.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
output.scenario1.generic = read.csv('../ModelOutputs/2005.2016.Scenario1.inp.Columbia.txt', sep=' ', header=FALSE, col.names=names.output.columbia)

projects = c('BON', 'TDDO', 'JHAW', 'MCPW')
projects = c('PRXW', 'RRDW')
temperatures.dams = NULL
for(project in projects) {
  data = download.dart.daily(project, syear, eyear)
  if(is.null(discharges.dams)){
    temperatures.dams = as.numeric(data$Temperature..C.)
  } else {
    temperatures.dams = cbind(temperatures.dams, as.numeric(data$Temperature..C.))
  }
}
temperatures.dams.obs = as.data.frame(temperatures.dams)
names(temperatures.dams.obs) = projects

plot(output.calibration$Columbia146.T, typ='l')
plot(temperatures.dams.obs$BON, typ='l')

plot(output.calibration$Columbia146.T, typ='l')
lines(temperatures.dams.obs$BON,  col='red')

plot(output.calibration$Columbia192.T, typ='l')
lines(temperatures.dams.obs$TDDO,  col='red')

plot(output.calibration$Columbia216.T, typ='l')
lines(temperatures.dams.obs$JHAW,  col='red')

plot(output.calibration$Columbia292.T[3500:4000], typ='l')
lines(temperatures.dams.obs$MCPW[3500:4000],  col='red')

plot(factor2numeric(output.calibration$Columbia397.T[3500:4000]), typ='l')  # Priest Rapids Dam
lines(temperatures.dams.obs$PRXW[3500:4000],  col='red')

plot(output.calibration$Columbia474.T[3500:4000], typ='l')  # Priest Rapids Dam
lines(temperatures.dams.obs$RRDW[3500:4000],  col='red')

plot(output.calibration$Columbia474.T, typ='l')  # Priest Rapids Dam
lines(temperatures.dams.obs$RRDW,  col='red')


# Scenarios
output.scenario1.generic$Columbia292.T = factor2numeric(output.scenario1.generic$Columbia292.T) #ANNOY

plot(output.calibration$Columbia292.T, typ='l', col='red')
lines(output.scenario1.generic$Columbia292.T,  col='blue')

plot( factor2numeric(output.scenario1.generic$Columbia292.T),  col='blue')


output.calibration$Year = as.numeric(substr(output.calibration$Date, 1,4))
output.scenario1.generic$Year = as.numeric(substr(output.scenario1.generic$Date, 1,4))

y = 2015:2015
par(mfrow=c(1,1))
for(year in y ){
  output = output.calibration[output.calibration$Year == year, ]
  plot(output$Columbia146.T, typ='l', ylim=c(0,30), ylab='Temperature C', col='blue', xlab = y[i])
  output = output.scenario1.generic[output.scenario1.generic$Year == year, ]
  lines(output$Columbia146.T, col='red', lty=2)
}



#output.scenario1.columbia.75 = read.csv('../ModelOutputs/1975/Scenario1.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);

output.scenario0.snake.75 = read.csv('../ModelOutputs/Calibration.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);
output.scenario1.snake.75 = read.csv('../ModelOutputs/1975/Scenario1.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);


output.scenario0.columbia.75$Year = floor(output.scenario0.columbia.75$Date)
output.scenario1.columbia.75$Year = floor(output.scenario1.columbia.75$Date)
output.scenario0.snake.75$Year = floor(output.scenario0.snake.75$Date)
output.scenario1.snake.75$Year = floor(output.scenario1.snake.75$Date)


plot(output.scenario0.columbia.75$Columbia146.T, typ='l', ylim=c(0,25), ylab='Temperature C', col='blue')
lines(output.scenario1.columbia.75$Columbia146.T, col='red', lty=2)

output.scenario0.columbia.75$Year = as.numeric(substr(output.scenario0.columbia.75$Date, 1,4))
output.scenario1.columbia.75$Year = as.numeric(substr(output.scenario1.columbia.75$Date, 1,4))

y = 1975:1995
y = 1990:1995
par(mfrow=c(2,2))
for(i in 1:length(y) ){
  output.summer = output.scenario0.columbia.75[output.scenario0.columbia.75$Year == y[i], ]
  plot(output.summer$Columbia146.T, typ='l', ylim=c(0,25), ylab='Temperature C', col='blue', xlab = y[i])
  output.summer = output.scenario1.columbia.75[output.scenario1.columbia.75$Year == y[i], ]
  lines(output.summer$Columbia146.T, col='red', lty=2)
}

#McNary
for(i in 1:length(y) ){
  output.summer = output.scenario0.columbia.75[output.scenario0.columbia.75$Year == y[i], ]
  plot(factor2numeric(output.summer$Columbia292.T), typ='l', ylim=c(0,25), ylab='Temperature C', col='blue', xlab = y[i])
  output.summer = output.scenario1.columbia.75[output.scenario1.columbia.75$Year == y[i], ]
  lines(factor2numeric(output.summer$Columbia292.T), col='red', lty=2)
}

plot(mcnary.1995.daily, typ='l')
output = output.scenario0.columbia.75[output.scenario0.columbia.75$Year == 1995, ]
plot(factor2numeric(output$Columbia292.T), typ='l', ylim=c(0,25), ylab='Temperature C', col='blue', xlab = y[i])
lines(mcnary.1995.daily, typ='l')

output = output.scenario0.snake.75[output.scenario0.snake.75$Year == 1995, ]
plot(output$Snake10.T, typ='l', ylim=c(0,25), ylab='Temperature C', col='blue', xlab = y[i])
lines(ice.daily.1995, typ='l')

output = output.scenario0.columbia.75[output.scenario0.columbia.75$Year == 1995, ]
plot(factor2numeric(output$Columbia397.T), typ='l', ylim=c(0,25), ylab='Temperature C', col='blue', xlab = y[i])
lines(priesttail.1995.daily, typ='l')


//lines(output.summer$Columbia146.T, col='red')
//output.summer = output.scenario1.columbia[150:250, ]
//lines(output.summer$Columbia216.T, col='blue', lty=2)
//lines(output.summer$Columbia146.T, col='red', lty=2)
legend('bottomright',
       c('Bonneville', 'Dalles', 'Bonneville Scenario 1', 'Dalles Scenario 1'),
       col=c('red', 'blue', 'red', 'blue'),
       lty=c(1,1,2,2))

