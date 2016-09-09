factor2numeric <- function(f)
{
  if(!is.factor(f)) stop("the input must be a factor")
  as.numeric(levels(f))[as.integer(f)]
}

output.scenario0.columbia.75 = read.csv('../ModelOutputs/1975/Scenario0.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
output.scenario1.columbia.75 = read.csv('../ModelOutputs/1975/Scenario1.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);

output.scenario0.snake.75 = read.csv('../ModelOutputs/1975/Scenario0.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);
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

