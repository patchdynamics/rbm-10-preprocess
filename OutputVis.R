names.output.snake = c('Date', 
                       'Snake168', 'Snake168.T', 'Snake168.Std',
                       'Snake107', 'Snake107.T', 'Snake107.Std',
                       'Snake70', 'Snake70.T', 'Snake70.Std',
                       'Snake41', 'Snake41.T', 'Snake41.Std',
                       'Snake10', 'Snake10.T', 'Snake10.Std'
)

output.impounded = read.csv('../ModelOutputs/Scenario0.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);
output.scenario1 = read.csv('../ModelOutputs/Scenario1.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);
output.scenario1ice = read.csv('../ModelOutputs/Scenario1ice.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);
output.scenario1granite = read.csv('../ModelOutputs/Scenario1granite.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);


par(mfrow=c(1,1))
output = output.impounded
output.summer = output[150:250, ]
plot(output.summer$Snake168.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Snake107.T, typ='l', col='blue')
lines(output.summer$Snake70.T, typ='l', col='green')
lines(output.summer$Snake41.T, typ='l', col='orange')
lines(output.summer$Snake10.T, typ='l', col='red')

legend('bottomright',
       c('Anantone', 'Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor'),
       col=c('black', 'blue', 'green', 'orange', 'red'),
       lwd=1
)


ice.daily = temp.timeseries.daily(ice)
lmon.daily = temp.timeseries.daily(lmontail)
lgs.daily = temp.timeseries.daily(lgstail)
lgn.daily = temp.timeseries.daily(lgtail)

par(mfrow=c(2,2))
plot(output.impounded$Snake10.T, ylim=c(0,30), typ='l', main='Ice Harbor',
     xlab='Julian Day', ylab='Temperature (C)')
lines(ice.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)

plot(output.impounded$Snake41.T, ylim=c(0,30), typ='l', main='Lower Monumental',
     xlab='Julian Day', ylab='Temperature (C)')
lines(lmon.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)

plot(output.impounded$Snake70.T, ylim=c(0,30), typ='l', main='Little Goose',
     xlab='Julian Day', ylab='Temperature (C)')
lines(lgs.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)

plot(output.impounded$Snake107.T, ylim=c(0,30), typ='l', main='Lower Granite',
     xlab='Julian Day', ylab='Temperature (C)')
lines(lgn.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)




names(output.scenario1ice) = names(output) = names.output.snake
names(output.scenario1granite) = names(output) = names.output.snake
plot(output.scenario1ice[150:250,]$Snake10.T,typ='l')
lines(output.scenario1granite[150:250,]$Snake10.T, col='red')
legend('bottomright', 
       c('Ice Harbor Unimpounded', 'Lower Granite Unimpounded'),
       col=c('black', 'red'),
       lwd=1
       )

# Middle Columbia
names.output.columbia = c('Date', 
                          'Columbia516', 'Columbia516.T', 'Columbia516.Std',
                          'Columbia474', 'Columbia474.T', 'Columbia474.Std',
                          'Columbia397', 'Columbia397.T', 'Columbia397.Std',
                          'Columbia292', 'Columbia292.T', 'Columbia292.Std',
                          'Columbia216', 'Columbia216.T', 'Columbia216.Std',
                          'Columbia192', 'Columbia192.T', 'Columbia192.Std',
                          'Columbia146', 'Columbia146.T', 'Columbia146.Std'
                          )
names.output.columbia.mcnary = c('Date', 
                          'Columbia516', 'Columbia516.T', 'Columbia516.Std',
                          'Columbia474', 'Columbia474.T', 'Columbia474.Std',
                          'Columbia397', 'Columbia397.T', 'Columbia397.Std',
                          'Columbia333', 'Columbia333.T', 'Columbia333.Std',
                          'Columbia292', 'Columbia292.T', 'Columbia292.Std',
                          'Columbia216', 'Columbia216.T', 'Columbia216.Std',
                          'Columbia192', 'Columbia192.T', 'Columbia192.Std',
                          'Columbia146', 'Columbia146.T', 'Columbia146.Std'
)


output.scenario1.columbia = read.csv('../ModelOutputs/Scenario1.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
#output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
#output.scenarioZ.columbia = read.csv('../ModelOutputs/ScenarioZ.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
#output.scenarymcnary.columbia = read.csv('../ModelOutputs/ScenarioMcNary.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
#output.scenarymcnaryb.columbia = read.csv('../ModelOutputs/ScenarioMcNaryb.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);


# Run the Model, look at the outputs
system('../Model/run2015.ScenarioMcNary.x.sh')
output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
output.scenario1.columbia = read.csv('../ModelOutputs/Scenario1.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
output.scenarymcnary.columbia = read.csv('../ModelOutputs/ScenarioMcNary.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
output.scenarymcnaryb.columbia = read.csv('../ModelOutputs/ScenarioMcNaryb.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
output.scenarymcnaryc.columbia = read.csv('../ModelOutputs/ScenarioMcNaryc.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);

plot.legend = function(){
  legend('bottomright', c('Chief Joseph','Rocky Reach', 'Priest Rapids', 'McNary Pool Upstream', 'McNary', 'John Day'),
         col=c('black', 'black', 'blue', 'orange', 'red', 'pink'),
         lwd=2
         )
}

output.summer = output.scenario0.columbia[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='Impounded')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='pink', lwd=2) #John Day
plot.legend()

output.summer = output.scenarymcnary.columbia[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='McNary Removed')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='pink', lwd=2) #John Day
plot.legend()

# Snake Dams Removed
# Upper is just Scenario 0 (or 1, but 0 has the Pre-McNary data point output)
output = output.scenario0.columbia 
output.summer = output[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='Snake Dams Removed')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
# Lower is Scenario 1
output.summer = output.scenario1.columbia [150:250, ]
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='pink', lwd=2) #John Day
plot.legend()

output = output.scenarymcnaryb.columbia
output.summer = output[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='McNary and Snake Dams Removed')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='pink', lwd=2) #John Day
plot.legend()


output.summer = output.scenarymcnaryc.columbia[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='McNary, Snake Removed, Yakima 20C')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='red') #McNary



output.scenario2c.columbia = read.csv('../ModelOutputs/Scenario2c.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
output.summer = output.scenario2c.columbia[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='Grand Coulee Somewhat Absurdly Cold')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='pink', lwd=2) #John Day
plot.legend()


# 120000 16
# 120000 14
# 120000 12
# 120000 10
# Absurd 160000 10
# Somewhat Absurd 160000 12


output.scenarioMNJD.columbia = read.csv('../ModelOutputs/MNJDDrawdown.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
output.summer = output.scenario2c.columbia[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='McNary & John Day 50% Drawdown, Grand Coule Somewhat Absurb')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='pink', lwd=2) #John Day
plot.legend()

output.scenarioMNJDSnk.columbia = read.csv('../ModelOutputs/MNJDDrawdownSnk.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia.mcnary);
output.summer = output.scenarioMNJDSnk.columbia[150:250, ]
plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C', main='McNary & John Day 50% Drawdown, Snake Dams Removed')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), col='blue') #Preist
lines(output.summer$Columbia333.T, typ='l', ylim=c(0,30), col='orange') #Before Snake
lines(output.summer$Columbia292.T, typ='l', ylim=c(0,30), col='red') #McNary
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), col='pink', lwd=2) #John Day
plot.legend()
















output.summer = output.scenario0.columbia[150:250, ]
plot(output.summer$Columbia216.T, typ='l', ylim=c(15,25), ylab='Temperature C', col='blue')
lines(output.summer$Columbia146.T, col='red')
output.summer = output.scenarioZ.columbia[150:250, ]
lines(output.summer$Columbia216.T, col='blue', lty=2)
lines(output.summer$Columbia146.T, col='red', lty=2)
  legend('bottomright',
         c('Bonneville', 'Dalles', 'Bonneville Scenario 1', 'Dalles Scenario 1'),
         col=c('red', 'blue', 'red', 'blue'),
         lty=c(1,1,2,2))

#McNary
output.summer = output.scenario0.columbia[150:250, ]
plot(output.summer$Columbia292.T, typ='l', ylim=c(15,25), ylab='Temperature C', col='red')
output.summer = output.scenario1.columbia[150:250, ]
lines(output.summer$Columbia292.T, col='blue')

plot(mcnary.daily$daily.T, typ='l', ylim=c(0,25))
lines(output.scenario0.columbia$Columbia292.T, col='red')

plot(priest.daily$daily.T, type='l', ylim=c(0,25))
lines(output.scenario0.columbia$Columbia397.T, col='red')


# John Day - not in the file
#output.summer = output.scenario0.columbia[150:250, ]
#plot(output.summer$Columbia2.T, typ='l', ylim=c(15,25), ylab='Temperature C', col='red')
#output.summer = output.scenario1.columbia[150:250, ]
#lines(output.summer$Columbia292.T, col='blue')


par(mfrow=c(1,1))
plot(output.scenario1.columbia$Columbia146.T, ylim=c(0,30), typ='l', main='Bonneville',
     xlab='Julian Day', ylab='Temperature (C)')
lines(bon.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)


#lines(as.numeric(ts.lgstail[range]$Temperature..C.), col='aquamarine', lty=2)
#lines(ts.lmontail[range]$Temperature..C., col='green')
#lines(as.numeric(ts.ice[range]$Temperature..C.), col='orange')
#legend('topleft', 
#       c('Granite', 'Goose', 'Monumental', 'Ice'), 
#       col=c('black', 'blue', 'green', 'orange'), lwd=1)





#
# Scenario 2
#
output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
output.scenario2a.columbia = read.csv('../ModelOutputs/Scenario2a.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
output.scenario2b.columbia = read.csv('../ModelOutputs/Scenario2b.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
output.scenario23.columbia = read.csv('../ModelOutputs/Scenario2+3.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);


par(mfrow=c(1,2))
plot(output.scenario0.columbia$Columbia216.T, typ='l', ylim=c(0,25), ylab='Temperature C', col='blue',, lwd=2)
lines(output.scenario2a.columbia$Columbia216.T, col='blue', lty=2, lwd=2)
lines(output.scenario2b.columbia$Columbia216.T, col='blue', lty=3, lwd=2)
legend('topleft', c('The Dalles', 'Scenario2a', 'Scenario2b'),
       col='blue', lty=c(1,2,3), lwd=1)

plot(output.scenario0.columbia$Columbia146.T, col='red', ylim=c(0,25), ylab='Temperature C', lwd=2, typ='l')
lines(output.scenario2a.columbia$Columbia146.T, col='red', lty=2, lwd=2)
lines(output.scenario2b.columbia$Columbia146.T, col='red', lty=3, lwd=2)
legend('topleft', c('Bonneville', 'Scenario2a', 'Scenario2b'),
       col='red', lty=c(1,2,3), lwd=1)

par(mfrow=c(1,1))
plot(output.scenario0.columbia$Columbia146.T, col='red', ylim=c(0,25), ylab='Temperature C', lwd=1, typ='l')
lines(output.scenario23.columbia$Columbia146.T, col='red', lty=2, lwd=2)
//legend('topleft', c('Bonneville', 'Scenario2a', 'Scenario2b'),
//       col='red', lty=c(1,2,3), lwd=1)



bon.daily = temp.timeseries.daily(bon)
lines(bon.daily$daily.T)
dalles.daily = temp.timeseries.daily(dalles)
dalles.forebay.daily = temp.timeseries.daily(dallesforebay)

plot(output.scenario0.columbia$Columbia146.T, col='red', lwd=2)
lines(dalles.daily$daily.T)
lines(dalles.forebay.daily$daily.T)


wells.daily = temp.timeseries.daily(wells)
plot(output.scenario0.columbia$Columbia516.T, ylim=c(0,30), typ='l')
lines(wells.daily$daily.T, col='blue')

rockyreach.daily = temp.timeseries.daily(rockyreachtail)
plot(output.scenario0.columbia$Columbia474.T, ylim=c(0,30), typ='l')
lines(rockyreach.daily$daily.T, col='blue')

priest.daily = temp.timeseries.daily(priesttail)
plot(output.scenario0.columbia$Columbia397.T, ylim=c(0,30), typ='l')
lines(priest.daily$daily.T, col='blue')

plot(output.scenario0.columbia$Columbia292.T, typ='l', ylim=c(0,30), col='black') #McNary
lines(mcnary.daily$daily.T, col='blue')
lines(output.scenario0.columbia$Columbia397.T, ylim=c(0,30), typ='l',col='aquamarine')
lines(priest.daily$daily.T, col='green')

