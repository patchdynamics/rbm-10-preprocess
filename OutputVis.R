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
plot(output.impounded$Snake10.T, ylim=c(0,30), typ='l')
lines(ice.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)

lmon.daily = temp.timeseries.daily(lmontail)
plot(output.impounded$Snake41.T, ylim=c(0,30), typ='l')
lines(lmon.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)

lgs.daily = temp.timeseries.daily(lgstail)
plot(output.impounded$Snake70.T, ylim=c(0,30), typ='l')
lines(lgs.daily$daily.T, col='orange')
legend('topleft', c('Modeled', 'Measured'), col=c('black', 'orange'), lwd=1)

lgn.daily = temp.timeseries.daily(lgtail)
plot(output.impounded$Snake107.T, ylim=c(0,30), typ='l')
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
output.scenario1.columbia = read.csv('../ModelOutputs/Scenario1.Columbia.txt', sep=' ', header = FALSE);
output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE);


output = output.scenario1.columbia
names.output = c('Date', 
                  'Columbia516', 'Columbia516.T', 'Columbia516.Std',
                  'Columbia474', 'Columbia474.T', 'Columbia474.Std',
                  'Columbia397', 'Columbia397.T', 'Columbia397.Std',
                  'Columbia216', 'Columbia216.T', 'Columbia216.Std',
                  'Columbia146', 'Columbia146.T', 'Columbia146.Std'
)
names(output) = names.output

output.summer = output[150:250, ]

plot(output.summer$Columbia516.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia474.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia397.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia216.T, typ='l', ylim=c(0,30), ylab='Temperature C')
lines(output.summer$Columbia146.T, typ='l', ylim=c(0,30), ylab='Temperature C')

names(output.scenario1.columbia) = names.output
names(output.scenario0.columbia) = names.output

output.summer = output.scenario0.columbia[150:250, ]
plot(output.summer$Columbia216.T, typ='l', ylim=c(0,30), ylab='Temperature C', col='blue')
lines(output.summer$Columbia146.T, col='red')
output.summer = output.scenario1.columbia[150:250, ]
lines(output.summer$Columbia216.T, col='blue', lty=2)
lines(output.summer$Columbia146.T, col='red', lty=2)
  legend('bottomright',
         c('Bonneville', 'Dalles', 'Bonneville Scenario 1', 'Dalles Scenario 1'),
         col=c('red', 'blue', 'red', 'blue'),
         lty=c(1,1,2,2))

#lines(as.numeric(ts.lgstail[range]$Temperature..C.), col='aquamarine', lty=2)
#lines(ts.lmontail[range]$Temperature..C., col='green')
#lines(as.numeric(ts.ice[range]$Temperature..C.), col='orange')
#legend('topleft', 
#       c('Granite', 'Goose', 'Monumental', 'Ice'), 
#       col=c('black', 'blue', 'green', 'orange'), lwd=1)





#
# Scenario 2
#

output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE);
output.scenario2a.columbia = read.csv('../ModelOutputs/Scenario2a.Columbia.txt', sep=' ', header = FALSE);
output.scenario2b.columbia = read.csv('../ModelOutputs/Scenario2b.Columbia.txt', sep=' ', header = FALSE);
names(output.scenario0.columbia) = names.output
names(output.scenario2a.columbia) = names.output
names(output.scenario2b.columbia) = names.output


plot(output.scenario0.columbia$Columbia216.T, typ='l', ylim=c(0,30), ylab='Temperature C', col='blue',, lwd=2)
lines(output.scenario2a.columbia$Columbia216.T, col='blue', lty=2, lwd=2)
lines(output.scenario2b.columbia$Columbia216.T, col='blue', lty=3, lwd=2)

plot(output.scenario0.columbia$Columbia146.T, col='red', lwd=2, typ='l')
lines(output.scenario2a.columbia$Columbia146.T, col='red', lty=2, lwd=2)
lines(output.scenario2b.columbia$Columbia146.T, col='red', lty=3, lwd=2)
legend('topleft', c('Bonneville', 'Scenario2a', 'Scenario2b'),
       col='red', lty=c(1,2,3), lwd=1)

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

