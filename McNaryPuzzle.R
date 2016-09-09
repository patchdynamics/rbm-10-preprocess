
plot(output.scenario0.columbia$Columbia292.T, typ='l', ylim=c(0,30), col='black') #McNary
lines(mcnary.daily$daily.T, col='red')
lines(output.scenario0.columbia$Columbia397.T, ylim=c(0,30), typ='l',col='aquamarine')
lines(priest.daily$daily.T, col='green')

plot(output.impounded$Snake10.T, ylim=c(0,30), typ='l')
lines(ice.daily$daily.T, col='blue')
lines(mcnary.daily$daily.T, col='red')
lines(output.scenario0.columbia$Columbia292.T, col='red')


output.impounded = read.csv('../ModelOutputs/Scenario0.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);

plot(output.impounded$Snake10.T, ylim=c(0,30), typ='l')
lines(ice.daily$daily.T, col='blue')

plot(output.impounded$Snake107.T, typ='l')
lines(lgn.daily$daily.T)


rockyreach.daily = temp.timeseries.daily(rockyreachtail)
plot(output.scenario0.columbia$Columbia474.T, ylim=c(0,30), typ='l')
lines(rockyreach.daily$daily.T, col='blue')

output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
plot(mcnary.daily$daily.T, col='red', typ='l')
lines(output.scenario0.columbia$Columbia292.T, col='pink', lwd=2)

plot(output.scenario0.columbia$Columbia397.T, ylim=c(0,30), typ='l',col='aquamarine')
lines(priest.daily$daily.T, col='green')

output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
plot(output.scenario0.columbia$Columbia146.T, ylim=c(0,30), typ='l')
lines(bon.daily$daily.T, col='blue')

output.scenario1.columbia = read.csv('../ModelOutputs/Scenario1.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
output.scenario0.columbia = read.csv('../ModelOutputs/Scenario0.Columbia.txt', sep=' ', header = FALSE, col.names=names.output.columbia);
plot(output.scenario0.columbia$Columbia292.T, typ='l', ylim=c(0,30), col='black', xaxt='n') #McNary
lines(output.scenario1.columbia$Columbia292.T, col='blue')
plot(output.scenario0.columbia$Columbia292.T[150:250], typ='l', ylim=c(0,30), col='black', xaxt='n') #McNary
lines(output.scenario1.columbia$Columbia292.T[150:250], col='blue')
abline(h=20, col='red')
mymonths <- c("Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
myticks = c(152, 182, 213, 244, 274, 305, 335)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)


par(mfrow=c(1,3))
diff = output.scenario0.columbia$Columbia292.T - output.scenario1.columbia$Columbia292.T
plot(diff, ylim=c(0,2.5), ylab='Diff between scenarios')

diff.mcnary = output.scenario0.columbia$Columbia292.T - output.scenario0.columbia$Columbia397.T
plot(diff.mcnary, ylim=c(0,2.5), ylab='Modeled Diff between Priest Rapids and McNary')

diff.mcnary.obs = mcnary.daily$daily.T - priest.daily$daily.T
plot(diff.mcnary.obs, ylim=c(0,2.5), ylab='Actual Diff between Priest Rapids and McNary')


plot(output.scenario0.columbia$Columbia146.T[150:250], typ='l', ylim=c(0,30), col='black', xaxt='n') #McNary
lines(output.scenario1.columbia$Columbia146.T[150:250], col='blue')
abline(h=20, col='red')
abline(h=21.1, col='red')

mymonths <- c("Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
myticks = c(152, 182, 213, 244, 274, 305, 335)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)

plot(bon.daily$daily.T)

diff.bon.scenarios = output.scenario0.columbia$Columbia146.T - output.scenario1.columbia$Columbia146.T
plot(diff.bon.scenarios, ylim=c(0,2.5))




