
#output.scenario1
#output.impounded

system('../Model/run2015.sh')

output.impounded = read.csv('../ModelOutputs/Crazy.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);
output.impounded = read.csv('../ModelOutputs/Calibration.Snake.txt', sep=' ', header = FALSE, col.names=names.output.snake);

summer.impounded = output.impounded[150:250,]*9/5 + 32

par(mfrow=c(1,2))
plot(summer.impounded$Snake107.T, typ='l', col='red')
lines(lgn.daily[150:250,]$daily.T * 9/5 + 32, typ='l')

plot(summer.impounded$Snake10.T, typ='l', col='red')
lines(ice.daily[150:250,]$daily.T * 9/5 + 32, typ='l')

par(mfrow=c(1,1))
plot(output.scenario$Snake107.T[150:250]*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='blue', lwd=2, xaxt='n',  xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
lines(output.scenario$Snake70.T[150:250]*9/5 + 32, lwd=2,  col='green')
lines(output.scenario$Snake41.T[150:250]*9/5 + 32,lwd=2, col='orange')
lines(output.scenario$Snake10.T[150:250]*9/5 + 32, lwd=2, col='red')

lines(output.scenario$Snake168.T[150:250]*9/5 + 32, lwd=2, col='purple')



summer.scenario1 = output.scenario1[150:250,]*9/5 + 32


par(mfrow=c(1,1))
data.lg = download.dart.daily('LGNW', 2015, 2015)

data1 = importDVs(c('13334300'),code="00060", sdate=sdate, edate=edate)
data2 = importDVs(c('13342500'),code="00060", sdate=sdate, edate=edate)

val = data1$val + data2$val

data.ice = download.dart.daily('IDSW', 2015, 2015)

plot(as.numeric(data.lg$Outflow..kcfs.)*1000, typ='l', col='red')
lines(val)
lines(as.numeric(data.ice$Outflow..kcfs.)*1000, typ='l', col='orange')



