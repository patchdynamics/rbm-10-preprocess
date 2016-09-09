# output.scenario on the Lower Snake

output.scenario = output.scenario1
output.scenario = output.impounded

width = 480
height = 480
png(filename = "lg.scenario1.png", width = width, height = height)
plot(output.scenario$Snake107.T[150:250]*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='blue', lwd=2, xaxt='n',  xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Lower Granite'),
       lwd=2, col=c('blue'), cex=1.6
)
dev.off()

png(filename = "lgs.scenario1.png", width = width, height = height)
plot(output.scenario$Snake70.T[150:250]*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='green', lwd=2, xaxt='n', xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Little Goose'),
       lwd=2, col=c('green'), cex=1.6
)
dev.off()

png(filename = "lmon.scenario1.png", width = width, height = height)
plot(output.scenario$Snake41.T[150:250]*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='orange', lwd=2, xaxt='n', xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Lower Monumental'),
       lwd=2, col=c('orange2'), cex=1.6
)
dev.off()

png(filename = "ice.scenario1.png", width = width, height = height)
plot(output.scenario$Snake10.T[150:250]*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='red', lwd=2, xaxt='n', xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
legend('bottomright', c('Ice Harbor'),
       lwd=2, col=c('red'), cex=1.6
)
dev.off()

png(filename = "all.scenario0.png", width = width, height = height)
plot(output.scenario$Snake107.T[150:250]*9/5 + 32, typ='l', ylim=c(55,75), ylab='Temperature (F)', col='blue', lwd=2, xaxt='n',  xlab='', cex.axis=1.6, cex.lab=1.6)
axis(1, at=myticks-150, labels=mymonths, cex.axis=1.6)
lines(output.scenario$Snake70.T[150:250]*9/5 + 32, lwd=2,  col='green')
lines(output.scenario$Snake41.T[150:250]*9/5 + 32,lwd=2, col='orange')
lines(output.scenario$Snake10.T[150:250]*9/5 + 32, lwd=2, col='red')

lines(output.scenario$Snake168.T[150:250]*9/5 + 32, lwd=2, col='purple')

legend('bottomright', c('Lower Granite', 'Little Goose', 'Lower Monumental', 'Ice Harbor'),
       lwd=2, col=c('blue', 'green', 'orange2', 'red'), cex=1.2
)
dev.off()
