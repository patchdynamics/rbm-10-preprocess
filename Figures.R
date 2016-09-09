# Figure 1
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)

johnday.daily$daily.T.F = johnday.daily$daily.T* 9/5 + 32
johnday.daily$daily.T.F = na.approx(johnday.daily$daily.T.F)
mcnary.daily$daily.T.F = mcnary.daily$daily.T* 9/5 + 32
mcnary.daily$daily.T.F = na.approx(mcnary.daily$daily.T.F)

png(filename = "columbia.png", width = 1040)

par(mfrow=c(1,2))
par(mar = c(5, 5, 4, 2)) 

plot(johnday.daily$daily.T.F, typ='l', ylab='Temperature (F)',
     main='John Day Dam', lwd=2, xaxt='n', ylim=c(39, 75), xlab='', cex.axis=1.6, cex.lab=1.6, cex.main=2)
#abline(70,0, lwd=2, col='red')
abline(68,0, lwd=2, col='red')
axis(1, at=c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
     labels=mymonths, cex.axis=1.4)
#abline(0,0, 0, 210, col='green', lty=3)

     #col=c('black', 'red')[(johnday.daily$daily.T.F >= 70) + 1])

plot(mcnary.daily$daily.T.F, typ='l', ylab='Temperature (F)', 
     main='McNary Dam', lwd=2, xaxt='n', ylim=c(39, 75), xlab='', cex.axis=1.6, cex.lab=1.6, cex.main=2)
#abline(70,0, lwd=2, col='red')
abline(68,0, lwd=2, col='red')
axis(1, at=c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335), 
     labels=mymonths, cex.axis=1.4)
#abline(0,0, 0, 210, col='green', lty=3)
dev.off();

priest.daily = temp.timeseries.daily(priesttail)

plot(johnday.daily$daily.T.F, typ='l', ylab='Temperature (F)',
main='', lwd=2, xaxt='n', ylim=c(39, 75), xlab='', cex.axis=1.6, cex.lab=1.6, cex.main=2)
lines(mcnary.daily$daily.T.F, col='orange2', lwd=2)
axis(1, at=c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
labels=mymonths, cex.axis=1.4)
abline(68,0, lwd=2, col='red')
lines(priest.daily$daily.T* 9/5 + 32, typ='l', col='pink2', lwd=2)

plot(df.approx.T$"Walla.Walla"[150:250]*9/5 + 32, typ='l')

# Try Scenario 1 with Walla Walla at 68
df.approx.T2 = df.approx.T
df.approx.T2$Walla.Walla = 68
