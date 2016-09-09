bon.scrollcase.max = c(22.2,21.1,21.1,23.3,21.7,20.0,21.1,21.7,21.1,20.6,19.4,21.1,21.7,21.1,21.1,21.1,18.9,20.6,21.7,20.6,23.3,20.0,21.1,22.2,21.7,21.1,20.6,22.2,20.0,22.8,21.7,21.1,21.7,22.8,21.1,22.2,21.7,21.1,20.6,23.3,22.2,22.2,21.7,23.3,22.2,21.7,21.7,22.8,22.8,22.2,22.2,21.7,23.9,22.8,22.8,22.2,22.8,22.2,22.2,22.2)
year = 1938:1997
plot(year, bon.scrollcase.max)

lm.temp = lm(bon.scrollcase.max ~ year)
lines(year, predict(lm.temp))

cut = 1976
##cut = 1965
##cut = 1961

b.s.after = bon.scrollcase.max[year > cut]
b.s.before = bon.scrollcase.max[year <= cut]
points(year[year>cut], b.s.after, col='red')
points(year[year<=cut], b.s.before, col='blue')

lm.temp.after = lm(b.s.after ~ year[year>cut])
lines(year[year>cut], predict(lm.temp.after))

lm.temp.before = lm(b.s.before ~ year[year<=cut])
lines(year[year<=cut], predict(lm.temp.before))

plot(head(bon.scrollcase.max,-1), bon.scrollcase.max[-1])
x1 = head(bon.scrollcase.max,-1)
x2 = bon.scrollcase.max[-1]
lm.tscroll = lm(x2~x1)
summary(lm.tscroll)

library(bbmle)
bon.scrollcase.df = data.frame(tmax = bon.scrollcase.max, year=year)
bon.scrollcase.df$cut = 1
bon.scrollcase.df[year > cut,]$cut=1
bon.scrollcase.df[year <= cut,]$cut=2
nfit.0 = mle2(tmax ~ dnorm(mean = m * year + b, sd = 1),
              start=list(m = .01 , b=0), data = bon.scrollcase.df)

nfit.cut = mle2(tmax ~ dnorm(mean = m * year + b, sd = 1),
              start=list(m = .01 , b=0), data = bon.scrollcase.df,
              parameters = list( m ~ cut, b ~ cut)
              )

anova(nfit.0, nfit.cut)


# now we would do the monte-carlo maximum likelihood
# run the model for other years
# callibrate the model and run it for other years
# in an increase in max temp the same as an overall temp increase

bon.scrollcase.2 = read.csv('../McKenzie/Prepared/bon_scr74-97.csv', header=TRUE 
                            #colClasses=c('character', 'character', 'numeric')
                            )
bon.scrollcase.2$DegC = as.numeric(levels(bon.scrollcase.2$DegC))[bon.scrollcase.2$DegC]
yearly.bon.scrollcase.2 = ddply(bon.scrollcase.2,  c('Date'), summarize, 
                                mean = mean(DegC, na.rm=TRUE))
yearly.bon.scrollcase.2 = yearly.bon.scrollcase.2[2:18,]
plot(as.numeric(levels(yearly.bon.scrollcase.2$Date))[yearly.bon.scrollcase.2$Date],
     yearly.bon.scrollcase.2$mean)



# Callibration
bon.combined.sources = read.csv('../McKenzie/Prepared/bon_combined_sources.csv', header=TRUE)

library(Metrics)
attach(output.scenario0.columbia.75)
year = 1991
column = paste0('X', year)
plot(Columbia146.T[Year==year], typ='l')
lines(bon.combined.sources[,(column)], col='orange')
#rmse(Columbia146.T[Year==year & !is.na(bon.combined.sources[,(column)])],
#     bon.combined.sources[!is.na(bon.combined.sources[,(column)])],(column))
legend('topleft', c('Modeled', 'Actual'), lwd=1, col=c('black', 'orange'))
detach(output.scenario0.columbia.75)


bon.scrollcase.2016 = read.csv('~/Downloads/rivermg_1471641126_509.csv')
str(bon.scrollcase.2016)
plot(bon.scrollcase.2016$tempscr, typ='l', col='orange')
lines(bon.daily, col='magenta')
lines(output.scenario0.columbia$Columbia146.T)
legend('topleft', c('Scroll Case', 'Forebay', 'Model'), lwd=1, col=c('magenta', 'orange', 'black'))





