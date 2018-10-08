options(digits = 4) 
library(tseries)
library(forecast)
library(lmtest)

workdir="F:\\r.python\\r_workingdirectory\\data"
setwd(workdir)

data=read.table("F:\\r.python\\r_workingdirectory\\data\\CaliforniaHospital_FinancialData.txt",header=T,sep='\t')
names(data)


net.total_ts=ts(data$NET_TOT,frequency=4)
opexp_ts=ts(data$TOT_OP_EXP,freq=4)
non_oprev_ts=ts(data$NONOP_REV,freq=4)

layout(1:3)
plot(net.total_ts)
plot(opexp_ts)
plot(non_oprev_ts)

net.total_dc=decompose(net.total_ts)
opexp_dc=decompose(opexp_ts)
non_oprev_dc=decompose(non_oprev_ts)


net.total_trend=net.total_dc$trend
nettotal_trenddata=data.frame(trend=c(net.total_trend),time=c(time(net.total_trend)))

nettotal_reg=lm(nettotal_trenddata$trend ~ nettotal_trenddata$time)
summary(nettotal_reg)


opexp_trend=opexp_dc$trend
opexp_trenddata=data.frame(trend=c(opexp_trend),time=c(time(opexp_trend)))

opexp_reg=lm(opexp_trenddata$trend ~ opexp_trenddata$time)
summary(opexp_reg)


non_oprev_trend=non_oprev_dc$trend
nonoprev_trenddata=data.frame(trend=c(non_oprev_trend),time=c(time(non_oprev_trend)))

nonoprev_reg=lm(nonoprev_trenddata$trend ~ nonoprev_trenddata$time)
summary(nonoprev_reg)


nettotal_trend=net.total_ts-net.total_dc$seasonal
Opexp_trend=opexp_ts-opexp_dc$seasonal
nonoprev_trend=non_oprev_ts-non_oprev_dc$seasonal


adf.test(nettotal_trend,k=0,alternative="stationary")
kpss.test(nettotal_trend)

adf.test(Opexp_trend,k=0,alternative="stationary")
kpss.test(Opexp_trend)

adf.test(nonoprev_trend,k=0,alternative="stationary")
kpss.test(nonoprev_trend)

pacf(net.total_ts)
acf(opexp_ts)
acf(non_oprev_ts)
pacf(non_oprev_ts)




nettotal_diff1=diff(nettotal_trend,differences=1)
plot(nettotal_diff1)
adf.test(nettotal_diff1,alternative='stationary')
kpss.test(nettotal_diff1)

opexp_diff1=diff(Opexp_trend,differences=1)
plot(opexp_diff1)
adf.test(opexp_diff1,alternative='stationary')
kpss.test(opexp_diff1)

nonoprev_diff1=diff(nonoprev_trend,differences=1)
plot(nonoprev_diff1)
adf.test(nonoprev_diff1,alternative='stationary')
kpss.test(nonoprev_diff1)

nonoprev_diff2=diff(nonoprev_trend,differences=2)
plot(nonoprev_diff2)
adf.test(nonoprev_diff2,alternative='stationary')
kpss.test(nonoprev_diff2)

layout(1:2)
acf(nettotal_diff1,lag.max=20)
pacf(nettotal_diff1,lag.max=20)


nettotal_arima1=arima(nettotal_diff1,order=c(0,0,1),method="ML")
nettotal_arima1
nettotal_arima2=arima(nettotal_diff1,order=c(1,0,1),method="ML")
nettotal_arima2
nettotal_arima3=arima(nettotal_diff1,order=c(2,0,2),method="ML")
nettotal_arima3


coeftest(nettotal_arima1)
nettotal_arima1_bic=AIC(nettotal_arima1,k=log(length(nettotal_diff1)))
nettotal_arima1_bic
nettotal_arima2_bic=AIC(nettotal_arima2,k=log(length(nettotal_diff1)))
nettotal_arima2_bic
nettotal_arima3_bic=AIC(nettotal_arima3,k=log(length(nettotal_diff1)))
nettotal_arima3_bic

nettotal_arima1_fore=forecast(nettotal_arima1,h=10)
plot(nettotal_arima1_fore)
accuracy(nettotal_arima1_fore)




layout(1:2)
acf(opexp_diff1,lag.max=20)
pacf(opexp_diff1,lag.max=20)

opexp_arima1=arima(opexp_diff1,order=c(0,0,1),method="ML")
opexp_arima1
opexp_arima2=arima(opexp_diff1,order=c(1,0,0),method='ML')
opexp_arima2
opexp_arima3=arima(opexp_diff1,order=c(1,0,2),method='ML')
opexp_arima3

coeftest(opexp_arima3)
opexp_arima3_bic=AIC(opexp_arima3,k=log(length(opexp_diff1)))
opexp_arima3_bic

opexp_arima3_fore=forecast(opexp_arima3,h=10)
opexp_arima3_fore
plot(opexp_arima3_fore)
accuracy(opexp_arima3_fore)


layout(1:2)
acf(nonoprev_diff2,lag.max=20)
pacf(nonoprev_diff2,lag.max=20)


nonoprev_arima1=arima(nonoprev_diff2,order=c(0,0,1),method='ML')
nonoprev_arima1
nonoprev_arima2=arima(nonoprev_diff2,order=c(0,0,2),method='ML')
nonoprev_arima2
nonoprev_arima3=arima(nonoprev_diff2,order=c(1,0,1),method='ML')
nonoprev_arima3

nonoprev_arima3=arima(nonoprev_diff1,order=c(1,0,1),method='ML')
nonoprev_arima3
nonoprev_arima=arima(nonoprev_trend,order=c(1,1,1),method='ML')
nonoprev_arima



coeftest(nonoprev_arima2)
acf(nonoprev_arima2)


nonoprev_arima2_bic=AIC(nonoprev_arima2,k=log(length(nonoprev_diff2)))
nonoprev_arima2_bic

nonoprev_arima2_fore=forecast(nonoprev_arima2,h=10)
nonoprev_arima2_fore
plot(nonoprev_arima2_fore)
accuracy(nonoprev_arima2_fore)







