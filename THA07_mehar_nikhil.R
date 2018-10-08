workdir="F:\\r.python\\r_workingdirectory\\data"
setwd(workdir)
options(digits=4)

library(forecast)

data=read.table("F:\\r.python\\r_workingdirectory\\data\\CaliforniaHospital_FinancialData.txt",header=T,sep='\t')
names(data)
str(data)



data1=data[,c('QRT','TOT_OP_EXP')]
data1



data_ts=ts(data1$TOT_OP_EXP,frequency=4)
data_ts


plot(data_ts,main='total operating expenses')

data_dc=decompose(data_ts)
plot(data_dc)


layout(1:3)
plot(data_ts)
plot(aggregate(data_ts))
boxplot(data_ts~cycle(data_ts))



sd(data_ts)
sd(data_ts-data_dc$seasonal)

# data has both seasonal and trend components thus conducting Holt Winters model

data_trial_hw=HoltWinters(data_ts,alpha= 0.6, beta=0.0001,gamma=0.5)
data_trial_hw
plot(data_trial_hw)
data_fore1=forecast.HoltWinters(data_trial_hw,h=40)
plot(data_fore1)

data_hw=HoltWinters(data_ts)
data_hw
plot(data_hw)
data_fore2=forecast.HoltWinters(data_hw,h=40)
data_fore2
plot(data_fore2)


layout(1:2)
plot(data_fore1$residuals, main='residuals for imposed Holtwinters forecast')
lines(c(0,25),c(0,0),col='red')
plot(data_fore2$residuals, main='residuals for R-predicted Holtwinters forecast')
lines(c(0,25),c(0,0),col='red')


layout(1:2)
plotForecastErrors(data_fore1$residuals,'normality assesment for imposed Holtwinters forecast')
plotForecastErrors(data_fore2$residuals,'normality assesment for R-predicted Holtwinters forecast')





































