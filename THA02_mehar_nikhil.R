workdir=("F:\\r.python\\r_workingdirectory\\data")
setwd(workdir)

hosp_data=read.csv("F:\\r.python\\r_workingdirectory\\data\\CaliforniaHospitalData.csv",header=T,sep=",")
emp_data=read.table("F:\\r.python\\r_workingdirectory\\data\\CaliforniaHospitalData_Personnel.txt",header=T,sep="\t")

hosp_data
emp_data
hos_data=merge(hosp_data,emp_data,by="HospitalID")
hos_data
str(hos_data)



newrow=c(1:23)
hos_data=rbind(hos_data,newrow)

hos_data1=edit(hos_data)
hos_data1

str(hos_data1)
startdate=strptime(as.character(hos_data1$StartDate),"%Y-%m-%d %H:%M:%S.0000000")
startdate=as.data.frame(startdate)
hos_data1=data.frame(hos_data1,startdate)


hos_data2=subset(hos_data1,select=-c(HospitalID,Website,Zip))
names(hos_data2)

summary(hos_data2)

write.table(hos_data2,file="mehar_nikhil_export.txt",sep="\t",row.names=FALSE,qmethod="escape")

data=read.table("F:\\r.python\\r_workingdirectory\\data\\mehar_nikhil_export.txt",header=T,sep="\t")
data

str(data)
summary(hos_data2[,c("Name","LastName","FirstName")])
summary(hos_data2[,c("TypeControl","Teaching","DonorType","Gender","PositionTitle")])

hist(data$NoFTE,main="NoFTE histogram")
hist(data$InOperExp,main="InOperExp histogram")
hist(data$OutPerExp,main="OutPerExp histogram")
hist(data$OperRev,main="OperRev histogram")
hist(data$OperInc,main="OperInc histogram")
hist(data$AvlBeds,main="AvlBeds histogram")
hist(data$NetPatRev,main="NetPatRev histogram")

plot(data$NoFTE,data$NetPatRev)
plot(data$InOperExp,data$NetPatRev)
plot(data$OutPerExp,data$NetPatRev)
plot(data$OperRev,data$NetPatRev)
plot(data$OperInc,data$NetPatRev)
plot(data$AvlBeds,data$NetPatRev)

boxplot(data$NoFTE,main="NoFTE boxplot")
boxplot(data$InOperExP,main="InOperExp boxplot")
boxplot(data$OutPerExp,main="OutPerExp boxplot")
boxplot(data$OperRev,main="OperRev boxplot")
boxplot(data$OperInc,main="OperInc boxplot")
boxplot(data$AvlBeds,main="AvlBeds boxplot")
boxplot(data$NetPatRev,main="NetPatRev boxplot")

qqnorm(data$NoFTE,main="NoFTE")
qqline(data$NoFTE,lty=2)

qqnorm(data$InOperExp,main="InOperExp")
qqline(data$InOperExp,lty=2)

qqnorm(data$OutPerExp,main="OutPerExp")
qqline(data$OutPerExp,lty=2)

qqnorm(data$OperRev,main="OperRev")
qqline(data$OperRev,lty=2)

qqnorm(data$OperInc,main="OperInc)
qqline(data$OperInc,lty=2)

qqnorm(data$AvlBeds,main="AvlBeds")
qqline(data$AvlBeds,lty=2)

qqnorm(data$NetPatRev,main="NetPatRev")
qqline(data$NetPatRev,lty=2)

shapiro.test(data$NoFTE)
shapiro.test(data$InOperExp)
shapiro.test(data$OutPerExp)
shapiro.test(data$OperRev)
shapiro.test(data$OperInc)
shapiro.test(data$AvlBeds)
shapiro.test(data$NetPatRev)


barplot(data$NetPatRev, main="NetPatRev barchart")

library(MASS)
G=data$Gender
G.freq=table(G)


barplot(G.freq,main="Gender distribution",ylab="Gender",horiz=T)

m=data$MaxTerm
m.freq=table(m)

barplot(m.freq,main="Maxterm distribution",,ylab="maxterm",horiz=T)

t=data$TypeControl
t.freq=table(t)

barplot(t.freq,main="Typecontrol distribution")

dt=data$DonorType
dt.freq=table(dt)

barplot(dt.freq,main="Donor type distribution")




