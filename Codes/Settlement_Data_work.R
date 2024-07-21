library(xlsx)
library(ggplot2)
library(tseries)
library(zoo)
library(xts)
?zoo
settle1<-read.xlsx(file="C:\\Users\\Debaditya\\Documents\\Project_Work\\Settle_Data_Full_Final.xlsx"
                   ,sheetIndex = 1,as.data.frame = 1)
dat1<-ts(as.numeric(settle1$UPI_Vol),frequency = 365,start = c(2020,152))
#1313to1370 is 1st jan 2024 to 1sat march 2024
temp23<-decompose(dat1)
plot(temp23)
dat3<-xts(dat1[366:396],order.by = seq(as.Date("2021-06-01"), by ="days" , length=30))
dat1
x1<-decompose(ts(dat1[(1313-31):1370],frequency=7 ))
plot(pacf(ts(dat1[(1313-31):1370],frequency=7 ),lag.max = 100))
plot(x1)
plot(y=dat1[(1313-31):1370],x=seq(as.Date("2023-12-01"),by="days",length=(1370-(1313-31))+1),type="l")
plot(dat1[1300:1390],type="l")
acf(dat1,lag.max = 300)
dat2<-diff(dat1,lag = 11,differences = 1)
acf(dat2,lag.max = 100)
adf.test(dat2)
adf.test(dat1)
plot(dat2)
m<-decompose(dat3)
m$seasonal
dat3
plot(dat3)
plot(m)
plot(dat1[61:91],type = "l")
