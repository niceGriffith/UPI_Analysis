#Loading Essential Libraries####
library(xlsx)
library(ggplot2)
library(tseries)
library(ggspectra)
library(forecast)
library(dplyr)
library(ggthemes)
install.packages("ggspectra")
#loading the dataset####
upi<-read.xlsx(file = "C:\\Users\\Debaditya\\Documents\\Project_Work\\UPI_Analytics.xlsx" ,sheetIndex = 1)
#converting the dataset to a dataframe for ease#### 
upi$Month->x
upi<-upi[,-1]
upi<-as.data.frame(upi)
rownames(upi)<-x
upi
colnames(upi)<-c("No Of banks live on UPI","Volume(In Mn)","Value(In Cr)","Volume(In Cr)")
#reversing the oreer of rows since we want the data to begin from 2016-04####
data1<-as.data.frame(t(rev(as.data.frame(t(upi)))))
#plotting the data####
ggplot(upi)+
  geom_line(data = upi,mapping = aes(x=as.Date(rownames(upi)),y=upi$`Value(In Cr)`),color="blue",linewidth=1)+
  geom_line(data = upi,mapping = aes(x=as.Date(rownames(upi)),y=ma(upi$`Value(In Cr)`,6)),color="red",linewidth=1)+
  theme_bw()
#converting the data to a time series object####
data2<-ts(data=as.numeric(data1$`Value(In Cr)`),start = c(2016,4),end=c(2024,2),deltat = 1/12)
data3<-ts(data=as.numeric(data1$`Volume(In Mn)`),start = c(2016,4),end=c(2024,2),deltat = 1/12)
#Calculating growth rate####
month_growth<-
  function(data,returndat)
  {
    returndat[1]=0;
    for(i in 2:length(data)){
      if(data[i-1]>0)
      {
        returndat[i]=(((data[i]-data[i-1])/data[i-1])*100)
      }
      else if(data[i-1]==0)
        returndat[i]=0
    }
    returndat
  }
dat<-list()
growth<-month_growth(as.numeric(data1$`Value(In Cr)`),dat)
growth1<-ts(as.numeric(growth[-10:-1]),deltat = 1/12,start = c(2017,2))
plot(growth,type="l",)
growth1
g<-decompose(growth1)
plot(g)
#Decomposing the different components of the time series####
Value_Decomp<-stl(data2,s.window = "periodic")
Vol_Decomp<-decompose(data3,type = "additive")
plot(Value_Decomp,col="Red")
Value_Decomp
autoplot(data)
#Inflation Adjustment####
##Inflation Data####
cpi<-c(127.3,128.6,130.1,131.1,131.1,130.9,131.4,131.2,130.4,130.3,130.6,130.9,
       131.1,131.4,132.0,134.2,135.4,135.2,136.1,137.6,137.2,136.9,136.4,136.5,
       137.1,137.8,138.5,139.8,140.4,140.2,140.7,140.8,140.1,139.6,139.9,140.4,
       141.2,142.0,142.9,144.2,145.0,145.8,147.2,148.6,150.4,150.2,149.1,148.6,
       151.4,150.9,151.8,153.9,154.7,156.4,158.4,158.9,157.3,156.3,156.6,156.8,
       157.8,160.4,161.3,162.5,162.9,163.2,165.5,166.7,166.2,165.7,166.1,167.7,
       170.1,171.7,172.6,173.4,174.3,175.3,176.7,176.5,175.7,176.5,176.8,177.2,
       178.1,179.1,181.0,186.3,186.2,184.1,185.3,186.3,185.7,185.5,189.5)
length(cpi)
cpi<-ts(cpi,start = c(2016,4),end = c(2024,2),frequency = 12)
rm(k)
k<-ts(c(data2,cpi),start =start(data2),frequency  =frequency(data2) )
k
autoplot(cpi,series = "orange",main="Consumer Price Index Number ",ylab = "CPI",size=2)+
                   labs(subtitle = "Base 2012(=100)")+
                theme_economist()
###Deflation####
deflat<-(data2/cpi)*100
autoplot(deflat,series = "Deflated Value",main="UPI total transaction Amount deflated vs Original",
         ylab = "UPI transaction Value(IN Cr.)",size=2)+
  labs(subtitle = "Base CPI 2012(=100)")+geom_line(mapping = aes(y = data2,color="Original Value"),linewidth=2)+
  theme_economist()
#Fitting Curves in the dataset####

