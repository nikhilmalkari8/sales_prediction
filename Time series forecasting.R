#-----------------------------------------Time Series Forecasting---------------------------------------#

#Analytical Problem: To forecast the Sales for the next 36 months

#----------------------------------Preparing the environment--------------------------------------------#

list.of.packages <- c("forecast","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(forecast)
library(tseries)
kpss.test

#---------------Setting working directory---------------------------------------------------------------#

Path<-("E:/Ivyproschool/R/Time Series Forecasting/Case Study 2")
setwd(Path)
getwd()

data<-read.csv("1sales.csv")
data1=data

#-------------------Exploration of data-------------------------------------------#

colSums(is.na(data))
class(data)
#---------------------Transformation of the date data into time series------------------------------------#

data=ts(data[,2],start=c(2003,1),frequency=12)
class(data)
start(data)
end(data)
frequency(data)
str(data)
data
#--------------------->plotting the sales
plot(data,ylab="Sales", xlab="Year",main="Sales between 2003-2017",col="orange")
abline(reg = lm(data~time(data)))
cycle(data)
plot(aggregate(data,FUN=mean))
#This plot displays the year on year trend in the sales from 2003-2017

#--------------->Differencing the data to remove trend and drift

plot(log10(data),ylab="log(Sales)",xlab="Year",main="log(Sales) between 2003-2017",col="orange")
##Differencing the data to remove trend
plot(diff(data,differences = 2),ylab="Diff(Sales)",xlab="Year",main="Diff(Sales) between 2003-2017",col="orange")

#The differenced data continues to have unequal variance
plot(diff(log10(data),differences = 2),ylab="Diff(Sales)",xlab="Year",main="Diff(Log(Sales) between 2003-2017",col="orange")
#So, with Log10 and 2 order of differencing makes the series stationary

#----------------->Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)
#For differencing=1
LDdata=diff(log10(data),differences = 1)
adf.test(LDdata,alternative="stationary")
#p-value=0.206, which is greater than 0.05 then we accept the null hypothesis that the series is Non-stationary
#For differencing=2
LDdata=diff(log10(data),differences = 2)
adf.test(LDdata,alternative="stationary")
#Since, the p-value <0.05, hence, we reject the Ho: Series is Non-Stationary

#creating the ACF and PACF plot
par(mfrow=c(1,2))
acf(diff(log10(data)),main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(diff(log10(data)),main="PACF plot")#PACF PLOT -- Auto Regressive or p


#Running the ARIMA model-R, gives the best model fit
require(forecast)
ARIMAFit1=auto.arima(log10(data),approximation=TRUE,trace=TRUE)
summary(ARIMAFit1)
ARIMAFit1$residuals

#Predicting the future values
pred=predict(ARIMAFit1,n.ahead=36)
pred
##Ploting the observed data and forecasted data together
par(mfrow=c(1,1))
plot(data,type="l",xlim=c(2003,2020),ylim=c(1,150000),xlab="Year",ylab="Sales")
lines(10^(pred$pred),col="red")

#plotting the +-1 standard error to range of expected error
plot(data,type="l",xlim=c(2003,2020),ylim=c(1,150000),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+1*pred$se),col="blue")
lines(10^(pred$pred-1*pred$se),col="grey")
## then forecast the result
pred = predict(ARIMAFit1, n.ahead = 36)
pred=as.data.frame(pred)
write.csv(pred,"predict.csv")

## then do the exponential since you had used log earlier.
normal_result=10^pred$pred ## you get the desired result.
class(normal_result)
normal_result_df<-as.data.frame(normal_result)
Date_Pred_seq<-NULL
Date_Pred_seq<-as.data.frame(seq(as.Date("2017/02/01"),as.Date("2020/01/01"),by = "month"))
final_result_df = cbind(Date_Pred_seq,normal_result_df)
library(dplyr)
colnames(final_result_df)<-c("Date","Sales_Predicted")
write.csv(final_result_df,"finalpredicted.csv", row.names = FALSE)

plot(normal_result)

























