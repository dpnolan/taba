# Peter Nolan x22154116
# 
# Code for Terminal Assessment by Assignment (TABA)
# 
# See Google Colab workbook and all the working files at 
# 
# Github https://github.com/dpnolan/taba 
#
# Contents 
# A Time Series Analysis 
# A.1 Load, preprocess and describe the data
# A.2 Use ETS to automatically fit and evaluate an exponential smoothing model
# A.2.2 Estimation
# A.2.3 Diagnosis 
# A2.4 Forecasts
#
# A3 - SARIMA analysis 
# A.3.1 Estimate the orders of integration
# A 3.2 Model estimation
# A.3.3 Forecasts

# A 4 - Simple time series models
# A 4.1 - Simple time series models
# A 4.2 - Simple decomposition

####################
# Section A.1 - Load, preprocess and describe the data
####################

getwd()
setwd("/Users/peternolan/Documents/Post Grad/NCI/Courses/Statistics/TABA/")
list.files()

# Install the tseries library, so that we can use the 'ts' object for time series data
install.packages('tseries')
library('tseries')
library(fpp2)

# Upload the file 
departs<-read.csv('Departure.csv')

# We will use only the time series object from the Rstat tseries
# This combines an observation in double float point format 
# with its datetime, the month+year in our case

# Convert to TS object 
ts_departs<-ts(departs$departures..000,start=c(2010,1),frequency=12)

#Print out the data for visual inspection
ts_departs

length(ts_departs)
# 153 observations
ts_departs<-(na.omit(ts_departs))
# Remove any NA values
length(ts_departs)
# Still 153 observations

# Check the basic definition terms are correct
start(ts_departs)
# Jan 2010, as in the original file 
end(ts_departs)
# Sept 2022, as in the original file
frequency(ts_departs)
# 12, so monthly, or 12 times per year

library('ggplot2')
library('zoo')
library(gcookbook)

# Graph the full time series 
ggplot(ts_departs, aes(x=as.yearmon(index(ts_departs)),y = ts_departs))+
  geom_line(colour='blue')+
  geom_point(size=1,colour='blue') + 
  ylab("Departures 000's") + xlab('Date')

# vertical line to show pandemic decrease   
#  geom_vline(xintercept = 2020.25,colour='red',size=1.5,alpha=0.5)  
#  scale_y_continuous(breaks =  seq(2010,2022))
#  geom_area(fill = "blue", alpha = .2, ymin=2020)#
#  Graph uses the yearmon data type from zoo library

# Data summary - 
# The top chart shows a pronounced, strong growth trend
# 1. Consistent upward trend in maximum and min for each series from 2013 to 2020
# 2. Clear seasonality with peaks in the summer months
# 3. Pandemic effect is clear in the rapid fall in Q1 of 2020 and slow recovery
# 4. Recovery in 2021 and 2022 brings the annual peak close to the 2019 numbers

# Descriptive statistics
# mean, median, quantiles, max and min values
summary(ts_departs)
# Max value is ?
max(ts_departs)
# When was the max? 
which.max(ts_departs)
index(ts_departs)[115]
# July 2019

# Min value is ?
min(ts_departs)
# When? 
which.min(ts_departs)
index(ts_departs)[124]
# April 2020

#Mean, skew and kurtosis
mean(ts_departs)
library('moments')
skewness(ts_departs)
kurtosis(ts_departs)

library('scales')

# Generate the histogram of departures
ggplot(ts_departs, aes(x = ts_departs,y=..density..)) + 
  geom_histogram(binwidth=50, fill = "blue", colour = "black", alpha=0.3)+
  geom_density(colour='blue', linewidth=1.5)+
  xlab('Departures 000s') +
  scale_y_continuous(breaks=c(0.001,0.0005,0),labels=percent)

#scale_y_discrete(labels=percent,breaks=c(0.001,0.0005,0))
  # xticks('')  
  # geom_vline(xintercept = 1088.6 ,colour='red',size=1.5,alpha=0.5)+
 
# Window the data by year for the boxplot
d2010<-window(ts_departs,start=2010, end=2010.99)
d2011<-window(ts_departs,start=2011, end=2011.99)
d2012<-window(ts_departs,start=2012, end=2012.99)
d2013<-window(ts_departs,start=2013, end=2013.99)
d2014<-window(ts_departs,start=2014, end=2014.99)
d2015<-window(ts_departs,start=2014, end=2015.99)
d2016<-window(ts_departs,start=2016, end=2016.99)
d2017<-window(ts_departs,start=2017, end=2017.99)
d2018<-window(ts_departs,start=2018, end=2018.99)
d2019<-window(ts_departs,start=2019, end=2019.99)
d2020<-window(ts_departs,start=2020, end=2020.99)
d2021<-window(ts_departs,start=2021, end=2021.99)
d2022<-window(ts_departs,start=2022, end=2022.99)

# Generate a boxplot with these

boxplot(d2010,d2011,d2012,d2013,d2014,d2015,d2016,d2017,d2018,d2019,d2020,d2021,d2022, 
        names=seq(2010,2022), ylab='Departures 000s',col='blue')

# T-tests
Does the mean match in the two yearly buckets?  
#2013 v 2020
(t.test(d2013,d2019,alternative='two.sided',var.equal=FALSE))['p.value']
# p.value=7.696756e-05, so REJECT null hypothesis of equal means

#2019 v 2020
t.test(d2019,d2020,alternative='two.sided',var.equal=FALSE)['p.value']
# p.value=6.286207e-08, so REJECT null hypothesis of equal means

#2020 v 2022
t.test(d2020,d2022,alternative='two.sided',var.equal=FALSE)['p.value']
# p.value=6.286207e-08, so REJECT null hypothesis of equal means

# Some plots of sub-periods of our time series
plot(d2020, main = 'Departures 2020')
plot(d2021, main = 'Departures 2021')
plot(d2022, main = 'Departures 2022')

pandemic_departs=window(ts_departs,start=c(2019,1),end=c(2022,9))
plot(pandemic_departs,ylab=('Departures 000s'),main='Departures through the Pandemic period and after')

#Seasonality
# Plot the seasonality - monthly range
# Horizontal bars are the means of each month
# Graph shows strong summer seasonality
monthplot(ts_departs)


# Plot the seasonality
library('fpp2')
ggs<-ggseasonplot(ts_departs,year.labels=TRUE, year.labels.left=TRUE) + ylab('1,000') 
ggs + ggtitle("Seasonal plot: departures from Irish airports")
# Clear and consistent seasonality every year apart from the collapse
# in 2020 and the progressive growth in 2021
# Timing, with the collapse dating to February

# How will we select the sample for estimation/  
# Since the start and end of the COVID pandemic was an extraordinary once in a century event
# with only one complete example to capture, we can remove it to
# discover more generalisable analysis from the data

#Filtered - the time series before the pandemic
ts_departs_filtered<-window(ts_departs, start = c(2010,1), end = c(2020,2))
ts_departs_filtered

# Train and test datasets
# Objective is to 'forecast the departures in the first 6 months of 2021'
# This can be our test dataset to evaluate the models 
# The data before then would be the training data,  
# to be used for estimating and validating the model 
ts_departs_train<-window(ts_departs, start = c(2010,1), end = c(2020,12))
ts_departs_test<-window(ts_departs, start = c(2021,1), end = c(2021,6))
ts_departs_train

plot(ts_departs_train,ylab=('Departures 000s'),main='Departures: training data set')
plot(ts_departs_test,ylab=('Departures 000s'),main='Departures: testing data set')

#seasonplot(ts_departs_filtered)
# same hump pattern in every year=
ggs<-ggseasonplot(ts_departs,year.labels=TRUE, year.labels.left=TRUE) 
ggs + ylab('1,000') 
ggs + ggtitle("Seasonal plot: departures from Irish airports")

monthplot(ts_departs_filtered)
# Month by month view confirms this


####################
# Section A.2 Use ETS to automatically fit 
# and evaluate an exponential smoothing model
####################

####################
# A.2.2 Estimation
####################

departs_fit1<-ets(y=ts_departs_train,model='ZZZ',damped=TRUE)
departs_fit1
summary(departs_fit1)
plot(departs_fit1)

# alpha = 0.9999  smoothing parameter for the level, the smoothed output
# beta  = 0.1413  the same for the slope ('trend') component
# gamma = 1e-04  smoothing parameter for the seasonality
# phi   = 0.8  component for the damping

#Initial states:
# l = 962.4632 
# b = 4.7834 
# s =  -208.2974 -202.7917 72.2755 130.9762 307.6094 349.7194
# 233.3971 73.3278 -65.9956 -144.2698 -278.4757 -267.4752
# 
# AIC          AICc        BIC 
# 1897.832     1903.886    1949.723

# RMSE = 100.4841

# The 3 components, error, trend and seasonal, are all 
# additive and not multiplicative

plot(departs_fit1)
autoplot(departs_fit1, main='departs_fit1 on the training data')

# departs_fit2
# Specify the Additive error, trend and seasonality
departs_fit2<-ets(ts_departs_train,model="AAA",use.initial.values = TRUE)
departs_fit2
summary(departs_fit2) 

  # shows parameters and initial states are the same 

# AIC       AICc      BIC 
# 1897.832 1903.886   1949.723 
# plus RMSE 
# 100.5841

plot(departs_fit2)

# TS model on the same dataset, selected as all arithmetic
# gives the same parameters and AIC, AICc and BIC as departs_fit1


# departs_fit3, same as fit1 but without damping component
departs_fit3<-hw(ts_departs_train,damped=FALSE,seasonal="additive")
departs_fit3
summary(departs_fit3)

# AIC         AICc        BIC 
# 1905.646    1911.014    1954.653 

# RMSE = 109.567

# Evaluation scores are all worse than for the model with damping


####################
# A.2.3 Diagnosis 
####################

checkresiduals(departs_fit1)
# AR order 1 showing in ACF
# ACF shows no other significant correlations
# 
# Ljung-Box test
#
# data:  Residuals from ETS(A,Ad,A)
# Q* = 22.878, df = 7, p-value = 0.00179
# Low probability score means rejects null hypothesis of independence in the time series residuals

Box.test(x = residuals(departs_fit1),type='Ljung-Box')
# Box-Ljung test
#
# data:  residuals(departs_fit1)
# X-squared = 10.285, df = 1, p-value = 0.001341
# rejects null hypothesis of independence in the time series residuals

residuals1=residuals(departs_fit1)
qqnorm(residuals(departs_fit1)); qqline(residuals(departs_fit1))
jarque.bera.test(residuals1)
#Jarque Bera Test
#data:  residuals1
#X-squared = 3370.8, df = 2, p-value < 2.2e-16

checkresiduals(departs_fit3)
#QQ plot and JB indicate residuals aren't normal
#not white noise, p-value near zero 

# Map residuals to forecast errors
cbind("Residuals"=residuals(departs_fit1),
      'Forecast errors' = 
        residuals(departs_fit1,type='response')) %>% 
  autoplot(facet=TRUE)

#decomposition for departs_fits2
autoplot(ts_departs)+
  autoplot(departs_fit2,series='HW additive forecasts', PI=FALSE) +
  xlab('Year')+
  ylab('Departures 000s')+
  guides(colour=guide_legend(title='Forecast'))


####################
# A2.4 Forecasts
####################

ts_departs_test
#       Jan   Feb   Mar   Apr   May   Jun
# 2021 104.1  46.7  57.5  61.5  82.4 174.7


mean(ts_departs_test) # mean = 87.81667
sd(ts_departs_test) # sd = 47.21324

forecast(departs_fit1,h=6) # 
#             Point Forecast     Lo 80     Hi 80      Lo 95     Hi 95
#Jan 2021       120.1400 -17.96313  258.2432  -91.07053  331.3506
#Feb 2021       127.7165 -78.91624  334.3493 -188.30103  443.7341
#Apr 2021       366.9147  48.54473  685.2846 -119.99014  853.8195
#Mar 2021       276.7834  11.60873  541.9580 -128.76624  682.3329
#May 2021       515.7952 147.98794  883.6024  -46.71744 1078.3078
#Jun 2021       683.4642 269.21589 1097.7125   49.92609 1317.0023

accuracy(departs_fit1,h=6)
#                   ME     RMSE     MAE       MPE     MAPE      MASE      ACF1
# Training set -2.425368 100.5841 52.8242 -30.07175 41.14341 0.2738436 0.2759947

# actual minus forecast
ts_departs_test-forecast(departs_fit1,h=6)$mean # y - y^hat
#           Jan        Feb        Mar        Apr        May        Jun
# 2021  -16.04004  -81.01654 -219.28335 -305.41467 -433.39517 -508.76419

mean(ts_departs_test-forecast(departs_fit1,h=6)$mean) 
# mean difference is -260.6523

# standard deviation of forecast
sd((forecast(departs_fit1,h=6))$mean) # = 222.0652

plot(forecast(departs_fit1,h=6)) #mean around 1500, with damping in the model


forecast(departs_fit3,h=6) #
ts_departs_test-forecast(departs_fit3,h=6)$mean
# Jan        Feb        Mar        Apr        May        Jun
# 2021   18.00152  -20.29705 -135.14903 -200.54848 -309.07076 -367.66809

mean(ts_departs_test-forecast(departs_fit3,h=6)$mean) = -169.112

sd(forecast(departs_fit3,h=6)$mean) #= 183.8436

accuracy(departs_fit3,h=6) #RMSE = 104.3934
#                   ME     RMSE      MAE       MPE    MAPE      MASE     ACF1
# Training set -9.089778 104.3934 53.82661 -40.31534 50.4259 0.2790401 0.375125


#######################
# Section A3 - SARIMA analysis 
#######################
#Plotting the data
#Take first differences of the data until it is stationary
#Transform the data if necessary to stabilise the variance
#Examine the ACF/PACF plots to identify possible models.
#Fit the models
#Use the AICc statistic to search for the best models 
#Check residuals by plotting their ACF and do a Box Ljung test
#Once residuals look like white noise, calculate the forecast values

# Plot the time series and its decomposition
plot(ts_departs_train)
plot(decompose(ts_departs))
# Seasonality with annual lag is obvious

#######################
# A.3.1 Estimate the orders of integration
#######################

ndiffs(ts_departs)
# d = 0 for the whole time series

# Training data order of integration
ndiffs(ts_departs_train)
# d = 1
nsdiffs(ts_departs_train)
# D =  1 for seasonal terms

#So, take first differences
diff_ts_departs_train<-diff(ts_departs_train)

#Test this first differences series for stationarity with Augmented Dickey Fuller 
adf.test(diff_ts_departs_train)

#Augmented Dickey-Fuller Test
#
#data:  diff_ts_departs_train
#alternative hypothesis: stationary
#Dickey-Fuller = -7.2297, Lag order = 5, p-value = 0.01
#
# So, we accept the alternative hypothesis of stationarity in the first diff series

# Graph the full time series 
# Plot one on top of the other using patchwork for ggplot2 graphs

library(patchwork)

p1<-ggplot(ts_departs_train, aes(x=as.yearmon(index(ts_departs_train)),y = ts_departs_train))+
  geom_line(colour='blue')+
  ylab("Departures 000's") + xlab('Date')

p2<-ggplot(diff_ts_departs_train, aes(x=as.yearmon(index(diff_ts_departs_train)),
                                  y = diff_ts_departs_train))+
  geom_line(colour='blue')+
  ylab("Departures 000's") + xlab('Date')

p1 / p2 # Graph ts_departs_train dataset and its first differences, one on top of the other

# KPSS, ADF and PP tests show that the first differences series has integration order zero
ndiffs(ts_departs_train,test='kpss') #1 
ndiffs(diff_ts_departs_train,test='kpss') #0

ndiffs(ts_departs_train,test='adf') #0
ndiffs(diff_ts_departs_train,test='adf') #0

ndiffs(ts_departs_train,test='pp') #1
ndiffs(diff_ts_departs_train,test='pp') #0

# Graph the Autocorrelation and Partial Autocorrelation Functions (PACF)
Acf(ts_departs_train)
Pacf(ts_departs_train)

# Acf shows significant positive correlation on lags 1 to 5 in the training data set 
# and slow decline, then significant around lag 12, indicating seasonality
#
# Pacf shows spikes at 1 and at 2 also, supporting view that there is AR
# Pacf reverses, so possibly some MA effects also, but not strongly indicated in graph

#######################
# A3.2 Model estimation
#######################

# Run auto.arima to devise a model
# stepwise = FALSE to do a comprehensive slow search
sax<-auto.arima(ts_departs_train,seasonal=TRUE,stepwise=FALSE,
                ic='aicc',start.p=1,start.q=1)
summary(sax)
# integrated of order 1
# AR(1) and seasonal MA(1)
# ARIMA(1,1,0)(0,1,1)[12] 
# RMSE = 100.1111 
# AIC=1466.16   AICc=1466.37   BIC=1474.5

# check manually possible alternatives
sa1<-auto.arima(ts_departs_train,seasonal=TRUE)
summary(sa1)
# order is ARIMA(1,1,0)(0,1,1)[12] 
# seasonal is (0,1,1)[12]
# RMSE = 100.1111 
# AIC=1466.16   AICc=1466.37   BIC=1474.5
# so, same model, same performance scores

sa2 <-Arima(ts_departs_train,order=c(2,0,0),seasonal=c(2,1,0))
summary(sa2)
# RMSE=101.8324 
# AIC=1481.63   AICc=1482.15   BIC=1495.57
# Somewhat worse scores

sa3 <-Arima(ts_departs,order=c(1,0,0),seasonal=c(0,1,1))
summary(sa3)
# AIC=1770.68   AICc=1770.86   BIC=1779.53
# RMSE = 108.5619
# Much worse scores than our first SAX autofitted model

sa4 <-Arima(ts_departs,order=c(1,1,0),seasonal=c(0,1,1))
summary(sa4)
# RMSE = 137.1179
# AIC=1808.88   AICc=1809.05   BIC=1817.72
# much worse than SAX

sa5 <-Arima(ts_departs,order=c(1,1,0),seasonal=c(1,0,0))
summary(sa5)
# RSME = 120.714
# AIC=1899.88   AICc=1900.04   BIC=1908.95
# Much, much worse than SAX 

# Plot the residuals from the automatically derived model, sax
# AR, MA unlikely, nothing is significant in ACF 
residuals(sax)
checkresiduals(sax,plot=TRUE)
# Result is that null hypothesis of independence is upheld
# Residuals from model 'sax' ARIMA(2,0,0)(0,1,1)[12]Ljung-Box test
#
# Ljung-Box test
# data:  Residuals from ARIMA(1,1,0)(0,1,1)[12]
# Q* = 5.235, df = 22, p-value = 0.9999
#
# Model df: 2.   Total lags used: 24
# Box tests, high prob so we can  uphold the null hypothesis of independence 

Box.test(x = residuals(sax),type='Ljung-Box')
# data:  residuals(sax)
# X-squared = 0.00067123, df = 1, p-value = 0.9793

# Box tests, all uphold the null hypothesis of independence 
Box.test(x = residuals(sax),type='Ljung-Box') # 0.9793
Box.test(x = residuals(sax),type='Ljung-Box',lag=1) # p = 0.9793
Box.test(x = residuals(sax),type='Ljung-Box',lag=2) # p = 0.9229
Box.test(x = residuals(sax),type='Ljung-Box',lag=3) # p = 0.9384
Box.test(x = residuals(sax),type='Ljung-Box',lag=4) # p = 0.9695
Box.test(x = residuals(sax),type='Ljung-Box',lag=5) # p = 0.9771
Box.test(x = residuals(sax),type='Ljung-Box',lag=6) # p = 0.9921
Box.test(x = residuals(sax),type='Ljung-Box',lag=7) # p = 0.981
Box.test(x = residuals(sax),type='Ljung-Box',lag=8) # p = 0.9002
Box.test(x = residuals(sax),type='Ljung-Box',lag=9) # p = 0.938
Box.test(x = residuals(sax),type='Ljung-Box',lag=10)# p = 0.9647
Box.test(x = residuals(sax),type='Ljung-Box',lag=11)# p = 0.974
Box.test(x = residuals(sax),type='Ljung-Box',lag=12)# p = 0.9858
Box.test(x = residuals(sax),type='Ljung-Box',lag=13)# p = 0.9925
Box.test(x = residuals(sax),type='Ljung-Box',lag=14)# p = 0.9962

#######################
# Section A.3.3 Forecasts
#######################

# This forecasts a pattern showing 2023 with the same max and min and seasonality as 2016
# This is probably realistic, given the likely downturn and the continuing seasonality we see
# as we continue to recover from the pandemic's economic effects
# 95% confidence interval goes from 439 to 2363, a very wide range for uncertainty.  

ts_departs_test
#       Jan   Feb   Mar   Apr   May   Jun
# 2021 104.1  46.7  57.5  61.5  82.4 174.7

mean(ts_departs_test) # 87.81
sd(ts_departs_test) # 47.21

forecast(sax,h=6)
# show the values
# Negative values aren't valid 
#
#         Point Forecast      Lo 80    Hi 80     Lo 95     Hi 95
# Jan 2021      104.45103  -32.29759 241.1996 -104.6879  313.5900
# Feb 2021       94.42007 -140.31920 329.1593 -264.5827  453.4228
# Mar 2021      168.66266 -147.82503 485.1504 -315.3635  652.6888
# Apr 2021      181.99916 -203.89686 567.8952 -408.1779  772.1762
# May 2021      312.76668 -133.51826 759.0516 -369.7672  995.3006
# Jun 2021      466.67683  -33.35034 966.7040 -298.0487 1231.4024

# Forecast errors 
ts_departs_test - forecast(sax,h=6)$mean
#            Jan          Feb          Mar          Apr          May          Jun
# 2021   -0.3510287  -47.7200719 -111.1626605 -120.4991608 -230.3666766 -291.9768292

# Mean forecast error
mean(ts_departs_test - forecast(sax,h=6)$mean) # =-133.6794

# Mean forecast
mean(forecast(sax,h=6)$mean) # = 221.496
# SD of forecast
sd(forecast(sax,h=6)$mean) # 143.2782

sax %>%forecast(h=6) %>% autoplot()

accuracy(sax,h=6)
#                 ME     RMSE      MAE       MPE     MAPE     MASE        ACF1
# Training set -4.243692 100.1111 50.25274 -18.05206 35.48415 0.260513 -0.00222962


####################
# Section 4 - Simple time series models
####################

####################
# Section 4.1 - Simple time series models
####################
# Following Hyndman and Athanasopoulos (2018), chapter 6.2
# Fit and forecast on the training data using the Mean, Naive, Seasonal naive, Drift methods

g41<-autoplot(ts_departs_train) +
  autolayer(ts_departs_test,series="Actual", PI=FALSE) +
  autolayer(meanf(ts_departs_train,h=6),series="Mean", PI=FALSE) +
  autolayer(naive(ts_departs_train,h=6),series="Naive", PI=FALSE) +
# autolayer(snaive(ts_departs_train,h=6),series="Seasonal naive", PI=FALSE) +
# autolayer(rwf(ts_departs_train,drift=TRUE,h=6),series="Drift",PI=FALSE)+
  ggtitle("Forecasts for departures H1 2021") +
  xlab("Year") + ylab('Departures 000s')

g42<-autoplot(ts_departs_train) +
  autolayer(ts_departs_test,series="Actual", PI=FALSE) +
  #autolayer(meanf(ts_departs_train,h=6),series="Mean", PI=FALSE) +
  #autolayer(naive(ts_departs_train,h=6),series="Naive", PI=FALSE) +
  autolayer(snaive(ts_departs_train,h=6),series="Seasonal naive", PI=FALSE) +
  autolayer(rwf(ts_departs_train,drift=TRUE,h=6),series="Drift",PI=FALSE)+
  ggtitle("Forecasts for departures H1 2021")+
  xlab("Year") + ylab('Departures 000s')

g41 / g42 # plot one over the other 


###################
# Testing meanf()

ts_departs_test 
#     Jan   Feb   Mar   Apr   May   Jun
#2021 104.1  46.7  57.5  61.5  82.4 174.7

meanf(ts_departs_train,h=6)
#        Point Forecast    Lo 80   Hi 80    Lo 95    Hi 95
#Jan 2021       1136.176 586.3412 1686.01 291.7162 1980.635
#Feb 2021       1136.176 586.3412 1686.01 291.7162 1980.635
#Mar 2021       1136.176 586.3412 1686.01 291.7162 1980.635
#Apr 2021       1136.176 586.3412 1686.01 291.7162 1980.635
#May 2021       1136.176 586.3412 1686.01 291.7162 1980.635
#Jun 2021       1136.176 586.3412 1686.01 291.7162 1980.635

sd(meanf(ts_departs_train,h=6)$mean) # =0, as it's constant 

#Forecast errors 
ts_departs_test - (meanf(ts_departs_train,h=6)$mean) 
#         Jan        Feb        Mar        Apr        May        Jun
#2021 -1032.0758 -1089.4758 -1078.6758 -1074.6758 -1053.7758  -961.4758
#mean of forecast error 
mean(ts_departs_test - (meanf(ts_departs_train,h=6)$mean) ) # -1048.359, very big

accuracy(meanf(ts_departs_train,h=6))
#                       ME     RMSE     MAE       MPE     MAPE     MASE      ACF1
# Training set -6.890995e-14 423.6527 322.303 -145.6652 165.8153 1.670837 0.9059284

# High forecast errors, so high bias, variance zero 
# very bad forecast for H1 2021, will look better as recovery continues in 2021 and 2022
# However, it has no seasonality and no trend, which are likely to be important as 
# conditions return to normal.  

####################
# Testing naive()

####################
# Testing Seasonal naive

snaive(ts_departs_train,h=6)
#        Point Forecast       Lo 80   Hi 80     Lo 95     Hi 95
#Jan 2021         1183.5  609.069955 1757.93  304.9851 2062.0149
#Feb 2021         1161.9  587.469955 1736.33  283.3851 2040.4149
#Mar 2021          575.6    1.169955 1150.03 -302.9149 1454.1149
#Apr 2021           12.8 -561.630045  587.23 -865.7149  891.3149
#May 2021           24.7 -549.730045  599.13 -853.8149  903.2149
#Jun 2021           53.1 -521.330045  627.53 -825.4149  931.6149
 
ts_departs_test 
#     Jan   Feb   Mar   Apr   May   Jun
#2021 104.1  46.7  57.5  61.5  82.4 174.7

#Forecast errors 
ts_departs_test - (snaive(ts_departs_train,h=6)$mean) 
#        Jan     Feb     Mar      Apr     May     Jun
# 2021 -1079.4 -1115.2  -518.1    48.7    57.7   121.6

#mean of forecast error 
mean(ts_departs_test - (snaive(ts_departs_train,h=6)$mean) ) # = -414.1167
# std dev of forecast error
sd(ts_departs_test - (snaive(ts_departs_train,h=6)$mean) ) # 577, very big

# std dev of forecast 
sd(snaive(ts_departs_train,h=6)$mean) # 561, very big

accuracy(snaive(ts_departs_train,h=6))
#                   ME     RMSE      MAE       MPE   MAPE     MASE      ACF1
# Training set -60.92417 448.2301 192.8992 -232.9477 243.34    1      0.9344325

# Forecast errors very big in H1 2021, so very big bias, low variance  

####################
# Testing Drift 

rwf(ts_departs_train,drift=TRUE,h=6)
#             Point Forecast      Lo 80    Hi 80     Lo 95    Hi 95
# Jan 2021       151.7008  -54.23036 357.6319 -163.2437 466.6452
# Feb 2021       147.3015 -145.03012 439.6332 -299.7811 594.3842
# Mar 2021       142.9023 -216.47286 502.2774 -406.7146 692.5191
# Apr 2021       138.5031 -278.01314 555.0192 -498.5035 775.5096
# May 2021       134.1038 -333.29700 601.5046 -580.7241 848.9317
# Jun 2021       129.7046 -384.18631 643.5955 -656.2237 915.6329

ts_departs_test 
#     Jan   Feb   Mar   Apr   May   Jun
#2021 104.1  46.7  57.5  61.5  82.4 174.7

#Forecast errors 
ts_departs_test - (rwf(ts_departs_train,drift=TRUE,h=6)$mean) 
#         Jan        Feb        Mar        Apr        May        Jun
#2021  -47.60076 -100.60153  -85.40229  -77.00305  -51.70382   44.99542
 
#mean of forecast error 
mean(ts_departs_test - (rwf(ts_departs_train,drift=TRUE,h=6)$mean) ) # = -52.88601 
# std dev of forecast error
sd(ts_departs_test - (rwf(ts_departs_train,drift=TRUE,h=6)$mean) ) # 52.01118

sd(rwf(ts_departs_train,drift=TRUE,h=6)$mean) = 8.23

accuracy(rwf(ts_departs_train,drift=TRUE,h=6))
#                   ME        RMSE      MAE       MPE     MAPE      MASE      ACF1
# Training set 6.378343e-14 159.4669 121.5397 -33.09169 46.47038 0.6300687 0.4252624

# Low estimates are not too bad as forecasts for H1 2021 
# However, this is still in the nadir of the pandemic
# The drift model is not signalling the recovery
# The downward trend on drift is moving in the opposite direction to the recovery's upward trend.


####################
# Section 4.2 - Simple decomposition
####################

#dev.off()
#par(mfrow = c(2, 1))
par(mfrow = c(1, 1))

fit1.decadd<-decompose(ts_departs_train,type='additive')
fit1.decadd
plot(fit1.decadd)

fit2.decmult<-decompose(ts_departs_train,type='multiplicative')
fit2.decmult
plot(fit2.decmult)

# Additive seems more likely,as the variance looks like it remains constant 
# through the training data history
# Both random terms fail normal 

classic_add<-decompose(ts_departs_train,type='additive')
pclass<-autoplot(classic_add)+xlab('Year')
clean_classic_add<-na.omit(classic_add$random)

jarque.bera.test(clean_classic_add)
# rejects normality of residuals
Box.test(x = clean_classic_add)
#rejects null hypothesis of independence in the residual term

classic_multi<-decompose(ts_departs_train,type='multiplicative')
pmult<-autoplot(classic_multi)+xlab('Year')
clean_classic_multi<-na.omit(classic_multi$random)
jarque.bera.test(clean_classic_multi)

# rejects normality of residualsBox.test(x = clean_classic_add)
#rejects null hypothesis of independence in the residual term
# Likely unexplained variation not captured by the decomp model here, 
# so we should look at other models instead

forecast(clean_classic_add,h=12)
#         Point Forecast     Lo 80     Hi 80      Lo 95       Hi 95
#Jul 2020      -571.6223 -685.3097 -457.9350  -745.4921 -397.752543
#Sep 2020      -571.6223 -768.5214 -374.7232  -872.7535 -270.491114
#Aug 2020      -571.6223 -732.3925 -410.8522  -817.4990 -325.745620
#Oct 2020      -571.6223 -798.9799 -344.2647  -919.3358 -223.908853
#Dec 2020      -571.6223 -850.0751 -293.1696  -997.4791 -145.765585
#Nov 2020      -571.6223 -825.8146 -317.4300  -960.3759 -182.868790
#Jan 2021      -571.6223 -872.3850 -270.8597 -1031.5991 -111.645569
#Mar 2021      -571.6223 -912.6540 -230.5906 -1093.1853  -50.059366
#Feb 2021      -571.6223 -893.1505 -250.0941 -1063.3573  -79.887368
#Apr 2021      -571.6223 -931.1009 -212.1438 -1121.3973  -21.847305
#May 2021      -571.6223 -948.6463 -194.5984 -1148.2307    4.986055
#Jun 2021      -571.6223 -965.4107 -177.8339 -1173.8697   30.625030

accuracy(forecast(clean_classic_add,h=12))
#                   ME     RMSE      MAE      MPE     MAPE     MASE      ACF1
# Training set -4.829311 87.96834 47.04839 118.2794 214.8454 0.781896 0.2756949

plot(forecast(clean_classic_add,h=12))

ts_departs_test
# Jan   Feb   Mar   Apr   May   Jun
# 2021 104.1  46.7  57.5  61.5  82.4 174.7

# Forecast error = (y - yhat)
ts_departs_test - c(-571.6223,-571.6223,-571.6223,-571.6223,-571.6223,-571.6223)
#       Jan      Feb      Mar      Apr      May      Jun
#2021 675.7223 618.3223 629.1223 633.1223 654.0223 746.3223

# mean of forecast error
mean(ts_departs_test - c(-571.6223,-571.6223,-571.6223,-571.6223,-571.6223,-571.6223)) # 659.44
# This is the biggest error by far among methods so far
# Std dev of forecast error
sd(ts_departs_test - c(-571.6223,-571.6223,-571.6223,-571.6223,-571.6223,-571.6223)) # 47.21324

