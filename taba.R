# Peter Nolan x22154116
# 
# Code for Terminal Assessment by Assignment (TABA)
# 
# See Google Colab workbook at   
# 
# https://colab.research.google.com/gist/dpnolan/7bf9b9b342490bfba31fb761510f9c19/tata-time-series-analysis.ipynb?authuser=3#scrollTo=-VpUAwwDymKr
# 

# Install the tseries library, so that we can use the 'ts' object for time series data
install.packages('tseries')
library('tseries')

# Upload the file 
departs<-read.csv('Departure.csv')

# We will use only the time series object from the Rstat tseries
# This combines an observation in double float point format 
# with its datetime, the month+year in our case
#
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

# Graph the full time series 
ggplot(ts_departs, aes(x=as.yearmon(index(ts_departs)),y = ts_departs))+
  geom_line(colour='blue')+
  geom_point(size=1,colour='blue') + 
  ylab("Departures 000's") + xlab('Date')
 # vertical line to show pandemic decrease   
#  geom_vline(xintercept = 2020.25,colour='red',size=1.5,alpha=0.5)  
#  scale_y_continuous(breaks =  seq(2010,2022))
#  geom_area(fill = "blue", alpha = .2,ymin=2020)
#
# uses yearmon data type from zoo library
#
# The top chart shows a pronounced, strong growth trend
# 1. Consistent upward trend in maximum and min for each series from 2010 to 2020
# 2. Clear seasonality with peaks in the summer months
# 3. Pandemic effect is clear in the rapid fall in Q1 of 2020 and slow recovery
# 4. 2022 Looks closer to the normal numbers


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

# Generate the histogram of departures
ggplot(ts_departs, aes(x = ts_departs,y=..density..)) + 
  geom_histogram(binwidth=50, fill = "blue", colour = "black", alpha=0.3)+
  geom_density(colour='blue', size=1.5)+
  xlab('Departures 000s')+
  scale_y_discrete(labels=percent,breaks=c(0.001,0.0005,0))
  # xticks('')  
  # geom_vline(xintercept = 1088.6 ,colour='red',size=1.5,alpha=0.5)+
 


# Window the data by year
d2010<-window(ts_departs,start=2010, end=2010.99)
d2011<-window(ts_departs,start=2011, end=2011.99)
d2012<-window(ts_departs,start=2012, end=2012.99)
d2013<-window(ts_departs,start=2013, end=2013.99)
d2014<-window(ts_departs,start=2014, end=2014.99)
d2015<-window(ts_departs,start=2014, end=2015.99)
d2016<-window(ts_departs,start=2016, end=2016.99)
d2017<-window(ts_departs,start=2017, end=2017.99)
d2018<-window(ts_departs,start=2018, end=2018.99)
d2020<-window(ts_departs,start=2020, end=2020.99)
d2021<-window(ts_departs,start=2021, end=2021.99)
d2022<-window(ts_departs,start=2022, end=2022.99)

# Generate a boxplot with these

boxplot(x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22, 
        names=seq(2010,2022), ylab='Departures 000s',col='blue')

# T-tests

#2013 v 2020
(t.test(d2013,d2019,alternative='two.sided',var.equal=FALSE))['p.value']
# p.value=7.696756e-05, so REJECT null hypothesis of equality

#2019 v 2020
t.test(d2019,d2020,alternative='two.sided',var.equal=FALSE)['p.value']
# p.value=6.286207e-08, so REJECT null hypothesis of equal means

#2020 v 2022
t.test(d2020,d2022,alternative='two.sided',var.equal=FALSE)['p.value']
# p.value=6.286207e-08, so REJECT null hypothesis of equal means


trend_model<-lm(ts_departs~index(ts_departs))
summary(trend_model)

plot(ts_departs[129:153])
plot(x20)
plot(x21)
plot(x22)

pandemic_departs=window(ts_departs,start=c(2019,1),end=c(2022,9))
plot(pandemic_departs,ylab=('Departures 000s'))

# Exponential smoothing
library(fpp2)
plot(ts_departs, main="Departures from Irish airports")
plot(ma(ts_departs,3), main = "Departures from Irish airports, Simple MA, k=3")
plot(ma(ts_departs,5), main = "Departures from Irish airports, Simple MA, k=5")
plot(ma(ts_departs,7), main = "Departures from Irish airports, Simple MA, k=7")
plot(ma(ts_departs,9), main = "Departures from Irish airports, Simple MA, k=9")
plot(ma(ts_departs,11), main = "Departures from Irish airports, Simple MA, k=11")
plot(ma(ts_departs,13), main = "Departures from Irish airports, Simple MA, k=13")
install.packages('gcookbook')
library(gcookbook)



kpss.test(ts_departs,null='Trend')

monthplot(ts_departs)
seasonplot(ts_departs)
library(ggplot2)
adf.test(ts_departs,alternative='stationary',k=0)
# k = 0, p = 0.5597
# k=1,2,3,4,5,6
# k=2, p=0.01
# k=1, p=0.03934
# k=3, p=0.02942
# k=4, p-0.3421
# k=5, p=0.4725

attributes(ts_departs)

m <- lm(coredata(ts) ~ index(ts))
lm(departs~departs.index())
departs(Month)

library('zoo')
typeof(departs$Month[153])

index(as.zoo(ts_departs))[153]

yearmon(index((as.zoo(ts_departs))))


Augmented Dickey-Fuller Test
=data:  ts_departs
Dickey-Fuller = -1.9273, Lag order = 6, p-value = 0.6067
alternative hypothesis: stationary

# Plot the seasonality
library('fpp2')
ggseasonplot(ts_departs,year.labels=TRUE, year.labels.left=TRUE) + ylab('1,000') 
+ ggtitle("Seasonal plot: departures from Irish airports")
# Clear and consistent seasonality every year apart from the collapse
# in 2020 and the progressive growth in 2021
# Timing, with the collapse dating to February

# Since the start and end of the COVID pandemic was extraordinary
# with only one complete example to capture, we can remove it to
# discover more generalisable analysis from the data

ts_departs_filtered<-window(ts_departs, start = c(2010,1), end = c(2020,2))
ts_departs_filtered

monthplot(ts_departs_filtered)
# Month by month view confirms this

seasonplot(ts_departs_filtered)
# same hump pattern in every year - is this consistent with the 

ggseasonplot(ts_departs_filtered,year.labels=TRUE, year.labels.left=TRUE) 
+ ylab('1,000') 
+ ggtitle("Seasonal plot: departures from Irish airports")

fit1.decmult<-decompose(ts_departs,type='additive')
fit1.decmult
plot(fit1.decmult)
fit1.decmult

fit2.decmult<-decompose(ts_departs_filtered,type='additive')
fit2.decmult
plot(fit2.decmult)
fit2
# Removing covid gives a clear trend upwards, clearer seasonality, random looks
# more like a random walk

fit3.decmult<-decompose(ts_departs,type='multiplicative')
fit3.decmult
plot(fit3.decmult)

fit4.decmult<-decompose(ts_departs_filtered,type='multiplicative')
fit4.decmult
plot(fit4.decmult)

# multiplicative gives a cleaner random term, its seasonal component also

acf(ts_departs)
acf(ts_departs_filtered)

pacf(ts_departs)
pacf(ts_departs_filtered)

ndiffs(ts_departs)
nsdiffs(ts_departs)
ndiffs(ts_departs_filtered)
nsdiffs(ts_departs_filtered)
sa1<-auto.arima(ts_departs,seasonal=TRUE)
sa2<-auto.arima(ts_departs_filtered,seasonal=TRUE)
forecast(sa1,12)
plot(forecast(sa1,12))

forecast(sa2,30)
plot(forecast(sa2,30))
