

departs<-read.csv('Departure.csv')
departs

ts_departs<-ts(departs$departures..000,start=c(2010,1),frequency=12)
#Print 
ts_departs

# Check the basic definition terms are correct
start(ts_departs)
end(ts_departs)
frequency(ts_departs)

# The top chart shows a pronounced, strong growth trend
plot(ts_departs)

library('tseries')
adf.test(ts_departs)

> adf.test(ts_departs,alternative='stationary',k=1)
# k=1,2,3,4,5,6
# k=2, p=0.01
# k=1, p=0.03934
# k=3, p=0.02942
# k=4, p-0.3421
# k=5, p=0.4725

Augmented Dickey-Fuller Test

data:  ts_departs
Dickey-Fuller = -1.9273, Lag order = 6, p-value = 0.6067
alternative hypothesis: stationary

# Plot the seasonality
library('fpp2')
ggseasonplot(ts_departs,year.labels=TRUE, year.labels.left=TRUE) 
+ ylab('1,000') 
+ ggtitle("Seasonal plot: departures from Irish airports")
# Clear and consistent seaonality every year apart from the collapse
# in 2020 and the progressive growth in 2021
# Timing, with the collapse dating to February
