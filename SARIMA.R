# SARIMA analysis 

#Plotting the data
#Take first differences of the data until it is stationary
#Transform the data if necessary to stabilise the variance
#Examinethe ACF/PACF plots to identify possible models.
#Fit the models
#Use the AICc statistic to search for the best models 
#Check residuals by plotting their ACF and do a Box Ljung test
#Once residuals look like white noise, calculate the forcast values

# Plot the time series and its decomposition
plot(ts_departs)
plot(decompose(ts_departs))
# Seasonality with annual lag is obvious

nsdiffs(ts_departs)
# D =  1 for seasonal terms
ndiffs(ts_departs)
# d = 0 

ndiffs(ts_departs_filtered)
nsdiffs(ts_departs_filtered)

Acf(ts_departs)
Pacf(ts_departs)
# Acf shows significant positive correlation on lags 1 and 2, then slow decline
# Pacf shows spikes at 1 also, supporting view that there is AR
# Acf Pacf reverses

# Run auto.arima to devise a model
# stepwise = FALSE to do a comprehensive slow search
sax<-auto.arima(ts_departs,seasonal=TRUE,stepwise=FALSE)
summary(sax)
# ARIMA(2,0,0)(0,1,1)[12] 
# RMSE = 102.1331 
# AIC=1742.81   AICc=1743.1   BIC=1754.6
# ARIMA(2,0,0)(0,1,1)[12]

# check manually possible alternatives
sa1<-auto.arima(ts_departs,seasonal=TRUE)
summary(sa1)
# order is (2,0,0)
# seasonal is (2,1,0)[12]

sa2 <-Arima(ts_departs,order=c(2,0,0),seasonal=c(2,1,0))
summary(sa2)
# Matches the automated search result
#sa2<-auto.arima(ts_departs_filtered,seasonal=TRUE)
# AIC=1745.71   AICc=1746.15   BIC=1760.45
# RMSE = 104.2724


sa3 <-Arima(ts_departs,order=c(1,0,0),seasonal=c(2,1,0))
summary(sa3)
# AIC=1774.42   AICc=1774.71   BIC=1786.22
# RMSE = 116.1302

sa4 <-Arima(ts_departs,order=c(1,0,0),seasonal=c(1,1,0))
summary(sa4)
# RMSE = 137.1179
# AIC=1808.88   AICc=1809.05   BIC=1817.72
# worse than sa1

sa5 <-Arima(ts_departs,order=c(1,1,0),seasonal=c(1,0,0))
summary(sa5)
# RSME = 120.714
# AIC=1899.88   AICc=1900.04   BIC=1908.95
# Worse than before

# Plot the residuals from the automatically derived model, sa1
checkresiduals(sax)
# AR, MA unlikely, nothing is significant out to 36 months
# ACF shows one significan tag around 23 months 
Box.test(x = sax,type='Ljung-Box')
# Result is that null hypothesis of independence is upheld
# Residuals from ARIMA(2,0,0)(0,1,1)[12]
# Q* = 17.836, df = 21, p-value = 0.6594

sax %>%forecast(h=12) %>% autoplot()
# This forecasts a pattern showing 2023 with the same max and min and seasonality as 2016
# This is probably realistic, given the likely downturn and the continuing seasonality we see
# as we continue to recover from the pandemic's economic effects
# 95% confidence interval goes from 439 to 2363, a very wide range for uncertainty.  

forecast(sax,h=12)
# show the values 
