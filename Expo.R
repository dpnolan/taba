# Use ETS to automatically fit and evaluate an exponential 
# smoothing model

departs_fit1<-ets(y=ts_departs,model='ZZZ',damped=TRUE)
departs_fit1
summary(departs_fit1)
plot(departs_fit1)
forecast(departs_fit1,h=24)
dsplot(forecast(departs_fit1,h=8),
     ylab="Departures 000s",
     main="Forecasts from ETS(A,Ad,A) 
     with damping")
plot(forecast(departs_fit3,h=8))

# alpha = 0.9999  smoothing parameter for the level, the smoothed output
# beta  = 0.1413  the same for the slope ('trend') component
# phi   = 0.8  component for the damping
# gamma = 1e-04  smoothing parameter for the seasonality
#Initial states:
#  l = 967.8715 
#  b = 4.9303 
#  s = -177.1826 -162.8496 87.1254 138.4545 288.4828 298.7065
#  215.4203 53.5407 -69.9537 -136.5938 -261.4512 -273.6992
#
# AIC          AICc        BIC 
# 2230.789     2235.894    2285.337


# The 3 components, error, trend and seasonal, are all 
# additive and not multiplicative

# departs_fit2
# TS model on the same dataset, selected as all arithmetic
# gives the same parameters and AIC, AICc and BIC.  


checkresiduals(departs_fit1)
# AR order 1 showing in ACF
# ACF also shows significant correlations around 23 and 24 months 
Box.test(x = residuals(departs_fit1),type='Ljung-Box')

residuals1=residuals(departs_fit1)
qqnorm(residuals(departs_fit1)); qqline(residuals(departs_fit1))
jarque.bera.test(residuals1)
#QQ plot and JB indicate residuals aren't normal
#not white noise p-value near zero 

cbind("Residuals"=residuals(departs_fit1),
         'Forecast errors' = 
        residuals(departs_fit1,type='response')) %>% 
  autoplot(facet=TRUE)

departs_fit2<-ets(ts_departs,model="AAA",use.initial.values = TRUE)
departs_fit2
summary(departs_fit2) 
# shows parameters AIC/c and BIC,
# plus RMSE 
plot(departs_fit2)
plot(departs_fit1)
autoplot(departs_fit1)

#doesn't work
autoplot(ts_departs)+
  autoplot(departs_fit2,series='HW additive forecasts', PI=FALSE) +
  xlab('Year')+
  ylab('Departures 000s')+
  guides(colour=guide_legend(title='Forecast'))

departs_fit3<-hw(ts_departs,damped=FALSE,seasonal="additive")
departs_fit3
summary(departs_fit3)

# fit3 
plot(forecast(departs_fit1,h=8)) #mean around 1500, with damping
plot(forecast(departs_fit3,h=8)) # mean around 2000, no damping
forecast(departs_fit1,h=8) #range 2331.54 to 746.3, point of 1538.9
forecast(departs_fit3,h=8) #range 941.48 to 2867.9, point of 1904.69


autoplot(ts_departs)+
  autolayer(departs_fit3,series='HW additive forecasts', PI=FALSE) +
  xlab('Year')+
  ylab('Departures 000s')+
  guides(colour=guide_legend(title='Forecast'))

plot(departs_fit3)
plot(departs_fit2)
residuals(departs_fit2)
ggplot2::autoplot(departs_fit2)
plot(departs_fit2,plot.type="single",ylab="",col=1:4)
legend("topleft", inset=.02,
       c('Line 1','Line 2','Line 3','Line 4'), 
       fill=topo.colors(4), horiz=TRUE, cex=0.8)

plot(departs_fit2)
plot(departs_fit2,plot.type="single",ylab="",col=1:3)

library(ggplot2)
autoplot(fit)

(lambda <- BoxCox.lambda(ts_departs))
autoplot(BoxCox(ts_departs,lambda))
departs_fit4<-ets(y=(BoxCox(ts_departs,lambda)),
                  model='ZZZ')#,damped=TRUE)
summary(departs_fit4)
boxcox_trans()