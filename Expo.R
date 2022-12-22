# Use ETS to automatically fit and evaluate an exponential smoothing model

departs_fit1<-ets(y=ts_departs,model='ZZZ',damped=TRUE)
departs_fit1
summary(departs_fit1)
forecast(departs_fit1,h=8)
plot(forecast(departs_fit1,h=8))
# alpha = 0.9999  smoothing parameter for the level 
# beta  = 0.1413  ditto for the slope, 
# phi   = 0.8 
# gamma = 1e-04 

#Initial states:
#  l = 967.8715 
#  b = 4.9303 
#  s = -177.1826 -162.8496 87.1254 138.4545 288.4828 298.7065
#  215.4203 53.5407 -69.9537 -136.5938 -261.4512 -273.6992

# AIC          AICc        BIC 
# 2230.789     2235.894    2285.337

# The 3 components, error, trend and seasonal, are all 
# additive and not multiplicative

# departs_fit2
# TS model on the same dataset, selected as all arithmetic
# gives the same parameters and AIC, AICc and BIC.  

residuals1=residuals(departs_fit1)
qqnorm(residuals1); qqline(residuals1)
jarque.bera.test(residuals1)
#QQ plot and JB indicate residuals aren't normal
#not white noise 

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
plot(forecast(departs_fit2,h=8)) #mean around 1500, with damping
plot(forecast(departs_fit3,h=8)) # mean around 2000, no dampin
forecast(departs_fit2,h=8) #range 2331.54 to 746.3 
forecast(departs_fit3,h=8) #range 941.48 to 2867.9

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