# Simple time series models

# Simple Moving Average

simplema=data.frame(ts_departs,row.names=index(ts_departs))
simplema

for (i in c(3,5,7,9,11,13))
{
#  print(i)
  mak<-ma(ts_departs,i)
  simplema<-cbind(simplema,mak)
  colnames(simplema)[length(colnames(simplema))]=paste("SMA_",toString(i),sep='')
}
colnames(simplema)

decompose(ts_departs,type='additive')

# Additive probably suits best, as the variance of the time series 
# does not significantly change with the level of the departures variable 

ts_departs %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of departures time series")

ts_departs %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of departures time series")

ma(ts_departs,12)



# multiplicative gives a cleaner random term, its seasonal component also