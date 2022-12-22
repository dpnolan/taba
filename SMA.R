
simplema=data.frame(ts_departs,row.names=index(ts_departs))
simplema

for (i in c(3,5,7,9,11,13))
{
  print(i)
  mak<-ma(ts_departs,i)
  simplema<-cbind(simplema,mak)
  colnames(simplema)[length(colnames(simplema))]=paste("SMA_",toString(i),sep='')
}
colnames(simplema)
dev.off()
