setwd("/Users/adrianhsu/Google_Drive/NTUEE_106_1/sta/sta2017/1013-lab2")

dat<-read.csv('NORMAL_DLY_sample.csv', header=TRUE)

newdat<-cbind(dat$DLY_TMIN_NORMAL, dat$DLY_TMAX_NORMAL)
temp<-as.data.frame(newdat) #轉成 data.frame的指令
colnames(temp)<-c("TMIN", "TMAX")

plot(temp$TMIN, main="TMIN&TMAX", xlab="date", ylab="temp", type='s', col='blue', asp=0.10)
points(temp$TMAX, type="s", col='red')
