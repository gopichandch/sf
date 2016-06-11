library(tidyr)
data=read.csv("order_data.csv",header=T)
data$order_id=NULL
data$seller_id=NULL
data$house_number=NULL
data$sublocality=NULL
data$scheduled_time=as.POSIXlt(data$scheduled_time)
summary(data)
data2=data.frame(data$rider_id,data$cluster_id,data$scheduled_time)
names(data2)=c('rider_id','cluster_id','s_date')
data2=data2[order(data2$rider_id,data2$cluster_id,data2$s_date),]
rownames(data2)=NULL

data2=separate(data=data2,col=s_date,into=c('s_date','s_time'),sep=' ')
data2$s_date=as.Date(data2$s_date)
data2$s_time=gsub(':','.',data2$s_time)
data2$s_time=substr(data2$s_time,0,5)
data2$s_time=as.numeric(data2$s_time)
data2$diff <- ave(data2$s_time, data2$rider_id,data2$cluster_id,data2$s_date, FUN=function(x) c(0, diff(x)))

data2$change=data2$s_time
data2$change=as.character(data2$change)
data2$change=gsub('\\.',':',data2$change)
data2=separate(data=data2,col=change,into=c('change','change2'),sep=':')
data2$change=as.numeric(data2$change)
data2$diff2 <- ave(data2$change, data2$rider_id,data2$cluster_id,data2$s_date, FUN=function(x) c(0, diff(x)))
data2$diff3=data2$diff
data2$diff3=ifelse(data2$diff2>0,data2$diff3-0.40,data2$diff3)
names(data2)
data2$diff3=ifelse(data2$diff3>0.60&data2$diff3<1,(data2$diff3-0.60)+1,data2$diff3)
data2$diff=NULL
data2$change=NULL
data2$diff2=NULL
data2$group=ifelse(data2$diff3==0|data2$diff3>0.50,1,0)


data$diff=NULL
data$diff3=NULL

data=data[order(data$rider_id,data$cluster_id,data$scheduled_time),]
rownames(data)=NULL
write.csv(data,"data1.csv",row.names=F)
write.csv(data2,"data2.csv",row.names=F)
data=read.csv("data1.csv",header=T)
data=data[data$delivered_time!='NULL',]
data3=read.csv("copy.csv",header=T)
summary(data3)
names(data)
data=merge(data,data3,by=c("rider_id","cluster_id","scheduled_time","delivered_time","pickup_latitude","pickup_longitude","delivered_latitude","delivered_longitude"),all.x=T)
data=unique(data)
data=data[data$delivered_time!='NULL',]
rownames(data)=NULL
data$time=data$time/60
data$time=round(data$time,0)
data$scheduled_time=strptime(data$scheduled_time,format="%m/%d/%Y %H:%M")
data$delivered_time=strptime(data$delivered_time,,format="%m/%d/%Y %H:%M")
data$delay=difftime(data$delivered_time,data$scheduled_time,units="mins")
data$user='0'
data$user=ifelse(data$delay>45,'not good','good')


