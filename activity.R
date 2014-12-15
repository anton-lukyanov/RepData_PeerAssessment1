setwd("C:/Users/Anton/RepData_PeerAssessment1/activity")
input<-read.csv("activity.csv")
activityData<-input[complete.cases(input),]
activityData_mod1$date<-as.Date(activityData_mod1$date,"%Y-%m-%d")

dates<-unique(as.Date(activityData$date,"%Y-%m-%d"))


activityDays<-aggregate(activityData_mod1[, 1], list(activityData_mod1$date), sum)

mean1<-lapply(split(activityData_mod1, as.Date(activityData_mod1$date,"%Y-%m-%d")),mean)

names(activityDays)<-c("Date","Steps")
hist(activityDays$Steps, col = "red",xlab="Steps daily",main="",breaks=15)
title(main = "Global Active Power",xlab= "Global Active Power (kilowatts)") 
mean1<-mean(activityDays$Steps)
median1<-median(activityDays$Steps)

setwd("C:/Users/Anton/RepData_PeerAssessment1")

activityIntervals<-aggregate(activityData[, 1], list(activityData$interval), mean)
plot(activityIntervals$x,type="l",xlab="5-min intervals",ylab="average steps per interval")
activityIntervals[activityIntervals$x==max(activityIntervals$x),]$Group.1

nrow(input)




activityIntervals[activityIntervals$Group.1==835,2]
newData<-input
fg<-function(){
  for (i in 1:nrow(newData)){
    if(is.na(newData$steps[i])==TRUE){
      newData$steps[i]<-activityIntervals[activityIntervals$Group.1==newData$interval[i],2]
    } 
  }
  newData
}
newData<-fg()
newData$date=as.Date(newData$date,"%Y-%m-%d")

newData1<-subset(newData,newData$WD=="weekday")
activityIntervals1<-aggregate(newData1[, 1], list(newData1$interval), mean)
newData2<-subset(newData,newData$WD=="weekend")
activityIntervals2<-aggregate(newData2[, 1], list(newData2$interval), mean)

Sys.setlocale("LC_TIME","English United States")
c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
  "Friday", "Saturday")[as.POSIXlt(newData$date)$wday + 1]

weekdays(newData$date[2],TRUE)

newData$WD <- ifelse(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                       "Friday", "Saturday")[as.POSIXlt(newData$date)$wday + 1]=="Saturday"|
                       c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                         "Friday", "Saturday")[as.POSIXlt(newData$date)$wday + 1]=="Sunday", "weekend", "weekday")



newData$WD <- ifelse(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                       "Friday", "Saturday")[as.POSIXlt(newData$date)$wday + 1]=="Saturday"|
                       c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                         "Friday", "Saturday")[as.POSIXlt(newData$date)$wday + 1]=="Sunday", "weekend", "weekday")

newData1<-subset(newData,newData$WD=="weekday")
activityIntervals1<-aggregate(newData1[, 1], list(newData1$interval), mean)
newData2<-subset(newData,newData$WD=="weekend")
activityIntervals2<-aggregate(newData2[, 1], list(newData2$interval), mean)
plot(activityIntervals1$x, type="n",xlab="",main="",
     ylab="average steps per interval,xaxt='n')
plot(activityIntervals1$x,type="l",col="red")
plot(activityIntervals2$x,type="l",col="blue")
legend("topright",legend=c("Weekdays","Weekends"),
       lty=1,col=c("red","blue"),bty="n")

#
Add factor:
```{r}
newData$date=as.Date(newData$date,"%y-%m-%d")
newData$WD <- ifelse(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[as.POSIXlt(newData$date)$wday + 1]=="Saturday"|c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday")[as.POSIXlt(newData$date)$wday + 1]=="Sunday", "weekend", "weekday")

newData1<-subset(newData,newData$WD=="weekday")
activityIntervals1<-aggregate(newData1[, 1], list(newData1$interval), mean)
newData2<-subset(newData,newData$WD=="weekend")
activityIntervals2<-aggregate(newData2[, 1], list(newData2$interval), mean)
plot(activityIntervals1$x, type="n",xlab="",main="",
     ylab="average steps per interval" ,xaxt='n')
plot(activityIntervals1$x,type="l",col="red")
plot(activityIntervals2$x,type="l",col="blue")
legend("topright",legend=c("Weekdays","Weekends"),
       lty=1,col=c("red","blue"),y.intersp=0.3,bty="n")

```

