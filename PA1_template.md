# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

###1.Load the data

```r
activity<-read.csv("activity/activity.csv",sep=",")
```

###2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity$date<-as.Date(activity$date)
#needed library
library(dplyr)
library(lubridate)
library(lattice)
```

## What is mean total number of steps taken per day?

###1.Make a histogram of the total number of steps taken each day
![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

###2.Calculate and report the mean and median of the total number of steps taken per day

```r
data.frame(mean=mean(steps.by.day$numStep,na.rm = TRUE),median=median(steps.by.day$numStep,na.rm = TRUE))
```

```
##      mean median
## 1 9354.23  10395
```



## What is the average daily activity pattern?

###1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
#Average number of steps take by 5mn interval
steps.by.interval<- summarise(group_by(activity,interval),meanStep=mean(steps,na.rm = TRUE))
#plot
with(steps.by.interval,plot(interval,meanStep, type="l",col="red",
                  main="average number of steps by 5mn time interval",xlab="5mn time interval",ylab="average number of steps"))
maxInter<-steps.by.interval$interval[which.max(steps.by.interval$meanStep)]
abline(v=maxInter)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The interval 835 contains the maximum number of steps



## Imputing missing values
###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
###2.Devise a strategy for filling in all of the missing values in the dataset.

The strategy for filling missing values consists in replacing the NA by the mean that has been calculated for the interval.

###3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity.filled.NA=activity
#function to get the meanStep of a given interval
getMeanStep=function(i) {steps.by.interval$meanStep[steps.by.interval$interval==i]}
#replace NA with the mean
activity.filled.NA$steps[is.na(activity$steps)]=sapply(activity$interval[(which(is.na(activity$steps)))],getMeanStep)
```

###4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#mean value by day
steps.by.day2<- summarise(group_by(activity.filled.NA,date),numStep=sum(steps,na.rm = TRUE))
#histogramme
hist(steps.by.day2$numStep,main="histogramme of the total number of steps taken per day",xlab="")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
data.frame(mean=mean(steps.by.day2$numStep,na.rm = TRUE),median=median(steps.by.day2$numStep,na.rm = TRUE))
```

```
##       mean   median
## 1 10766.19 10766.19
```
The distribution is centered as the mean and median are the same.  
The bias due to missing values has been corrected


## Are there differences in activity patterns between weekdays and weekends?
###1.#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
#function to get weekday/weekend
weekSplit=function(dt){
    if (wday(dt) %in% 2:6) {
        res<-"weekday"
    }else{
        res<-"weekend"
    }
    res
}
activity.filled.NA$weeksplit=sapply(activity.filled.NA$date,weekSplit)
```

###2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
#mean steps by interval
steps.by.interval2<- summarise(group_by(activity.filled.NA,interval,weeksplit),meanStep=mean(steps))
#plot separately weekdays and weekends
xyplot(meanStep ~ interval | weeksplit, data = steps.by.interval2, layout = c(1, 2),type="l",
       main="average number of steps by 5mn time interval",xlab="5mn time interval",ylab="average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

