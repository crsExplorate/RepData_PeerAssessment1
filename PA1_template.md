# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

The data is loaded and data and time objects are created.


```r
data<-read.csv(unz("activity.zip","activity.csv"))
data$time<-as.POSIXct(strptime(paste(data$date,formatC(data$interval,digits=3,flag="0")),"%Y-%m-%d %H%M",tz="UTC"))
data$date<-as.Date(data$date,"%Y-%m-%d")
```


## What is mean total number of steps taken per day?


```r
stepPerDay<-tapply(data$steps,data$date,sum,na.rm=TRUE);
hist(stepPerDay,xlab = "Steps per day",col="red",breaks = 10,main = "Histogram of Steps per day")
abline(v=mean(stepPerDay),col="blue")
```

![](figure/steps_per_day-1.png) 

```r
mean(stepPerDay)
```

```
## [1] 9354.23
```

```r
median(stepPerDay)
```

```
## [1] 10395
```

The mean steps per day is 9354.2295082 and the median is 10395.

## What is the average daily activity pattern?


```r
library(ggplot2)
stepPerInterval<-aggregate(steps~interval,data=data,FUN=mean,na.rm=TRUE);
qplot(interval,steps,data = stepPerInterval,geom="line",ylab = "Number of Steps",xlab = "Interval",main = "Average Steps per interval")
```

![](figure/daily_activiy_pattern-1.png) 

```r
stepPerInterval$interval[which.max(stepPerInterval$steps)]
```

```
## [1] 835
```

The interval 835 has an average of 206.1698113, that is the maximum interval average.

## Imputing missing values

There are only missin value on steps and they are 2304.

```r
summary(data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304                                          
##       time                    
##  Min.   :2012-10-01 00:00:00  
##  1st Qu.:2012-10-16 05:58:45  
##  Median :2012-10-31 11:57:30  
##  Mean   :2012-10-31 11:57:30  
##  3rd Qu.:2012-11-15 17:56:15  
##  Max.   :2012-11-30 23:55:00  
## 
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
auxData<-aggregate(data$steps,by=list(data$interval),FUN = median,na.rm=TRUE)
names(auxData)<-c("interval","median.steps")
filledData<-merge(data,auxData)
filledData$steps[is.na(filledData$steps)]<-filledData$median.steps[is.na(filledData$steps)]
summary(filledData)
```

```
##     interval          steps          date           
##  Min.   :   0.0   Min.   :  0   Min.   :2012-10-01  
##  1st Qu.: 588.8   1st Qu.:  0   1st Qu.:2012-10-16  
##  Median :1177.5   Median :  0   Median :2012-10-31  
##  Mean   :1177.5   Mean   : 33   Mean   :2012-10-31  
##  3rd Qu.:1766.2   3rd Qu.:  8   3rd Qu.:2012-11-15  
##  Max.   :2355.0   Max.   :806   Max.   :2012-11-30  
##       time                      median.steps   
##  Min.   :2012-10-01 00:00:00   Min.   : 0.000  
##  1st Qu.:2012-10-16 05:58:45   1st Qu.: 0.000  
##  Median :2012-10-31 11:57:30   Median : 0.000  
##  Mean   :2012-10-31 11:57:30   Mean   : 3.962  
##  3rd Qu.:2012-11-15 17:56:15   3rd Qu.: 0.000  
##  Max.   :2012-11-30 23:55:00   Max.   :60.000
```

```r
stepPerDayAdj<-tapply(filledData$steps,filledData$date,sum,na.rm=TRUE);
hist(stepPerDayAdj,xlab = "Steps per day",col="red",breaks = 10,main = "Histogram of Steps per day")
abline(v=mean(stepPerDayAdj),col="blue")
```

![](figure/missing_values_estimate-1.png) 

```r
mean(stepPerDayAdj)
```

```
## [1] 9503.869
```

```r
median(stepPerDayAdj)
```

```
## [1] 10395
```

The adjusted mean steps per day is 9503.8688525 and the median is 10395.


## Are there differences in activity patterns between weekdays and weekends?


```r
Sys.setlocale("LC_TIME", "C");
```

```
## [1] "C"
```

```r
filledData$weekday=as.factor(ifelse(weekdays(filledData$time,abbreviate = TRUE) %in% c("Sat","Sun"),"weekday","weekend"));
filledData$weekday=relevel(filledData$weekday,"weekday")
filledDataByInterval<-aggregate(steps~interval+weekday,data=filledData,FUN = mean,na.rm=TRUE)
library(lattice)
qplot(interval,steps,data=filledDataByInterval,facets = weekday~.,geom="line",ylab = "Number of Steps",xlab = "Interval",main = "Average number os steps per Interval: Day Pattern")
```

![](figure/pattern_weekdays-1.png) 

