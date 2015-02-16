---
  title: "Reproducible Research: Peer Assessment 1"
  output: 
  html_document:
  self_contained: true
  keep_md: true
---
  
  
## Loading and preprocessing the data
  
### set working directery and read in data

```r
library(ggplot2)
setwd("C:/Users/leip/Desktop/Reproducible Research/RepData_PeerAssessment1")
unzip(zipfile = "activity.zip")
activity<-read.csv("activity.csv", header=T, na.strings = "NA")
```

## What is mean total number of steps taken per day?
### 1. Calculate the total number of steps taken per day

```r
Dsum <-aggregate(activity[,'steps'], by=list(activity$date), FUN=sum, na.rm=TRUE)
names(Dsum) <- c("Date","Sum.step")
```

### 2. Make a histogram of the total number of steps taken each day

```r
qplot(Sum.step, data=Dsum, geom="histogram", binwidth = 1000,
      xlab="Total number of steps taken each day")
```

![plot of chunk TotalStepDay](figure/TotalStepDay-1.png) 

### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
Dmean <- aggregate(activity[,'steps'], by=list(activity$date), FUN=mean, na.rm=TRUE)
names(Dmean) <- c("Date","Mean.step")
Dmedian <- aggregate(activity[,'steps'], by=list(activity$date), FUN=median, na.rm=TRUE)
names(Dmedian) <- c("Date","Median.step")
merge(Dmean, Dmedian, "Date")
```

```
##          Date  Mean.step Median.step
## 1  2012-10-01        NaN          NA
## 2  2012-10-02  0.4375000           0
## 3  2012-10-03 39.4166667           0
## 4  2012-10-04 42.0694444           0
## 5  2012-10-05 46.1597222           0
## 6  2012-10-06 53.5416667           0
## 7  2012-10-07 38.2465278           0
## 8  2012-10-08        NaN          NA
## 9  2012-10-09 44.4826389           0
## 10 2012-10-10 34.3750000           0
## 11 2012-10-11 35.7777778           0
## 12 2012-10-12 60.3541667           0
## 13 2012-10-13 43.1458333           0
## 14 2012-10-14 52.4236111           0
## 15 2012-10-15 35.2048611           0
## 16 2012-10-16 52.3750000           0
## 17 2012-10-17 46.7083333           0
## 18 2012-10-18 34.9166667           0
## 19 2012-10-19 41.0729167           0
## 20 2012-10-20 36.0937500           0
## 21 2012-10-21 30.6284722           0
## 22 2012-10-22 46.7361111           0
## 23 2012-10-23 30.9652778           0
## 24 2012-10-24 29.0104167           0
## 25 2012-10-25  8.6527778           0
## 26 2012-10-26 23.5347222           0
## 27 2012-10-27 35.1354167           0
## 28 2012-10-28 39.7847222           0
## 29 2012-10-29 17.4236111           0
## 30 2012-10-30 34.0937500           0
## 31 2012-10-31 53.5208333           0
## 32 2012-11-01        NaN          NA
## 33 2012-11-02 36.8055556           0
## 34 2012-11-03 36.7048611           0
## 35 2012-11-04        NaN          NA
## 36 2012-11-05 36.2465278           0
## 37 2012-11-06 28.9375000           0
## 38 2012-11-07 44.7326389           0
## 39 2012-11-08 11.1770833           0
## 40 2012-11-09        NaN          NA
## 41 2012-11-10        NaN          NA
## 42 2012-11-11 43.7777778           0
## 43 2012-11-12 37.3784722           0
## 44 2012-11-13 25.4722222           0
## 45 2012-11-14        NaN          NA
## 46 2012-11-15  0.1423611           0
## 47 2012-11-16 18.8923611           0
## 48 2012-11-17 49.7881944           0
## 49 2012-11-18 52.4652778           0
## 50 2012-11-19 30.6979167           0
## 51 2012-11-20 15.5277778           0
## 52 2012-11-21 44.3993056           0
## 53 2012-11-22 70.9270833           0
## 54 2012-11-23 73.5902778           0
## 55 2012-11-24 50.2708333           0
## 56 2012-11-25 41.0902778           0
## 57 2012-11-26 38.7569444           0
## 58 2012-11-27 47.3819444           0
## 59 2012-11-28 35.3576389           0
## 60 2012-11-29 24.4687500           0
## 61 2012-11-30        NaN          NA
```
  
## What is the average daily activity pattern?
### 1. Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
###    averaged across all days (y-axis)

```r
Iavg <- aggregate(activity[,'steps'], by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(Iavg) <- c("Interval","Mean.step")


plot2 <- ggplot(Iavg, aes(Interval, Mean.step)) + geom_line() +
          xlab("5-minute Interval") + ylab("Average Steps Across All Days")
plot2
```

![plot of chunk AvgStepInterval](figure/AvgStepInterval-1.png) 

### 2. Which 5-minute interval, on average contains the maximum number of steps?

```r
Iavg[which(Iavg$Mean.step==max(Iavg$Mean.step)),  ]
```

```
##     Interval Mean.step
## 104      835  206.1698
```

## Imputing missing values
### 1. Calculate and report the total number of missing values in the dataset 
###    (total number of row of NAs)

```r
missing <- activity[activity$steps == 'NA', ]
### Number of obs is num of missing values
str(missing)
```

```
## 'data.frame':	2304 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: NA NA NA NA NA NA NA NA NA NA ...
##  $ interval: int  NA NA NA NA NA NA NA NA NA NA ...
```

### 2. Imputing missing values (impute with mean).
### 3. Create a new dataset with the missing data imputed.

```r
library(Hmisc)
activity.imputed <- activity
activity.imputed$steps <- with(activity.imputed, impute(steps, mean))
```

### 4. Make a histogram of the total number of steps taken each day and 


```r
Dsum.imp <-aggregate(activity.imputed[,'steps'], by=list(activity.imputed$date), FUN=sum, na.rm=TRUE)
names(Dsum.imp) <- c("Date","Sum.step")

qplot(Sum.step, data=Dsum.imp, geom="histogram", binwidth = 1000,
      xlab="Total number of steps taken each day (after imputing)")
```

![plot of chunk imputed_SumStep](figure/imputed_SumStep-1.png) 

###    Calculate and report the mean and median total number of steps taken per day

```r
Dmean.imp <- aggregate(activity.imputed[,'steps'], by=list(activity.imputed$date), 
                       FUN=mean, na.rm=TRUE)
names(Dmean.imp) <- c("Date","Mean.step")

Dmedian.imp <- aggregate(activity.imputed[,'steps'], by=list(activity.imputed$date), 
                        FUN=median, na.rm=TRUE)
names(Dmedian.imp) <- c("Date","Median.step")

merge(Dmean.imp, Dmedian.imp, "Date")
```

```
##          Date  Mean.step Median.step
## 1  2012-10-01 37.3825996     37.3826
## 2  2012-10-02  0.4375000      0.0000
## 3  2012-10-03 39.4166667      0.0000
## 4  2012-10-04 42.0694444      0.0000
## 5  2012-10-05 46.1597222      0.0000
## 6  2012-10-06 53.5416667      0.0000
## 7  2012-10-07 38.2465278      0.0000
## 8  2012-10-08 37.3825996     37.3826
## 9  2012-10-09 44.4826389      0.0000
## 10 2012-10-10 34.3750000      0.0000
## 11 2012-10-11 35.7777778      0.0000
## 12 2012-10-12 60.3541667      0.0000
## 13 2012-10-13 43.1458333      0.0000
## 14 2012-10-14 52.4236111      0.0000
## 15 2012-10-15 35.2048611      0.0000
## 16 2012-10-16 52.3750000      0.0000
## 17 2012-10-17 46.7083333      0.0000
## 18 2012-10-18 34.9166667      0.0000
## 19 2012-10-19 41.0729167      0.0000
## 20 2012-10-20 36.0937500      0.0000
## 21 2012-10-21 30.6284722      0.0000
## 22 2012-10-22 46.7361111      0.0000
## 23 2012-10-23 30.9652778      0.0000
## 24 2012-10-24 29.0104167      0.0000
## 25 2012-10-25  8.6527778      0.0000
## 26 2012-10-26 23.5347222      0.0000
## 27 2012-10-27 35.1354167      0.0000
## 28 2012-10-28 39.7847222      0.0000
## 29 2012-10-29 17.4236111      0.0000
## 30 2012-10-30 34.0937500      0.0000
## 31 2012-10-31 53.5208333      0.0000
## 32 2012-11-01 37.3825996     37.3826
## 33 2012-11-02 36.8055556      0.0000
## 34 2012-11-03 36.7048611      0.0000
## 35 2012-11-04 37.3825996     37.3826
## 36 2012-11-05 36.2465278      0.0000
## 37 2012-11-06 28.9375000      0.0000
## 38 2012-11-07 44.7326389      0.0000
## 39 2012-11-08 11.1770833      0.0000
## 40 2012-11-09 37.3825996     37.3826
## 41 2012-11-10 37.3825996     37.3826
## 42 2012-11-11 43.7777778      0.0000
## 43 2012-11-12 37.3784722      0.0000
## 44 2012-11-13 25.4722222      0.0000
## 45 2012-11-14 37.3825996     37.3826
## 46 2012-11-15  0.1423611      0.0000
## 47 2012-11-16 18.8923611      0.0000
## 48 2012-11-17 49.7881944      0.0000
## 49 2012-11-18 52.4652778      0.0000
## 50 2012-11-19 30.6979167      0.0000
## 51 2012-11-20 15.5277778      0.0000
## 52 2012-11-21 44.3993056      0.0000
## 53 2012-11-22 70.9270833      0.0000
## 54 2012-11-23 73.5902778      0.0000
## 55 2012-11-24 50.2708333      0.0000
## 56 2012-11-25 41.0902778      0.0000
## 57 2012-11-26 38.7569444      0.0000
## 58 2012-11-27 47.3819444      0.0000
## 59 2012-11-28 35.3576389      0.0000
## 60 2012-11-29 24.4687500      0.0000
## 61 2012-11-30 37.3825996     37.3826
```

## Are there differences in activity patterns between weekdays and weekends?
### 1. Create a factor variable in the dataset with two levels- "weekday" and "weekend"

```r
weekday.yn <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

activity.imputed$date.fmt <- as.Date(activity.imputed$date)
activity.imputed$day.type <- as.factor(sapply(activity.imputed$date.fmt, FUN=weekday.yn))
```

### 2. Make a panel plot of time series plot of the 5-min interval(x) and the avg steps,
###    across weekdays or weekend days (y).

```r
meansteps <- aggregate(activity.imputed$steps, 
                       by = list(activity.imputed$interval, activity.imputed$day.type), 
                       FUN=mean)
names(meansteps) <- c("interval", "day.type", "steps")
xyplot(steps ~ interval | day.type, meansteps, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk Panel_Wkday_steps](figure/Panel_Wkday_steps-1.png) 
