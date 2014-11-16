# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
```

## What is the mean total number of steps taken per day?

Histogram of the total number of steps:

```r
steps.number <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps.number$steps, names.arg=steps.number$date, xlab="date", ylab="steps")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

The mean and median number of steps:

```r
mean(steps.number$steps)
```

```
## [1] 10766
```

```r
median(steps.number$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Time series plot of the 5 minute interval and the average number of steps taken:

```r
steps.interval <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(steps.interval, type="l")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

5 minute interval that contains the maximum number of steps:


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```


## Imputing missing values

Total number of missing values:

```r
sum(is.na(activity))
```

```
## [1] 2304
```


The strategy is to use means to impute the missing values.


```r
activity <- merge(activity, steps.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[,c(1:3)]
```

The histogram, mean and median:


```r
steps.date <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(steps.date$steps, names.arg=steps.date$date, xlab="date", ylab="steps")
```

![plot of chunk unnamed-chunk-8](./PA1_template_files/figure-html/unnamed-chunk-8.png) 

```r
mean(steps.date$steps)
```

```
## [1] 10766
```

```r
median(steps.date$steps)
```

```
## [1] 10766
```

The impact of the missing data is minimal.

## Are there differences in activity patterns between weekdays and weekends?


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

Panel plot containing time series plot:


```r
steps_aggregate <- aggregate(steps~interval+daytype,data = activity,mean)
library(lattice)
xyplot(steps~interval | daytype, data=steps_aggregate, type='l', as.table=FALSE,  layout = c(1, 2))
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 

