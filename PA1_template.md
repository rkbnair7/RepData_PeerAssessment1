
<!-- --- -->
<!-- title: 'Reproducible Research: Peer Assessment 1' -->
<!-- output: -->
<!--   html_document: -->
<!--     keep_md: yes -->

<!-- --- -->


<!-- Start -->

```r
library(knitr)
library(dplyr)
library(ggplot2)
# opts_chunk$set(echo = TRUE)

setwd("C:\\DS\\Week2\\RepData_PeerAssessment1-master")
```

Loading/ preprocessing the data

```r
dr<- read.csv('activity.csv')
activity_data <- dr[ with (dr, { !(is.na(steps)) } ), ]
```

What is the mean total number of steps taken per day?

```r
activity_data_by_day <- group_by(activity_data, date)
stepsByDay <- summarise(activity_data_by_day, total = sum(steps))
stepsByDay
```

```
## # A tibble: 53 x 2
##    date       total
##    <fct>      <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # ... with 43 more rows
```


```r
hist(stepsByDay$total, main="Total steps per day", 
     xlab="Total steps in a day",col="green")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
summary(stepsByDay)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

What is the average daily activity pattern?

```r
stepsByInterval <- aggregate(steps ~ interval, activity_data, mean)

plot(stepsByInterval$interval, stepsByInterval$steps, type='l', 
     main="Avg steps - all days", xlab="Interval", 
     ylab="Avg steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)


```r
Maximum_steps_no <- which.max(steps_by_interval$steps)
```

```
## Error in which.max(steps_by_interval$steps): object 'steps_by_interval' not found
```

```r
stepsByInterval[Maximum_steps_no, ]
```

```
## Error in `[.data.frame`(stepsByInterval, Maximum_steps_no, ): object 'Maximum_steps_no' not found
```

Impute missing values

```r
sum(is.na(dr))
```

```
## [1] 2304
```

```r
da_imputed <- dr
for (i in 1:nrow(da_imputed)) {
  if (is.na(da_imputed$steps[i])) {
    interval <- da_imputed$interval[i]
    steps <- stepsByInterval[
      stepsByInterval$interval == interval,]
    da_imputed$steps[i] <- steps$steps
  }
}

impStepByDday <- aggregate(steps ~ date, da_imputed, sum)
head(impStepByDday)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(impStepByDday$steps, main="Total No. of steps / day (imputed)", 
     xlab="Total #  of steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
mean(impStepByDday$steps)
```

```
## [1] 10766.19
```

```r
median(impStepByDday$steps)
```

```
## [1] 10766.19
```

```r
mean(stepsByDay$total)
```

```
## [1] 10766.19
```

```r
median(stepsByDay$total)
```

```
## [1] 10765
```

Are there differences in activity patterns between weekdays and weekends?

```r
WeekDay <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        rtn <- 'Weekday'
    } else {
        rtn <- 'Weekend'
    }
    rtn
}
```

<!-- # Apply the week_day function and add a new column to activity dataset -->
<!-- # generating aggregated data frame  -->
Showing the chart

```r
dr$day_type <- as.factor(sapply(dr$date, WeekDay))


stepsPerDayImpute <- aggregate(steps ~ interval+day_type, dr, mean)


chrt <- ggplot(stepsPerDayImpute, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("# of steps/Interval by daytype")

print(chrt)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)


