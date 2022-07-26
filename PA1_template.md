---
title: "Course Project 1"
author: "John Magcamit"
date: '2022-07-26'
output:
  html_document:
    keep_md: true
---




## Loading and preprocessing the data

```r
library(dplyr)
df <- read.csv("activity.csv", sep = ",", colClasses = c("numeric", "Date", "numeric"))
df$date <- as.POSIXct(df$date, "%Y%m%d")
```

```r
head(df, 10)
```

```
##    steps                date interval
## 1     NA 2012-10-01 08:00:00        0
## 2     NA 2012-10-01 08:00:00        5
## 3     NA 2012-10-01 08:00:00       10
## 4     NA 2012-10-01 08:00:00       15
## 5     NA 2012-10-01 08:00:00       20
## 6     NA 2012-10-01 08:00:00       25
## 7     NA 2012-10-01 08:00:00       30
## 8     NA 2012-10-01 08:00:00       35
## 9     NA 2012-10-01 08:00:00       40
## 10    NA 2012-10-01 08:00:00       45
```

## Total number of steps taken each day
Total number of steps each day

```r
df1 <- df %>% group_by(date) %>% summarize(TotalSteps = sum(steps))
head(df1, 10)
```

```
## # A tibble: 10 × 2
##    date                TotalSteps
##    <dttm>                   <dbl>
##  1 2012-10-01 08:00:00         NA
##  2 2012-10-02 08:00:00        126
##  3 2012-10-03 08:00:00      11352
##  4 2012-10-04 08:00:00      12116
##  5 2012-10-05 08:00:00      13294
##  6 2012-10-06 08:00:00      15420
##  7 2012-10-07 08:00:00      11015
##  8 2012-10-08 08:00:00         NA
##  9 2012-10-09 08:00:00      12811
## 10 2012-10-10 08:00:00       9900
```
Histogram of Total Steps per Day

```r
hist(df1$TotalSteps, xlab="Number of steps taken each day", col="green", breaks = length(df1$date)/4, main = "Histogram of Total Steps per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Mean and median number of steps taken each day (excluding NA)


```r
averageSteps <- mean(df1$TotalSteps, na.rm = TRUE)
medianSteps <- median(df1$TotalSteps, na.rm = TRUE)
```


```
## [1] "Average steps:  10766.1886792453"
```

```
## [1] "Median steps:  10765"
```
## Average daily activity pattern


```r
df2 <- df %>% group_by(interval) %>% summarize(aveSteps=mean(steps, na.rm=TRUE))
head(df2, 10)
```

```
## # A tibble: 10 × 2
##    interval aveSteps
##       <dbl>    <dbl>
##  1        0   1.72  
##  2        5   0.340 
##  3       10   0.132 
##  4       15   0.151 
##  5       20   0.0755
##  6       25   2.09  
##  7       30   0.528 
##  8       35   0.868 
##  9       40   0     
## 10       45   1.47
```
Time series plot

```r
plot(df2$interval, df2$aveSteps, type="l", xlab="5-Minute Interval", ylab="Average Number of Steps", main = "Time series plot of the 5-Minute Interval and average number of steps")  
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
maxSteps <- which.max(df2$aveSteps)
df2[maxSteps,2]
```

```
## # A tibble: 1 × 1
##   aveSteps
##      <dbl>
## 1     206.
```
Maximum steps for 5 minute interval is 206

## Imputing missing values
Total number of missing values in the dataset

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```


```r
newdf <- df
newdf$steps[which(is.na(newdf$steps) & newdf$interval==df2$interval)] <- df2$aveSteps
head(newdf, 10)
```

```
##        steps                date interval
## 1  1.7169811 2012-10-01 08:00:00        0
## 2  0.3396226 2012-10-01 08:00:00        5
## 3  0.1320755 2012-10-01 08:00:00       10
## 4  0.1509434 2012-10-01 08:00:00       15
## 5  0.0754717 2012-10-01 08:00:00       20
## 6  2.0943396 2012-10-01 08:00:00       25
## 7  0.5283019 2012-10-01 08:00:00       30
## 8  0.8679245 2012-10-01 08:00:00       35
## 9  0.0000000 2012-10-01 08:00:00       40
## 10 1.4716981 2012-10-01 08:00:00       45
```

Histogram of the total number of steps taken each day after missing values are imputed

```r
df3 <- newdf %>% group_by(date) %>% summarize(TotalSteps = sum(steps))
hist(df3$TotalSteps, xlab="Number of steps taken each day", col="green", breaks = length(df1$date)/4, main = "Histogram of Total Steps per Day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

Mean and median of total number of steps per day

```r
mean(df3$TotalSteps)
```

```
## [1] 10766.19
```

```r
median(df3$TotalSteps)
```

```
## [1] 10766.19
```

##  Differences in activity patterns between weekdays and weekends

Using newdf as dataset with the filled-in missing values

```r
df4 <- newdf
weekend <- c("Saturday", "Sunday")
df4$day <- weekdays(df4$date)
df4$day[df4$day=="Saturday" | df4$day=="Sunday"] <- "weekend"
weekendDF <- df4 %>% filter(df4$day=="weekend") %>% group_by(interval) %>% summarize(aveSteps=mean(steps))
weekdayDF <- df4 %>% filter(df4$day!="weekend") %>% group_by(interval) %>% summarize(aveSteps=mean(steps))
```

Plotting the weekend vs weekday average steps/day

```r
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(weekendDF$interval, weekendDF$aveSteps, type="l", main="Weekend", col="blue", xlab="5-Minute Intervals", ylab="Average Steps per Day")
plot(weekdayDF$interval, weekdayDF$aveSteps, type="l", main="Weekday", col="red", xlab="5-Minute Intervals", ylab="Average Steps per Day")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)
