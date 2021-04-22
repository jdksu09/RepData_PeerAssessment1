---
title: "Course Project 1"
author: "David Hale"
date: "4/19/2021"
output: 
  html_document: 
    keep_md: yes
---




## Intro

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

## Reading in the data

The first step is to read in the data set.


```r
data <- read.csv("activity.csv")
```

## What is the mean total of steps taken per day?

First we'll look at a histogram of the total steps taken per day.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data%>%
  group_by(date) %>%
  summarize(total.steps = sum(steps)) -> sumdata
hist(sumdata$total.steps, xlab = "Total steps per day", main = "Histogram of Total Steps Per Day")
```

![](PA1_template_files/figure-html/data-1.png)<!-- -->

Next, we'll figure the mean and median total steps taken per day.  Missing values are left out for this part.


```r
mean(sumdata$total.steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(sumdata$total.steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
data%>%
  group_by(interval)%>%
  summarize(average.steps = mean(steps, na.rm = T)) -> intervaldata
plot(intervaldata$interval, intervaldata$average.steps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/data2-1.png)<!-- -->

Next we'll determine which 5-minute interval has the highest number of steps on average across all days.


```r
intervaldata[which.max(intervaldata$average.steps),]
```

```
## # A tibble: 1 x 2
##   interval average.steps
##      <int>         <dbl>
## 1      835          206.
```
You can see the 835 5-minute interval has the highest average number of steps across all days at 206.

## Imputing missing values

We'll determine the total number of missing values in the data.


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Next we'll replace the missing values in the data.  The strategy I've chosen is to replace missing values with the average number of steps for that 5-minute interval.


```r
data %>%
  group_by(interval)%>%
  mutate(interval.avg = mean(steps, na.rm = T)) -> newdata

impute.data <- within(newdata, steps[is.na(steps)] <- interval.avg[is.na(steps)])
impute.data <- select(impute.data, "steps", "date", "interval")
```

Next we'll plot a histogram of the total steps taken per day for the same data set with missing values replaced.


```r
impute.data%>%
  group_by(date) %>%
  summarize(total.steps = sum(steps)) -> sumdata2
hist(sumdata2$total.steps, xlab = "Total steps per day", main = "Histogram of Total Steps Per Day")
```

![](PA1_template_files/figure-html/data3-1.png)<!-- -->

Again, we'll calculate the mean and median on the data with missing values replaced.


```r
mean(sumdata2$total.steps)
```

```
## [1] 10766.19
```

```r
median(sumdata2$total.steps)
```

```
## [1] 10766.19
```

So the total daily number of steps doesn't chang a lot with the imputed missing values.  The mean is the same, and median went up slightly and is equal to the mean.

## Are there difference in activity patterns between weekends and weekdays?

We'll add a factor variable to the date identifying days as weekend or weekday:


```r
impute.data <- as.data.frame(impute.data)
impute.data$date <- as.Date(as.character(impute.data$date), format = "%m/%d/%Y")
impute.data <- mutate(impute.data, day = weekdays(impute.data$date))
impute.data2 <- within(impute.data, day <- ifelse(impute.data$day == "Saturday" | impute.data$day == "Sunday", "weekend", "weekday"))
```

Now we'll do a time series panel plot showing average number of steps per 5-minute interval across weekends and weekdays


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
impute.data2%>%
  group_by(interval, day)%>%
  summarize("avg.steps" = mean(steps)) -> intervaldata2
qplot(interval,avg.steps, data=intervaldata2, geom = "line", facets = .~day)
```

<img src="PA1_template_files/figure-html/data4-1.png" width="100%" />
