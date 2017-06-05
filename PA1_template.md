# PA1_template
Deepa McGriff  
June 4, 2017  



*Peer Graded Assignment 1*

Loading and preprocessing the data:
Here the data is loaded from a directory, and the date field is formatted for easier manipulation


```r
setwd("H:/Coursera Data Specialization Course/Reproducible Research/HW1")
activity <- read.csv("activity.csv")
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
#Recode date field
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.3.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity$Date <- ymd(activity$date)


#Create a new variable to indicate day of week for each date
activity$weekday <- weekdays(activity$Date)
```

*Part 1*: What is mean total number of steps taken per day?

This section will calculate the sum of steps for each day in the dataset.

1. Calculate the total number of steps taken per day, by grouping observations by date and calculating the sum


```r
a <- activity%>%
  group_by(Date) %>%
  summarise(daily_steps = sum(steps))
```
2. Make a histogram of the total steps taken each day

```r
library(ggplot2)
ggplot(a, aes(x = daily_steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/part1.2_histogram-1.png)<!-- -->

3. Calculate and report the mean/median of the total number of steps taken per day

```r
summary(a$daily_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

*Part 2*: What is the average daily activity pattern?

This section will look at each 5-minutes time interval and calculate average number of steps at each interval

1. Make a time series plot of the 5-min interval and the avg number of steps taken, averaged across all days


```r
#Create simplified dataset and eliminate NA values for steps
simple2 = activity[,c("steps", "interval")]
simple3 = subset(simple2, !is.na(steps))

#calculate average steps taken per 5-min interval
b <- simple3%>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps))

#Plot average steps per 5 min interval
plot(b$interval, b$average_steps, type="l")
```

![](PA1_template_files/figure-html/part2.1_time_series_plot-1.png)<!-- -->

2. Which 5-min interval (across all days) contains the max number of steps?


```r
#Print interval with highest average steps
b <- b[order(-b$average_steps),]
head(b)
```

```
## # A tibble: 6 × 2
##   interval average_steps
##      <int>         <dbl>
## 1      835      206.1698
## 2      840      195.9245
## 3      850      183.3962
## 4      845      179.5660
## 5      830      177.3019
## 6      820      171.1509
```
The time interval with the maximum steps is 835, with an average of 206.1698 steps

*Part 3*: Imputing Missing Values

1. Calculate and report total number of missing values:


```r
nrow(activity[!complete.cases(activity$steps),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all missing values

This analysis will use the average number of steps at each 5-minute time interval and impute the corresponding value in any missing data points.

3. Create a new dataset with missing data filled in

```r
complete <- activity
nas <- is.na(complete$steps)
avg_interval <- tapply(complete$steps, complete$interval, mean, na.rm=TRUE, simplify=TRUE)
#Replace NAs with average steps per 5 min interval
complete$steps[nas] <- avg_interval[as.character(complete$interval[nas])]
```

4. Make a histogram of the total number of steps taken each day, calculate the mean and median total steps per day.


```r
a.2 <- complete%>%
    group_by(Date) %>%
   summarise(daily_steps = sum(steps))
#Plot histogram of number of steps per day
ggplot(a.2, aes(x = daily_steps)) +
    geom_histogram(fill = "blue", binwidth = 1000) +
    labs(title = "Histogram of Steps per day, imputed missing values", x = "Steps per day", y = "Frequency")
```

![](PA1_template_files/figure-html/part3.4_histogram-1.png)<!-- -->

```r
#Calculate mean/median steps per day
summary(a.2$daily_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
This adjusted analysis shifted the median closer to the mean but did not impact the overall distribution much.

*Part 4*: Are there differences in activity patterns between weekdays and weekends?

This section will separate the dataset into weekdays/weekends and compare the two datasets side by side.

1. Create a new factor variable with 2 levels- weekday and weekend

```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$wDay <- factor((activity$weekday %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
simple4 = activity[,c("wDay", "steps", "interval")]
simple5 = subset(simple4, !is.na(steps))
wkday = subset(simple5, wDay=="weekday")
wkend = subset(simple5, wDay=="weekend")
```

2. Make a panel plot with time series of 5-minute intervals and average steps taken per interval, over weekdays and weekends


```r
#summarize both datasets with average steps per 5-min interval
wd <- wkday%>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps))

we <- wkend%>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps))

#Plot panels of weekday and weekend time series of average steps per time interval
par(mfrow=c(2,1))
plot(wd$interval, wd$average_steps, type="l", main="Weekdays")
plot(we$interval, we$average_steps, type="l", main="Weekends")
```

![](PA1_template_files/figure-html/part4.2_time_series_plot_panel-1.png)<!-- -->
