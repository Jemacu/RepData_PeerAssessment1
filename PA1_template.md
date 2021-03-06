---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

### Loading and preprocessing the data
##### Check if data or zip file is in directory with the .Rmd file  
    - if yes, load the data from the activity.csv file or unzip the compressed file and load the data  
    - if not, download the data, unzip and then load data

```r
zipUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

#check if data file is available and if not, download
if (!file.exists("activity.csv") & !file.exists("activity.zip")){
      if(!file.exists("activity.zip")){
            setInternet2(use=TRUE)
            download.file(zipUrl, destfile="activity.zip")
      }
}
if (!file.exists("activity.csv")){
      unzip("activity.zip")
}
activity_data <- read.csv("activity.csv")
```

### What is the mean total number of steps taken per day?

```r
#Create a table of the total steps per day
daysums <- aggregate(steps ~ date, data=activity_data, FUN=sum)
# Create histogram of the steps per day
hist(daysums$steps, breaks=10, main="Histogram of Steps/Day", xlab="average steps/day", ylim=c(0,25))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

##### Average number of total steps per day:

```r
#Calculate mean  and median total steps per day
options(scipen=3) # this prevents R from converting the number to scientific notation
meansteps <- round(mean(daysums$steps), 2)
meansteps
```

```
## [1] 10766.19
```

```r
mediansteps <- median(daysums$steps)
mediansteps
```

```
## [1] 10765
```

The total mean steps per day is **10766.19** and the median is **10765**.
  

### What is the average daily activity pattern?


```r
#Calculate and plot the average steps for each 5 minute interval for all days
interval_means <- aggregate(steps ~ interval, data=activity_data, FUN=mean)
plot(interval_means, type="l", main="Average daily activity pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


```r
#Calculate the 5 minute interval with the highest average number of steps
max_steps <- interval_means[interval_means$steps==max(interval_means$steps),"interval"]
max_steps
```

```
## [1] 835
```
The 5-minute interval that, averaged across all days, has the maximum number of steps is interval **835**.

### Imputing missing values


```r
# Calculate the total number of missing values
NAcount <- nrow(activity_data[is.na(activity_data$steps),])
NAcount
```

```
## [1] 2304
```
The total number of missing values is **2304**.   

##### Fill in missing values:
For each time interval with a missing value, the average of available values for that time interval is used.


```r
#convert data.frame to data.table, fill in missing values with the mean of 
#available values for each time interval
library(data.table)
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
```

```r
activity_data_dt <- data.table(activity_data)
activity_data_dt <- activity_data_dt[, intmeans:=mean(steps, na.rm=TRUE), by=interval]
activity_data_dt$steps <- ifelse(is.na(activity_data_dt$steps), activity_data_dt$intmeans, activity_data_dt$steps)

# Create histogram of steps taken each day
daysums2 <- aggregate(steps ~ date, data=activity_data_dt, FUN=sum, na.rm=TRUE)
hist(daysums2$steps, breaks=10, main="Histogram of Steps/Day", xlab="average steps/day", ylim=c(0,25))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 


```r
# Calculate mean steps per day with NAs replaced with imputed values 
mean_steps_nna <- round(mean(daysums2$steps), 2)
mean_steps_nna
```

```
## [1] 10766.19
```


```r
# Calculate the median steps per day with NAs replaced with imputed values 
median_steps_nna <- round(median(daysums2$steps), 2)
median_steps_nna
```

```
## [1] 10766.19
```

The mean steps per day after missing values have been replaced with imputed values is **10766.19** and the median is also **10766.19**.

Even though the total number of steps is increased, since the missing values for each interval are the average of available values, the mean value does not change.  
The median value changes to the mean value because all of the days that had missing values were missing all values and therefore ended up having the mean value as their total steps/day.

### Are there differences in activity patterns between weekdays and weekends?

```r
#Convert date field to date type
activity_data_dt$date <- as.Date(as.character(activity_data_dt$date))
#add day type field with values weekend (for Saturday and Sunday) and weekday for all other values
activity_data_dt$daytype <- ifelse(weekdays(activity_data_dt$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
#convert to factor
activity_data_dt$daytype <- as.factor(activity_data_dt$daytype)
#Subset the data then calculate the average value for each time interval
weekdays <- subset(activity_data_dt, daytype=="weekday")
weekends <- subset(activity_data_dt, daytype=="weekend")
weekday_avs <- aggregate(steps~interval, data=weekdays, FUN=mean)
weekend_avs <- aggregate(steps~interval, data=weekends, FUN=mean)
#Create plot of subsets
par(mfrow=c(2,1), oma = c(0,4,0,0),mar=c(4,3,4,2))
plot(weekend_avs, type="l", main="Weekend", xlab="", ylab="")
plot(weekday_avs, type="l", main="Weekday", xlab="Interval", ylab="")
mtext("Number of Steps", side=2, outer=TRUE)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

There definitely appear to be differences in the weekday pattern compared to the weekend. For example, on weekdays, time intervals between roughly 500 and 800 all have average step counts near 50 or higher but on the weekend, the same time intervals have counts starting very low and gradually increasing over the (roughly) 300 hundred time intervals.  
On the other hand, time intervals between roughly 1000 and 2000 mostly have much higher counts for the weekend than during the week as well as a larger range of values.  
Both have a spike with highest values around the 835th interval but the spike is quite a bit higher for the weekdays (~230 steps) than weekends (~167 steps).
