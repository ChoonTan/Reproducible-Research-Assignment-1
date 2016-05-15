# Reproducible Research - Assignment 1
Choon Guan TAN  
May 15, 2016  



## Summary

This document summarises the R codes and outputs generated from analysing a sample dataset collected a personal activity monitoring device. 

This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA).

**date**: The date on which the measurement was taken in YYYY-MM-DD format.

**interval**: Identifier for the 5-minute interval in which measurement was taken.

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
 

## Part 1 - Loading and preprocessing the data

1.1 Check of data file already exists in working directory. Download and unzip if file does not exist.


```r
if(!file.exists("ActivityData.zip")) {
        temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        datafile <- unzip(temp)
        unlink(temp)
}
```

1.2. Examine the dataset


```r
Activitydata <- read.csv(datafile, header=T, sep=",", na.strings="NA", 
                        colClasses = c('integer','Date','integer')
                        )

str(Activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## Part 2 - What is the mean total number of steps taken per day, after ignoring NA values?

2.1 Calculate the total number of steps using aggregate function.


```r
totalsteps <- aggregate(steps ~ date, Activitydata, sum, na.rm=TRUE)
```

2.2 Make a histogram of the total number of steps taken each day, ignoring NA values.


```r
library(ggplot2) # Use ggplot
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
ggplot(data=totalsteps, aes(totalsteps$steps)) + 
        geom_histogram(breaks=seq(0, 25000, by = 1000), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
        labs(title="Total Number of Steps Per Day") +
        labs(x="Total Number of Steps Per Day", y="Frequency")
```

![](PA1_Template_files/figure-html/plot_totalsteps-1.png)<!-- -->

2.3 What is the mean and median of the total number of steps taken per day?


```r
mean_totalsteps <- mean(totalsteps$steps, na.rm=TRUE)
mean_totalsteps
```

```
## [1] 10766.19
```

```r
median_totalsteps <- median(totalsteps$steps, na.rm=TRUE)
median_totalsteps
```

```
## [1] 10765
```

The mean of total number of steps taken per day is
The median of total number of steps taken per day is



## Part 3 - What the average activity pattern?

3.1 Make a time series plot of the 5 minute interval and average number of steps taken, averaged across all days.


3.1.1 Calculate the average steps per interval


```r
int_meansteps <- aggregate(steps ~ interval, Activitydata, mean, na.rm=TRUE)
```


3.1.2 Plot time series of average steps per interval

```r
ggplot(int_meansteps, aes(x=interval, y=steps)) +   
        geom_line(color="green", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Avg Number of steps")
```

![](PA1_Template_files/figure-html/plot_timeseries-1.png)<!-- -->

3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_int <- int_meansteps[which.max(int_meansteps$steps),]
max_int
```

```
##     interval    steps
## 104      835 206.1698
```

The interval with the maximum number of steps taken is 835, with 206 steps taken.


## Part 4 - Inputing missing values

4.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
countNA <- sum(is.na(Activitydata$steps))
countNA
```

```
## [1] 2304
```

The number of NA in the dataset is 2304.

4.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



```r
na_fill <- function(Activitydata, pervalue) {
        na_index <- which(is.na(Activitydata$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = Activitydata[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- Activitydata$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}
```

4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
Activitydata_new <- data.frame(  
        steps = na_fill(Activitydata, int_meansteps),  
        date = Activitydata$date,  
        interval = Activitydata$interval)
```

4.3.1 Check output and confirm NA has been replaced.


```r
str(Activitydata_new)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
countNA <- sum(is.na(Activitydata_new$steps))
countNA
```

```
## [1] 0
```

4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

4.4.1 Calculate new total number of steps taken per day


```r
totalsteps2 <- aggregate(steps ~ date, Activitydata_new, sum, na.rm=TRUE)
```

4.4.2 Make a histogram of the total number of steps taken each day.


```r
ggplot(data=totalsteps2, aes(totalsteps2$steps)) + 
        geom_histogram(breaks=seq(0, 25000, by = 1000), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
        labs(title="Total Number of Steps Per Day") +
        labs(x="Total Number of Steps Per Day", y="Frequency")
```

![](PA1_Template_files/figure-html/plot_totalsteps2-1.png)<!-- -->

4.4.3 Calculate mean and median number of steps taken each day


```r
mean_totalsteps2 <- mean(totalsteps2$steps, na.rm=TRUE)
mean_totalsteps2
```

```
## [1] 10766.19
```

```r
median_totalsteps2 <- median(totalsteps2$steps, na.rm=TRUE)
median_totalsteps2
```

```
## [1] 10766.19
```

The revised mean and median total number of steps taken differs from the mean and median calculated using the old dataset (see section 2.3)

By replacing the NA values with the mean of the interval, we have effectively aligned the mean and median number of steps.


## Part 5 - Are there differences in activity patterns between weekdays and weekends?

5.1 Create a new factor variable in the dataset with two levels ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.


```r
Activitydata_new$weekdays <- factor(format(Activitydata_new$date, "%A"))

levels(Activitydata_new$weekdays) <- list(
                                        weekday = c("Monday","Tuesday","Wednesday","Thursday", "Friday"),
                                        weekend = c("Saturday", "Sunday")
                                        )

# Check levels
levels(Activitydata_new$weekdays)
```

```
## [1] "weekday" "weekend"
```

5.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


5.2.1 Calculate mean steps per interval

```r
int_meansteps_new <- aggregate(Activitydata_new$steps, 
                      list(interval = as.numeric(as.character(Activitydata_new$interval)), 
                           weekdays = Activitydata_new$weekdays),
                      FUN = "mean")

names(int_meansteps_new)[3] <- "meanintSteps"
```


5.2.2 Plot time series chart


```r
library(lattice) # Use lattice

plot4 <- xyplot(int_meansteps_new$meanintSteps ~ int_meansteps_new$interval | int_meansteps_new$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")

print(plot4)
```

![](PA1_Template_files/figure-html/plot_timeseries2-1.png)<!-- -->




