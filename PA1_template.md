# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Read the data

```r
library(plyr)
library(lattice)
data <- read.csv("./Reproducable_Research/activity.csv", colClasses = c("numeric", "character", "numeric"),
                 header = TRUE, sep = ",", na.strings=c("NA"))
```

process the data for initial analysis by removing NA's

```r
clean_data <-data[complete.cases(data),]

sums<-ddply(clean_data, .(date), summarise, Total_Steps=sum(steps))
```


Make a histogram of the total number of steps taken each day

```r
hist(sums$Total_Steps, xlab= "Total_Steps", 
     main = "Steps per Day",
     col ="red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Calculate and report the mean and median total number of steps taken per day

```r
sums<-ddply(clean_data, .(date), summarise, Total_Steps=sum(steps))

mean(sums$Total_Steps)
```

```
## [1] 10766
```

```r
median(sums$Total_Steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Make a time series plot

```r
Interval_Avg<-ddply(clean_data, .(interval), summarise, Avg_Steps=mean(steps))

plot(Interval_Avg$interval, Interval_Avg$Avg_Steps,
     ylab="Avg Daily Steps", type="l", lwd=1, xlab="5 Min interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Which interval contains the max

```r
which.max(Interval_Avg$Avg_Steps)
```

```
## [1] 104
```

## Imputing missing values
calculate the number of NAs

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Devise a strategy for filling NAs:
Using the average of the interval across all days to fill in the steps na for the coresponding NA

make a new dataset with missing values filled in

```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

filled_dataset <- ddply(data, ~ interval, transform, steps = impute.mean(steps))
filled_sums<-ddply(filled_dataset, .(date), summarise, Daily_Steps=sum(steps))
```


 make a histogram of Total number of steps per day

```r
hist(filled_sums$Daily_Steps, xlab= "Total_Daily_Steps", 
     main = "Steps per Day",
)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

Calculate and report the mean and median total number of steps taken per day

```r
mean(filled_dataset$steps , na.rm=TRUE)
```

```
## [1] 37.38
```

```r
median(filled_dataset$steps, na.rm=TRUE)
```

```
## [1] 0
```
The impact of imputing missing data on estimate has greatly reduced both calculations

## Are there differences in activity patterns between weekdays and weekends?
Create a new variable

```r
filled_dataset$date <- weekdays(as.Date(filled_dataset$date))
filled_dataset$date <- ifelse((filled_dataset$date == "Saturday") | (filled_dataset$date == "Sunday"),"weekend","weekday")  
```

make a panel plot

```r
aggregated_filled_data = aggregate(steps ~ interval + date, filled_dataset, mean)
xyplot(steps ~ interval | factor(date), data = aggregated_filled_data, 
       type = "l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
