# Reproducible Research - Peer Assessment 1


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
library(scales)
```

```
## Warning: package 'scales' was built under R version 3.2.5
```

```r
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.2.5
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

### Load the data (i.e. read.csv())

```r
activityData <- read.csv('activity.csv')
```

### Calculate the total number of steps taken per day

```r
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

### Make a histogram of the total number of steps taken everyday

```r
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Calculate and report the mean and median of the total number of steps taken per day


```r
print(paste("Mean: ",mean(stepsByDay)))
```

```
## [1] "Mean:  9354.22950819672"
```

```r
print(paste("Median: ",median(stepsByDay)))
```

```
## [1] "Median:  10395"
```


#### Average Daily Activity Pattern

```r
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)
```

### Time Series Plot

```r
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Maximum number of steps

```r
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])

print(paste("Most Steps: ",timeMostSteps))
```

```
## [1] "Most Steps:  8:35"
```

### Calculate and report the total number of missing values in the dataset

```r
numMissingValues <- length(which(is.na(activityData$steps)))
```

## * Number of missing values: r numMissingValues

### Devise a strategy for filling in all of the missing values in the dataset.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
```

### Make a histogram of the total number of steps taken each day

```r
stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Calculate and report the mean and median total number of steps taken per day.

```r
print(paste("Mean:  ", mean(stepsByDayImputed)))
```

```
## [1] "Mean:   10766.1886792453"
```

```r
print(paste("Median: ", median(stepsByDayImputed)))
```

```
## [1] "Median:  10766.1886792453"
```


### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

### Make a panel plot containing a time series plot

```r
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
