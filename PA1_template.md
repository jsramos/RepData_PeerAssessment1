# Reproducible Research: Peer Assessment 1
This peer-assessed work is part of the evaluation for the 'Reproducible Research' MOOC.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The questions that this analysis aims to answer are:

1. What's the mean steps per day?
2. What's the avg daily activity pattern?
3. How could we impute missing values?
4. Are the activity patterns during weekdays from weekends?

## Loading and preprocessing the data
A good practice is to make sure input data files are there, and if not, download them. We're not showing results here for convenience reasons. We're also converting the date column of type character to POSIX with format 'year-month-day'.


```r
options(scipen = 1, digits = 2) # Set scientific notation digits

# Load required libraries
if (!require(lubridate)) {stop('Package lubridate must be installed before proceeding.')}
if (!require(dplyr)) {stop('Package dplyr must be installed before proceeding.')}
if (!require(ggplot2)) {stop('Package ggplot2 must be installed before proceeding.')}

# Download and process data
downloadedFilename <- 'activity.zip'
dataFilename <- 'activity.csv'
if (!file.exists(dataFilename)) {
    dataUrl <- paste('https://d396qusza40orc.cloudfront.net/repdata/data/',filename, sep = '')
    download.file(dataUrl, destfile = downloadedFilename)
    unzip(filename, files = dataFilename)
}

activity <- read.csv('activity.csv', stringsAsFactors = F, 
                     colClasses = c('numeric', 'character', 'numeric'))

# Convert date from character to posix
activity <- activity %>% mutate(date=ymd(date))
```


## What is mean total number of steps taken per day?
For this exercise the data were grouped by date, then a histogram was plotted. Vertical lines were added at the mean and median values:

```r
steps <- activity %>% group_by(date) %>% summarise(totalsteps=sum(steps, na.rm = T))

# Median and mean
meansteps <- mean(steps$totalsteps, na.rm = T)
mediansteps <- median(steps$totalsteps, na.rm = T)

ggplot(aes(x=totalsteps), data=steps) + 
  geom_histogram(fill='blue', col='black', binwidth=800, show_guide=TRUE) + 
  geom_vline(data=steps, aes(xintercept=meansteps), color='red') + 
  geom_vline(data=steps, aes(xintercept=mediansteps), color='#009900') +
  labs(x='Total steps measured each day (bin=800 steps)', 
      y='Number of days with X step count', 
      title='Frequency of total steps measured each day') +
  annotate('text', label=paste('Mean=',round(meansteps,2)), x=meansteps - 4000, y=10, color='red') +
  annotate('text', label=paste('Median=',mediansteps), x=mediansteps + 4000, y=10, color='#009900')
```

![](PA1_template_files/figure-html/meanstepsday-1.png) 

From the histogram we can conclude:

* There are a little under 10 days with 0 steps.
* The distribution is skewed to the right.
* This skewness usually causes the mean to be greater than the median, but in this case, the outlier at 0 steps pulls the mean to the left.

Finally, the mean and median around the central tendency of the distribution are 9354.23 and 10395, respectively.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
