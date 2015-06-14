# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


For this analysis I will use the following libraries (packages):


```r
library(dplyr, warn.conflicts = FALSE)
library(knitr)
library(ggplot2)
library(grid)
```


Read raw data file into R. I'll actually do this twice - one data frame will treat all zero values as NA values. I will calculate means / medians with zeroes and without.


```r
dataReader <- read.csv("./activity.csv", na.strings = c(0, "NA"))
dataReader0 <- read.csv("./activity.csv")
```


Create groups of data by day and by interval and summarise to get required statistics:


```r
dataByDateWith0 <- summarise(group_by(dataReader0, date), sum(steps, na.rm = T),
                  mean(steps, na.rm = T), median(steps, na.rm = T))
dataByDateNo0 <- summarise(group_by(dataReader, date), 
                           mean(steps, na.rm = T), median(steps, na.rm = T))
dataByInterval <- summarise(group_by(dataReader0, interval), 
                            mean(steps, na.rm = T))
```


Use descriptive names as column headers:
                  

```r
colnames(dataByDateWith0) <- c("date", "sum", "mean-with-0s", "median-with-0s")
colnames(dataByDateNo0) <- c("date", "mean-no-0s", "median-no-0s")
colnames(dataByInterval) <- c("interval", "mean")
```


## What is mean total number of steps taken per day?


Plot a histogram of total number of steps taken daily:


```r
hist(dataByDateWith0$sum, xlab = "Count", main = "Total Steps, Oct - Nov 2012, Daily")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Combine data into one summary table:


```r
dataByDate <- cbind(dataByDateWith0[, 1:3], dataByDateNo0[, 2],
                    dataByDateWith0[, 4], dataByDateNo0[, 3])
```

View() is a nice function to view daily statistics of steps, but it doesn't work in R Markdown, so I will use kable() function from the knitr package instead. This produces a table "knit HTML" is run:


```r
kable(dataByDate, digits = 2)
```



date            sum   mean-with-0s   mean-no-0s   median-with-0s   median-no-0s
-----------  ------  -------------  -----------  ---------------  -------------
2012-10-01        0             NA           NA               NA             NA
2012-10-02      126           0.44        63.00                0           63.0
2012-10-03    11352          39.42       140.15                0           61.0
2012-10-04    12116          42.07       121.16                0           56.5
2012-10-05    13294          46.16       154.58                0           66.0
2012-10-06    15420          53.54       145.47                0           67.0
2012-10-07    11015          38.25       101.99                0           52.5
2012-10-08        0            NaN          NaN               NA             NA
2012-10-09    12811          44.48       134.85                0           48.0
2012-10-10     9900          34.38        95.19                0           56.5
2012-10-11    10304          35.78       137.39                0           35.0
2012-10-12    17382          60.35       156.59                0           46.0
2012-10-13    12426          43.15       119.48                0           45.5
2012-10-14    15098          52.42       160.62                0           60.5
2012-10-15    10139          35.20       131.68                0           54.0
2012-10-16    15084          52.38       157.12                0           64.0
2012-10-17    13452          46.71       152.86                0           61.5
2012-10-18    10056          34.92       152.36                0           52.5
2012-10-19    11829          41.07       127.19                0           74.0
2012-10-20    10395          36.09       125.24                0           49.0
2012-10-21     8821          30.63        96.93                0           48.0
2012-10-22    13460          46.74       154.71                0           52.0
2012-10-23     8918          30.97       101.34                0           56.0
2012-10-24     8355          29.01       104.44                0           51.5
2012-10-25     2492           8.65        56.64                0           35.0
2012-10-26     6778          23.53        77.02                0           36.5
2012-10-27    10119          35.14       134.92                0           72.0
2012-10-28    11458          39.78       110.17                0           61.0
2012-10-29     5018          17.42        80.94                0           54.5
2012-10-30     9819          34.09       110.33                0           40.0
2012-10-31    15414          53.52       179.23                0           83.5
2012-11-01        0            NaN          NaN               NA             NA
2012-11-02    10600          36.81       143.24                0           55.5
2012-11-03    10571          36.70       117.46                0           59.0
2012-11-04        0            NaN          NaN               NA             NA
2012-11-05    10439          36.25       141.07                0           66.0
2012-11-06     8334          28.94       100.41                0           52.0
2012-11-07    12883          44.73       135.61                0           58.0
2012-11-08     3219          11.18        61.90                0           42.5
2012-11-09        0            NaN          NaN               NA             NA
2012-11-10        0            NaN          NaN               NA             NA
2012-11-11    12608          43.78       132.72                0           55.0
2012-11-12    10765          37.38       156.01                0           42.0
2012-11-13     7336          25.47        90.57                0           57.0
2012-11-14        0            NaN          NaN               NA             NA
2012-11-15       41           0.14        20.50                0           20.5
2012-11-16     5441          18.89        89.20                0           43.0
2012-11-17    14339          49.79       183.83                0           65.5
2012-11-18    15110          52.47       162.47                0           80.0
2012-11-19     8841          30.70       117.88                0           34.0
2012-11-20     4472          15.53        95.15                0           58.0
2012-11-21    12787          44.40       188.04                0           55.0
2012-11-22    20427          70.93       177.63                0           65.0
2012-11-23    21194          73.59       252.31                0          113.0
2012-11-24    14478          50.27       176.56                0           65.5
2012-11-25    11834          41.09       140.88                0           84.0
2012-11-26    11162          38.76       128.30                0           53.0
2012-11-27    13646          47.38       158.67                0           57.0
2012-11-28    10183          35.36       212.15                0           70.0
2012-11-29     7047          24.47       110.11                0           44.5
2012-11-30        0            NaN          NaN               NA             NA


## What is the average daily activity pattern?


Plot a time series with mean steps on the y-axis and intervals on the x-axis:


```r
plot(dataByInterval$mean ~ dataByInterval$interval, type = "l", main = "Mean Steps,
     Oct - Nov 2012, by 5 min Interval", xlab = "Interval", ylab = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 


See which interval has most steps:


```r
dataByInterval[which(dataByInterval$mean == max(dataByInterval$mean, na.rm = T)), ]
```

```
## Source: local data frame [1 x 2]
## 
##   interval     mean
## 1      835 206.1698
```

As you can see, interval between 835 and 840 minutes has most average steps at approximately 206.


## Imputing missing values

Let's get a number of rows with missing values:


```r
sum(!complete.cases(dataReader0))
```

```
## [1] 2304
```

8 days have all NAs or zeroes as evidenced by the following piece of code. Daily mean of NA is probably because the tracking device was not on. Daily mean of NaN is probably because the user did not wear the device all day, so it was recording zero steps.


```r
daysMissingData <- dataByDate$date[which(!complete.cases(dataByDate))]
daysMissingData
```

```
## [1] 2012-10-01 2012-10-08 2012-11-01 2012-11-04 2012-11-09 2012-11-10
## [7] 2012-11-14 2012-11-30
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```

First, I am going to copy my original data set with zeroes to a new data set that I will work with to replace missing values:


```r
newData <- dataReader0
```

I think the proper way to replace NAs for these days is to assign value of daily mean of the next day. This is the best option as I do not think assigning zero is realistic unless this person was actually confined to a bed 8 days out of the 2 month period.

What I am doing next is looping over NA days, and if my original data rows match those NA days, then I am assigning the mean steps from the following day (divided by the number of intervals, or 288), to every NA interval.


```r
for (i in seq_along(daysMissingData)){
  for (j in seq(nrow(newData))){
  if (newData$date[j] == daysMissingData[i]){
    newData$steps[j] <- dataByDate[[3]][which(dataByDate$date == daysMissingData[i]) + 1]/288
    }
  }
}
```

After running this we still have to take care of 2012-11-09 (Because 2012-11-10 was also NA), and the last day, (because there is no following day in the data set).


```r
for (i in 11233:11520){
  newData$steps[i] <- newData$steps[11521]/288
}
for (i in 17281:17568){
  newData$steps[i] <- dataByDate[[3]][60]/288
}
```

Doing a quick check for any missing values reveals that they are all taken care of:


```r
sum(!complete.cases(newData))
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?

Create an empty vector and then loop over all rows to transform dates to days of the week:


```r
day = character()
for(i in seq(nrow(newData))){
    if(weekdays(as.Date(newData$date[i])) == "Saturday"
       | weekdays(as.Date(newData$date[i])) == "Sunday"){
               day[i] <- "weekend"
           } else {
               day[i] <- "weekday" 
           }

}
```

Add this vector to the dataset:


```r
newData <- cbind(newData, day)
```

Create 2 groups of data summarised by mean of steps for each interval, use descriptive names for 2nd column:


```r
newDataWeekday <- summarise(group_by(filter(newData, day == "weekday"), interval), mean(steps))
newDataWeekend <- summarise(group_by(filter(newData, day == "weekend"), interval), mean(steps))
colnames(newDataWeekday)[2] <- "mean"
colnames(newDataWeekend)[2] <- "mean"
```

Create two plots to view weekday activity next to weekend activity:


```r
p1 <- ggplot(newDataWeekday, aes(x = interval, y = mean)) +
    geom_line() +
    ggtitle("Weekday")
p2 <- ggplot(newDataWeekend, aes(x = interval, y = mean)) +
    geom_line() +
    ggtitle("Weekend")
pushViewport(viewport(layout = grid.layout(2, 1)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png) 

Now isn't that interesting. If you only look at the scale it appears that this person walks more on weekdays, but if you follow the 100 level, there is a clear break through to 200 for a few intervals, but then it almost never reaches this level. On the contrary, weekend intervals never reach 200, but they breach the 100 steps level quite a few times, and there is more variability.
