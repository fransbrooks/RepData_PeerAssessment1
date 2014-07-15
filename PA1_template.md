#Reproduceable Research
##Peer Assesment 1  
###Loading and preprocessing the data
The following code loads the relevant packages that are required for the code to run.

```r
library(data.table)
```

```
## data.table 1.9.2  For help type: help("data.table")
```

```r
library(timeDate)
```
The next chunk of code sets the working directory to the directory where my csv file containing the data is stored. It also reads in the .csv file and stores the data in a variable called "data". The data is then stored in another variable, "DataTable", as a data.table, because it is more convenient to work with a data.table object than a data.frame object  

```r
setwd("/home/frans/datasciencecoursera/RepData_PeerAssessment1")
data <- read.csv(file = "activity.csv")
DataTable <- data.table(data)
head(DataTable)
```

```
##    steps       date interval
## 1:    NA 2012-10-01        0
## 2:    NA 2012-10-01        5
## 3:    NA 2012-10-01       10
## 4:    NA 2012-10-01       15
## 5:    NA 2012-10-01       20
## 6:    NA 2012-10-01       25
```
###What is mean total number of steps taken per day?
A new data.table is formed called "StepsPerDay". This data.table contains the total number of steps taken for each day.The Column representing the total number of steps taken during a day is renamed to "TotalSteps". A histogram is plotted to show how frequently a certain range of steps are taken in a day. The mean and the median for the amount of daily steps is also computed.

```r
StepsPerDay <- DataTable[,sum(steps),by=date]
setnames(StepsPerDay,"V1","TotalSteps")
head(StepsPerDay)
```

```
##          date TotalSteps
## 1: 2012-10-01         NA
## 2: 2012-10-02        126
## 3: 2012-10-03      11352
## 4: 2012-10-04      12116
## 5: 2012-10-05      13294
## 6: 2012-10-06      15420
```

```r
hist(StepsPerDay$TotalSteps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
mean <- mean(StepsPerDay$TotalSteps,na.rm = TRUE)
median <- median(StepsPerDay$TotalSteps,na.rm = TRUE)
mean
```

```
## [1] 10766
```

```r
median
```

```
## [1] 10765
```
###What is the average daily activity pattern?
A new datatable, "AvgPerInterval", is created which contains the average number of steps per 5 min interval across all the days. NA values are ignored in this computation. The column that contains the average number of steps for an interval is renamed to "AvgNoOfSteps". The average number of steps for each interval is then plotted against the intervals.

```r
AvgPerInterval <- DataTable[,mean(steps,na.rm = TRUE),by=interval]
setnames(AvgPerInterval,"V1","AvgNoOfSteps")
head(AvgPerInterval)
```

```
##    interval AvgNoOfSteps
## 1:        0      1.71698
## 2:        5      0.33962
## 3:       10      0.13208
## 4:       15      0.15094
## 5:       20      0.07547
## 6:       25      2.09434
```

```r
plot(AvgPerInterval$interval,AvgPerInterval$AvgNoOfSteps,type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
   
It is then determined in which row the maximum value of average steps is and the row number is stored in a variable called "MaxRow". The specific interval which is contained in this row is then printed.

```r
MaxRow <- which.max(AvgPerInterval$AvgNoOfSteps)
AvgPerInterval$interval[MaxRow]
```

```
## [1] 835
```
###Inputing missing values
The total number of NA entries is displayed. A for-loop is used to loop through the "steps" value for each row in the original dataset read into R as a data.frame. If the value is NA, the value is replaced by the mean for that specific interval.The "steps" column is then converted to the numeric class. The data.frame with the missing values replaced by the mean for their respective intervals is then stored as a data.table in the variable "NewDataSet".

```r
sum(is.na(x = data$steps))
```

```
## [1] 2304
```

```r
for (i in 1:length(data$steps)){
        if(is.na(data$steps[i]==TRUE)){
                data$steps[i] <- subset(AvgPerInterval,interval==data$interval[i],AvgNoOfSteps)
        }
}
data$steps <- as.numeric(data$steps)
NewDataSet <- data.table(data)
```
A data.table is created for the total number of steps taken using the new updated data which does not include NA values. The column for the total number of steps taken during a day is renamed to "NewStepsPerDay". A histogram is plotted for the total number of steps taken using the new data and the mean and median are also recalculated

```r
NewStepsPerDay <- NewDataSet[,sum(steps),by=date]
setnames(NewStepsPerDay,"V1","TotalSteps")
hist(NewStepsPerDay$TotalSteps)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
newMean <- mean(NewStepsPerDay$TotalSteps,na.rm = FALSE)
newMedian <- median(NewStepsPerDay$TotalSteps,na.rm = FALSE)
newMean
```

```
## [1] 10766
```

```r
newMedian
```

```
## [1] 10766
```
###Are there differences in activity patterns between weekdays and weekends?
The "date" column in the new dataset is converted to the date class. A new column is inserted which tests whether the date is on a weekend or not. Saturday and Sunday are considered to be weekend days. Another new column is added to the data which says whether the day is a "weekend" or "weekday". This new column is the converted to the factor class.

```r
NewDataSet$date <- as.Date(NewDataSet$date)
NewDataSet[,DayTrueFalse:=isWeekend(date)]
```

```
##          steps       date interval DayTrueFalse
##     1: 1.71698 2012-10-01        0        FALSE
##     2: 0.33962 2012-10-01        5        FALSE
##     3: 0.13208 2012-10-01       10        FALSE
##     4: 0.15094 2012-10-01       15        FALSE
##     5: 0.07547 2012-10-01       20        FALSE
##    ---                                         
## 17564: 4.69811 2012-11-30     2335        FALSE
## 17565: 3.30189 2012-11-30     2340        FALSE
## 17566: 0.64151 2012-11-30     2345        FALSE
## 17567: 0.22642 2012-11-30     2350        FALSE
## 17568: 1.07547 2012-11-30     2355        FALSE
```

```r
NewDataSet[,DayFactor:=ifelse(NewDataSet$DayTrueFalse == TRUE, "weekend","weekday")]
```

```
##          steps       date interval DayTrueFalse DayFactor
##     1: 1.71698 2012-10-01        0        FALSE   weekday
##     2: 0.33962 2012-10-01        5        FALSE   weekday
##     3: 0.13208 2012-10-01       10        FALSE   weekday
##     4: 0.15094 2012-10-01       15        FALSE   weekday
##     5: 0.07547 2012-10-01       20        FALSE   weekday
##    ---                                                   
## 17564: 4.69811 2012-11-30     2335        FALSE   weekday
## 17565: 3.30189 2012-11-30     2340        FALSE   weekday
## 17566: 0.64151 2012-11-30     2345        FALSE   weekday
## 17567: 0.22642 2012-11-30     2350        FALSE   weekday
## 17568: 1.07547 2012-11-30     2355        FALSE   weekday
```

```r
NewDataSet$DayFactor <- as.factor(NewDataSet$DayFactor)
```
The average number of steps for each interval is then calculated for weekdays and for weekends in the variable "AvgPerWeekdayInterval". The column containing the average number of steps is renamed to "AvgNoOfSteps". This data.table is then subsetted to obtain the data for weekdays and the data for weekends. The parameters are changed so that two plots can be generated in one image. The average number of steps for an interval is then plotted against the interval for weekends and a second plot is generated which displays the average number of steps against the interval for weekdays

```r
AvgPerWeekdayInterval <- NewDataSet[,mean(steps), by=list(DayFactor,interval)]
setnames(AvgPerWeekdayInterval,"V1",new="AvgNoOfSteps")
weekData <- subset(AvgPerWeekdayInterval, DayFactor == "weekday")
weekendData <- subset(AvgPerWeekdayInterval, DayFactor == "weekend")
par(mfrow = c(2, 1))
plot(weekendData$interval,weekendData$AvgNoOfSteps,type = "l", ylim=c(0,250))
plot(weekData$interval,weekData$AvgNoOfSteps,type ="l", ylim=c(0,250))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
