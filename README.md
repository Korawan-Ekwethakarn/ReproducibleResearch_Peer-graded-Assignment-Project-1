# ReproducibleResearch_Peer-graded-Assignment-Project-1
Korawan Ekwethakarn
September 25,2020
Activity monitoring devices
This is my submission for Reproducible Research Course Project 1. To read more information view the ReadMe on GitHub.

##Assignment Instructions
1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

Loading and preprocessing the data
Show any code that is needed to
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
+         temp <- tempfile()
+         download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
+         unzip(temp)
+         unlink(temp)
+ }
trying URL 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
Content type 'application/zip' length 53559 bytes (52 KB)
downloaded 52 KB

> 
> activity<-read.csv("activity.csv")
> head(activity)
  steps       date interval
1    NA 2012-10-01        0
2    NA 2012-10-01        5
3    NA 2012-10-01       10
4    NA 2012-10-01       15
5    NA 2012-10-01       20
6    NA 2012-10-01       25
> totalStepsByDay<-aggregate(steps~date, activity, sum)
> hist(totalStepsByDay$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Total Number of Steps taken each day")
> mean_raw<-mean(totalStepsByDay$steps)
> mean_raw
[1] 10766.19
> median_raw<-median(totalStepsByDay$steps)
> median_raw
[1] 10765
> averageStepsbyInterval<-aggregate(steps~interval, activity, mean)
> with(averageStepsbyInterval, plot(interval, steps, type = "l"))
> averageStepsbyInterval[which.max(averageStepsbyInterval[,2]),1]
[1] 835
> missingIndex<-is.na(activity[,1])
> m<-mean(averageStepsbyInterval$steps)
> activity1<-activity
> activity1[missingIndex,1]<-m
> head(activity1)
    steps       date interval
1 37.3826 2012-10-01        0
2 37.3826 2012-10-01        5
3 37.3826 2012-10-01       10
4 37.3826 2012-10-01       15
5 37.3826 2012-10-01       20
6 37.3826 2012-10-01       25
> totalStepsByDay1<-aggregate(steps~date, activity1, sum)
> hist(totalStepsByDay1$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")
> totalStepsByDay1<-aggregate(steps~date, activity1, sum)
> mean_afterImput<-mean(totalStepsByDay1$steps)
> mean_afterImput
[1] 10766.19
> median_afterImput<-median(totalStepsByDay1$steps)
> median_afterImput
[1] 10766.19
> activity1$date<-as.Date(activity1$date)
> library(dplyr)
> activity2<-activity1%>%
+         mutate(dayType= ifelse(weekdays(activity1$date)=="Saturday" | weekdays(activity1$date)=="Sunday", "Weekend", "Weekday"))
> head(activity2)
    steps       date interval dayType
1 37.3826 2012-10-01        0 Weekday
2 37.3826 2012-10-01        5 Weekday
3 37.3826 2012-10-01       10 Weekday
4 37.3826 2012-10-01       15 Weekday
5 37.3826 2012-10-01       20 Weekday
6 37.3826 2012-10-01       25 Weekday
> averageStepByDayTypeAndInterval<-activity2 %>%
+   group_by(dayType, interval) %>%
+   summarize(averageStepByDay=sum(steps))
`summarise()` regrouping output by 'dayType' (override with `.groups` argument)
> 
> head(averageStepByDayTypeAndInterval)
# A tibble: 6 x 3
# Groups:   dayType [1]
  dayType interval averageStepByDay
  <chr>      <int>            <dbl>
1 Weekday        0             315.
2 Weekday        5             242.
3 Weekday       10             231.
4 Weekday       15             232.
5 Weekday       20             228.
6 Weekday       25             283.
> library(lattice)
> with(averageStepByDayTypeAndInterval, 
+       xyplot(averageStepByDay ~ interval | dayType, 
+       type = "l",      
+       main = "Total Number of Steps within Intervals by dayType",
+       xlab = "Daily Intervals",
+       ylab = "Average Number of Steps"))
> 
