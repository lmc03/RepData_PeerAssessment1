---
title: "Reproducible Research Course Assignment 1"
author: "lmc03"
date: "3/25/2022"
output: html_document
---

## Reproducible Research Course Assignment 1  
1. Code for reading in the dataset and/or processing the data  
2. Histogram of the total number of steps taken each day  
3. Mean and median number of steps taken each day  
4. Time series plot of the average number of steps taken  
5. The 5-minute interval that, on average, contains the maximum number of steps  
6. Code to describe and show a strategy for imputing missing data  
7. Histogram of the total number of steps taken each day after missing values are imputed  
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends  
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report  
  
  
## 1. Code for reading in the dataset and/or processing the data 
activity.csv is read into the variable activity.
```{r, echo = TRUE}
library("lubridate")
library("ggplot2")
activity <- read.csv("activity.csv")
head(activity)
```

## 2. Histogram of the total number of steps taken each day 
Histogram is created by first aggregating the steps by the date.
```{r, echo = TRUE}
totalStepsPerDay <- aggregate(activity$steps, list(activity$date), sum, na.rm = TRUE)
hist(totalStepsPerDay$x, main = "Histogram of Total Steps Per Day", xlab = "Total Steps Taken Per Day", ylab = "Frequency")
```

## 3. Mean and median number of steps taken each day  
Mean and median are outputted in this section.
```{r, echo = TRUE}
mean <- mean(totalStepsPerDay$x)
mean
median <- median(totalStepsPerDay$x)
median
```

## 4. Time series plot of the average number of steps taken  
The time series plot is shown by first aggregating the number of steps by the intervals.
```{r, echo=TRUE}
aveDailyActivityPattern <- aggregate(activity$steps, list(activity$interval), mean, na.rm = TRUE)
colnames(aveDailyActivityPattern) <- c('Interval', 'Average')
plot(aveDailyActivityPattern$Interval, aveDailyActivityPattern$Average, type = "l", main = "Average Daily Activity Pattern", xlab = "Interval", ylab = "Average Number of Steps")
```

## 5. The 5-minute interval that, on average, contains the maximum number of steps 
Outputs the 5-min interval that contains the maximum number of steps.
```{r, echo = TRUE}
aveDailyActivityPattern[which.max(aveDailyActivityPattern$Average), ]$Interval
```

## 6. Code to describe and show a strategy for imputing missing data  
The strategy implemented was to impute the mean for the specific 5-minute interval. First, the number of rows with missing values is outputted. The original dataset is then copied into the impute variable. Then, the average interval value is placed into every match between activity interval and aveDailyActivityPattern Interval. Lastly, the average interval value is imputed to the steps column if the value is NA.
```{r, echo = TRUE}
sum(is.na(activity)) #get the number of rows with missing values
impute <- activity #copy the original dataset
impute$imputeInterval <- aveDailyActivityPattern$Average[match(activity$interval, aveDailyActivityPattern$Interval)] #get the average interval value and put its value into every interval it matches
imputeData <- transform(impute,steps = ifelse(is.na(impute$steps),impute$imputeInterval, activity$steps)) #impute by using transform to change the steps values to whatever comes out of ifelse
imputeData <- subset(imputeData, select = c(0:3)) #get a subset of the imputeData
head(imputeData)
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed  
The Histogram shows the total number of steps taken each day after the missing values are imputed. The resulting mean and median are both higher compared to the original values computed earlier, which shows the impact of the imputed values.
```{r, echo = TRUE}
totalStepsPerDay1 <- aggregate(imputeData$steps, list(imputeData$date), sum, na.rm = TRUE)
colnames(totalStepsPerDay1) <- c('Date', 'totalSteps')
totalStepsPerDay1
hist(totalStepsPerDay1$totalSteps, main = "Histogram of Total Steps Per Day", xlab = "Total Steps Taken Per Day", ylab = "Frequency")
mean1 <- mean(totalStepsPerDay1$totalSteps)
mean1
median1 <- median(totalStepsPerDay1$totalSteps)
median1
```

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends 
The panel plot shows the average number of steps taken per 5-minute interval across weekdays and weekends.
```{r, echo = TRUE}
imputeData$date <- as.Date(imputeData$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
imputeData$weekdayorweekend <- factor((weekdays(imputeData$date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

activities <- aggregate(list(imputeData$steps), by = list(imputeData$interval, imputeData$weekdayorweekend), mean, na.rm = TRUE)
colnames(activities) <- c('interval', 'weekdayorweekend', 'totalsteps')

g <- ggplot(activities, aes(x = interval, y = totalsteps, color = weekdayorweekend))
plot1 <- g + geom_line() + facet_grid(weekdayorweekend~.) + labs(title = "Average Steps for Weekdays and Weekends") + xlab("Interval") + ylab("Average Number of Steps")
plot1
```

