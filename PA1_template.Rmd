---
title: 'Reproducible Research: Peer Assessment 1'
output: html_document
---

## Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data
```{r loaddata,echo=TRUE}
setwd("D:/01 Programming/02 R/01 Johns Hopkings-Coursera/05 Reproducible Research/02 Week 2/05 Project/repdata_data_activity")
activity <- read.csv("activity.csv",header = TRUE,sep = ",",na.strings = "NA")
```

## What is the mean total number of steps taken per day?
```{r,echo=TRUE}
library(ggplot2)
StepsPerDay <- tapply(activity$steps,activity$date ,FUN = sum,na.rm = TRUE)
qplot(StepsPerDay,binwidth=1000)
mean(StepsPerDay,na.rm = TRUE)
median(StepsPerDay,na.rm = TRUE)
```

## What is the average daily acivity pattern?
Making a time series plot
```{r,echo=TRUE}
AveragesSteps <- aggregate(list(steps=activity$steps),by=list(activity$interval),FUN = mean,na.rm = TRUE)
ggplot(data = AveragesSteps,aes(x=AveragesSteps$Group.1,y=AveragesSteps$steps))+geom_line()+xlab('5-minute interval')+ylab('average of steps')
```

Which 5-minute interval,on average across all the days in the dataset, contains the maximun number of steps?
```{r,echo=TRUE}
AveragesSteps[which.max(AveragesSteps$steps),]
```

## Imputin missing values
The presence of missing days may introduce bias into calculations or summaries of the data.
```{r,echo=TRUE}
sum(is.na(activity$steps))
```
Estrategy for filling the missing values: filled with the mean value for that 5-minute interval. The new dataset is called 'flag_activity'
```{r,echo=TRUE}
library(dplyr)
flag_activity <- activity %>% 
	group_by(interval) %>% 
		mutate(steps=ifelse(is.na(steps),mean(steps,na.rm = TRUE),steps))
sum(is.na(flag_activity$steps))## NA's have been filled.
```
New histogram created with the new dataset.
```{r,echo=TRUE}
Flag_StepsPerDay <- tapply(flag_activity$steps,flag_activity$date ,FUN = sum,na.rm = TRUE) ## Obtaining the new steps per day.
qplot(Flag_StepsPerDay,binwidth=1000,xlab='total number of steps taken each day')
mean(Flag_StepsPerDay)
median(Flag_StepsPerDay)
```
The mean and median are higher because in the first data set the NA's where 0 by default, and now we are replacing them with values greater than 0.

## Are there differences in activity patterns between weekdays and weekends?
We use the new dataset
```{r,echo=TRUE}
## Adding a column that tells us the day of the week.
flag_activity$date <- as.Date(flag_activity$date)
flag_activity$day <- weekdays(flag_activity$date)
## Creating a string vectors with the days of the week. (In my case the days are in spanish)
days_week <- c('lunes','martes','miércoles','jueves','viernes')
days_weekend <- c('sábado','domingo')
## Adding a column called 'week' to know if a day is weekday or weekend
flag_activity$week <- ifelse(flag_activity$day %in% days_week,'weekday','weekend')
```
Now we create the time series plot
```{r,echo=TRUE}
Flag_AveragesSteps <- aggregate(flag_activity$steps~flag_activity$interval + flag_activity$week,FUN = mean)
## Changing the columns names
colnames(Flag_AveragesSteps) <- c('interval','week','steps')
ggplot(data = Flag_AveragesSteps,aes(Flag_AveragesSteps$interval,Flag_AveragesSteps$steps)) + geom_line() + facet_grid(Flag_AveragesSteps$week~.) + xlab('5 minute interval') + ylab('Number of steps')
```