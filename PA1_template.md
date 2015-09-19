---
title: "Reproducible Research: Peer Assessment 1"
author: Filippo Quarta
output: 
html_document:
keep_md: true
---


## Loading and preprocessing the data
```{r}
library("plyr")
library("lubridate")
library("ggplot2")

options(digit=10)
Sys.setlocale("LC_TIME", "English")

fileName <- "activity"
fileNameZip <- paste(fileName,".zip",sep = "")
fileNameCsv <- paste(fileName,".csv", sep = "")
data <- NULL
if (file.exists(fileNameZip)) {
  unzip(fileNameZip)
  data <- read.csv(fileNameCsv)
} else {
  print("File is not present.")
}
data$date <- ymd(data$date)
dataComp <- data[complete.cases(data),]
```

## What is mean total number of steps taken per day?
```{r}
tmp <- aggregate(dataComp$steps, by=list(dataComp$date), FUN=sum)
colnames(tmp) <- c("date","steps")


avg <- mean(tmp$steps)
med <- median(tmp$steps)
hist(tmp$step,breaks=10, xlab = "Steps", main="Histogram of total steps per day")
```

**Mean Total Steps/Day** =`r format(avg,digits=10)`  
**Median Total Steps/Day**=`r format(med,digits=10)`

## What is the average daily activity pattern?

The daily pattern is obtained calculating the average of steps over all available dates ( the one with no NAs).

```{r}
dap <- aggregate(dataComp$steps, by=list(dataComp$interval), FUN=mean)

colnames(dap) <- c("interval","steps")
g1 <- ggplot(dap, aes(x=interval,y=steps))
g1 <- g1 + geom_line(color="blue")
plot(g1)

```

Interval with maximum steps in average:
```{r}
dap[dap$step==max(dap$step),"interval"]
```


## Imputing missing values

In the original data are present **`r sum(is.na(data$step))`** observations with null steps. Missing values are filled using the averaged steps for corresponding interval from Daily Activity Pattern.  

```{r}
dataFill <- data
dataFill <- join(dataFill, dap, by=c("interval"))
colnames(dataFill) <- c("steps","date","interval","steps1")
dataFill$steps <- ifelse(is.na(dataFill$steps), dataFill$steps1, dataFill$steps)
dataFill <- dataFill[,1:3]

tmp <- aggregate(dataFill$steps, by=list(dataFill$date), FUN=sum)
colnames(tmp) <- c("date","steps")

avg <- mean(tmp$steps)
med <- median(tmp$steps)
hist(tmp$step,breaks = 10,xlab = "Steps", main="Histogram of total steps per day")
```

**Mean Total Steps/Day** =`r format(avg,digits=10)`  
**Median Total Steps/Day**=`r format(med,digits=10)`  

These values are different from the ones obtained without substituting NA values. Median and Mean of total steps per day are now equal.

## Are there differences in activity patterns between weekdays and weekends?

```{r}
dataFill$daytype <- as.factor(ifelse(weekdays(dataFill$date) %in% c("Saturday","Sunday"), "weekend", "weekday"))
tmp <- split(dataFill, dataFill$daytype)
tmp <- lapply(tmp, FUN = function(x) aggregate(x$steps,by=list(x$interval,x$daytype), FUN=mean))
tmp <- rbind(tmp$weekday,tmp$weekend)
colnames(tmp) <- c("interval","daytype","steps")

sp <- ggplot(tmp, aes(y=steps, x=interval))
sp <- sp + geom_line()
sp <- sp + facet_grid(daytype ~ .)

plot(sp)
```
