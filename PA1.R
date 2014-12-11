## loading and processing. Assume

rawdata <- read.csv("activity.csv")
## analysis makes use of lubridate and dplyr packages. 
library(lubridate)
library(dplyr)
library(tidyr)
library(lattice)

## make a data frame, remove raw data. Look at output.
data <- tbl_df(rawdata)
remove("rawdata")
data

## Tidy the data in following ways: change the order of columns so that
## identifier (interval) is first, convert date to date format, remove NA. 
## check the result
data <- select(data,interval,date,steps)
data <- mutate(data, date=ymd(date))

data

## make histogram of steps per day, find mean and median
hist(data$steps,xlab="number of steps per 5 minutes", col="yellow")

by_date <- group_by(data,date)
daily_sum <- summarize(by_date,sum=sum(steps,na.rm=TRUE))
mean_daily_sum <- mean(daily_sum$sum)
median_daily_sum <- median(daily_sum$sum)
median_daily_sum
mean_daily_sum

## plot by interval
by_interval <- group_by(data,interval)
interval_ave <- summarize(by_interval,average=mean(steps,na.rm=TRUE))
plot2 <- xyplot(average ~ interval,data=interval_ave,type="l")
print(plot2)
max_interval_ave <- max(interval_ave$average)

## work with NA
sum(is.na(data$steps))
## replace by average (mean) for the 5-minute interval. Loop over all rows.
## If value is missing, replace it with the average in interval
data_replaced <- data
for(i in 1:nrow(data)) {
    
    if (is.na(data$steps[i])) {
        row <- interval_ave[data$interval[i]==interval_ave$interval,]
        data_replaced$steps[i] <- row$average}
}

hist(data_replaced$steps,
     xlab="number of steps per 5 minutes, NA's replaced", col="green")

by_date_replaced <- group_by(data_replaced,date)
daily_sum_replaced <- summarize(by_date_replaced,sum=sum(steps,na.rm=TRUE))
mean_daily_sum_r <- mean(daily_sum_replaced$sum)
median_daily_sum_r <- median(daily_sum_replaced$sum)
mm <- as.data.frame(list(c("with NA", "without NA"),
                         c(mean_daily_sum, mean_daily_sum_r),
                   c(median_daily_sum, median_daily_sum_r)))
colnames(mm)<-c("dataset","mean","median")
mm

letter_convert <- function(char) {
    if ((char=="L")||(char=="P")) {daytype <- "weekend"}
    else {daytype <- "weekday"}
    daytype
}

data_replaced <- mutate(data_replaced, dayletter=weekdays(date,abbreviate=TRUE),
                        daytype="weekday")
for(i in 1:nrow(data_replaced)) {
    if (data_replaced$dayletter[i]=="L"|data_replaced$dayletter[i]=="P") {
        data_replaced$daytype[i] <- "weekend"
    }
}

by_interval_new <- group_by(data_replaced, daytype,interval)
blergh <- summarize(by_interval_new,average=mean(steps))
blergh <- transform(blergh, daytype = factor(daytype))

## Make plot with lattice system
plot4 <- xyplot(average~interval|daytype, data = blergh, layout=c(1,2))
print(plot4)