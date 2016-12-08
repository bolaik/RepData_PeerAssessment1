## unzip and read file
if(!file.exists("activity.csv")) {
  unzip("activity.zip")
}

activity <- read.csv("activity.csv")

## total number of steps per day
steps_per_day <- with(activity, tapply(steps, date, sum, na.rm = TRUE))

## histogram of steps per day
hist(steps_per_day, xlab = "Number of Steps", main = "Histogram of Total Steps Per Day")

## mean and median of steps per day
summary(steps_per_day, digits = 6)

## mean steps as a function of five-minute interval
steps_5min <- with(activity, tapply(steps, interval, mean, na.rm= TRUE))
plot(names(steps_5min), steps_5min, type = "l",
     xlab = "Time Intervals", ylab = "Number of Steps",
     main = "Time Series Plot of Average Number of Steps")

max_steps_5min <- max(steps_5min)
names(steps_5min[steps_5min == max_steps_5min])

## impute missing values
sum(is.na(activity$steps))

na_or_not <- is.na(activity$steps)
interval_id <- as.factor(activity$interval[na_or_not])
activity$steps[na_or_not] <- steps_5min[interval_id]

## replot histogram of steps per day with the new clean dataset
steps_per_day <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
hist(steps_per_day, xlab = "Number of Steps", main = "Histogram of Total Steps Per Day")
summary(steps_per_day, digits = 6)

## weekdays vs weekends
library(chron)
library(dplyr)
wday_or_wend <- is.weekend(as.Date(activity$date))
activity <- mutate(activity, weekday.or.weekend = wday_or_wend)
activity$weekday.or.weekend[wday_or_wend] <- "weekday"
activity$weekday.or.weekend[!wday_or_wend] <- "weekend"

library(lattice)
steps_5min_wday <- aggregate(activity$steps, 
                             by = list(interval = activity$interval, 
                                       weekday.or.weekend = activity$weekday.or.weekend), 
                             FUN = mean)
xyplot(x ~ interval | weekday.or.weekend, data = steps_5min_wday, 
       type = "l", layout = c(1,2), ylab = "Number of Steps",
       main = "Times Series Plot of Steps in Weekdays vs Weekends")