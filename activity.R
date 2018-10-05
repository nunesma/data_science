setwd("~/Cursos/5 - Reproducible Research/Week 1 - Concepts, Ideas, & Structure/Assignment - Course Project 1/Repro_Research_-Project_1")
getwd()

# 1 - Code for reading in the dataset and/or processing the data
actv<-read.csv("activity.csv", sep = ",")

str(actv)
summary(actv)
head(actv)

# 2 - Histogram of the total number of steps taken each day
steps_day <- aggregate(steps ~ date, data = actv, sum, na.rm = T)
hist(steps_day$steps, breaks = 20, main = "Total number of steps per day", xlab = "Steps", ylab = "Frequency")


# 3 - Mean and median number of steps taken each day
mean(steps_day$steps)
median(steps_day$steps)

# 4 - Time series plot of the average number of steps taken

steps_interval <- aggregate(steps ~ interval, data = actv, mean, na.rm = T)
plot( steps_interval$interval, steps_interval$steps,type = "l", main = "Steps per 5 min interval", xlab = "5 min interval", ylab = "Steps")

# 5 - The 5-minute interval that, on average, contains the maximum number of steps
which.max(steps_interval$steps)
steps_interval[104,]

# 6 - Code to describe and show a strategy for imputing missing data
sum(is.na(actv$steps))
actv_i <- actv
actv_i$steps[is.na(actv_i$steps)] <- mean(actv_i$steps, na.rm = T)
sum(is.na(actv_i$steps))

# 7 - Histogram of the total number of steps taken each day after missing values are imputed
steps_day_i <- aggregate(steps ~ date, data = actv_i, sum, na.rm = T)
hist(steps_day_i$steps, breaks = 20, main = "Total number of steps per day - missing data imputed", xlab = "Steps", ylab = "Frequency")
mean(steps_day_i$steps)
median(steps_day_i$steps)

# 8 - Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
actv_i$date <- as.Date(actv_i$date)
actv_i$dayname <- weekdays(actv_i$date)
actv_i$weekend <- as.factor(ifelse(actv_i$dayname == "sábado" | actv_i$dayname == "domingo", "weekend", "weekday"))
library(lattice)
plotdata <- aggregate(steps ~ interval + weekend, actv_i, mean)
xyplot(steps ~ interval | factor(weekend), layout = c(1, 2), data=plotdata, aspect=1/3, type="l")
