activity<-read.csv("D:\\Swirl course\\repdata%2Fdata%2Factivity\\activity.csv")
sapply(activity, class)
activity$date<-as.Date(activity$date, format="%Y-%m-%d")
colMeans(is.na(activity))
steps_day <- aggregate(steps ~ date, rm.na = TRUE, data = activity, FUN = sum)
#histogram for the total number of steps taken by each day
plot(steps_day, type = "h", lwd = 10, lend = "square")

#calculate mean and median report of the total number steps taken per day

aggregate(steps ~ date, data = activity, FUN = mean)
aggregate(steps ~ date, data = activity, FUN = median)

plot(aggregate(steps ~ interval, data = activity, FUN = mean), type = "l")

max(activity$steps, na.rm = TRUE)

sum(is.na(activity))

activity2 <- activity
sapply(activity2, class)

activity2$steps[is.na(activity2$steps)] <- mean(na.omit(activity$steps))
activity2$date <- as.Date(activity2$date, format = "%Y-%m-%d")

steps_day2 <- aggregate(steps ~ date, rm.na = TRUE, data = activity2, FUN = sum)

par(mfrow = c(1, 2))
plot(steps_day, type = "h", lwd = 5,lend = "square", main = "With NAs")
abline(h = seq(0, 20000, 2500), lty = "dashed")
plot(steps_day2, type = "h", lwd = 5, lend = "square", main = "NAs filled")
abline(h = seq(0, 20000, 2500), lty = "dashed")



aggregate(steps ~ date, data = activity, FUN = mean)
aggregate(steps ~ date, data = activity, FUN = median)
aggregate(steps ~ date, data = activity2, FUN = mean)
aggregate(steps ~ date, data = activity2, FUN = median)


activity2$weekday <- factor(format(activity2$date, "%A"))

levels(activity2$weekday) <- list(weekday = c("Monday", "Tuesday",
                                              "Wednesday", "Thursday",
                                              "Friday"), weekend =
                                    c("Saturday", "Sunday"))


par(mfrow = c(2, 1))

with(activity2[activity2$weekday == "weekend",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekends"))

with(activity2[activity2$weekday == "weekday",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekdays"))
