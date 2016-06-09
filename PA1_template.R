setwd("c://datascience/RRp")
getwd()

### Load the data using read.csv())
#Dataset: Activity monitoring data [52K]
data <- read.csv("activity.csv")

## What is mean total number of steps taken per day?
#Sum steps by day and create Histogram
stps_by_day <- aggregate(steps ~ date, data, sum)
hist(stps_by_day$steps, main=paste("Total steps by day"),col="blue",xlab="Number of steps")

#calculate mean and median.
smean <- mean(stps_by_day$steps)
smedian <- median(stps_by_day$steps)

# What is the average daily activity pattern?
#Calculate average steps for each interval for all days.
stps_by_intval <- aggregate(steps ~ interval,data, mean)
#Plot Average # of Steps per Day by Interval and interval
plot(stps_by_intval$interval,stps_by_intval$steps, type ="l",xlab="Interval",ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
max_interval <- stps_by_intval[which.max(stps_by_intval$steps),1]

#Imputing missing values 
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), stps_by_intval$steps[match(data$interval, stps_by_intval$interval)], data$steps))
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0

#Recount total steps by day and create Histogram.
stps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(stps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

#Create Histogram to show difference
hist(stps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
smean.i <- mean(stps_by_day_i$steps) 
smedian.i <- median(stps_by_day_i$steps)

#Calculate new mean and median for imputed data.
mean_diff <- smean.i -smean
median_diff <- smedian.i -smedian

#Calculate total difference.
tot_diff <- sum(stps_by_day_i$steps)- sum(stps_by_day$steps)

#Are there differences in activity patterns between weekdays and weekends?

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
                "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
stps_by_intval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(stps_by_intval_i$steps ~ stps_by_intval_i$interval|stps_by_intval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
 
