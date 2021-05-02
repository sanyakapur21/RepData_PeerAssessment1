##Loading and preprocessing data

library(ggplot2)
#Read file
activity <- read.csv("./data/activity.csv")
#Load data
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
summary(activity)


##1)What is mean total number of steps taken per day?

##Reading data
tot_steps<-with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm=TRUE))
names(tot_steps)<-c("date", "steps")
#Plotting
hist(tot_steps$steps,main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkcblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))
#Mean No of Steps
mean(activity_tot_steps$steps)  ## 9354.23


##2)What is the average daily activity pattern?

avg_act<-aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(avg_act) <- c("interval", "mean")
#Plotting
plot(avg_act$interval, avg_act$mean, type = "l", col="blue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
avg_act[which.max(avg_act$mean), ]$interval  ##835


##3)Imputing missing values

##Calculate and report the total no of missing values
sum(is.na(activity$steps)) ## 2304
# Come up with a plan to fill all missing values 
imp_steps<-avg_act$mean[match(activity$interval, avg_act$interval)]
# Create a dataset that is same as original but without missing values
act_imp <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imp_steps,no = activity$steps))
steps_imp<- aggregate(steps ~ date, act_imp,sum)
names(steps_imp) <- c("date", "daily_steps")
#Plotting
hist(steps_imp$daily_steps, col = "blue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
#Mean no of steps
mean(steps_imp$daily_steps) ##10766.19


##4)Are there differences in activity patterns between weekdays and weekends?

activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})
#Plotting
act_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(act_by_date, aes(x = interval , y = steps, color = datetype)) +geom_line() + labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
