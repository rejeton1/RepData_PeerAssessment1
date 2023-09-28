##install needed package
install.packages('dplyr')
install.packages('ggplot2')
library(stats)
library(base)
library(dplyr)
library(ggplot2)

##Data load
activity <- read.csv('activity.csv')

################
##Caculate mean Total number of steps per day
total_step_per_day <- activity %>% group_by(date) %>% 
  summarize(total=sum(steps, na.rm = TRUE))

##plot it
hist(total_step_per_day$total, 
     xlab = 'Steps', ylab = 'Frequency(days)',
     main = 'Total number of steps per day')

##Calculate the mean and median of the total number of steps per day
mean_steps_per_day <- mean(total_step_per_day$total)
median_steps_per_day <- median(total_step_per_day$total)

##################
##make time-series plot of average number of steps per 
##5minutes of all day
aver_steps_per_5min <- activity %>% group_by(interval) %>%
  summarize(aver = mean(steps, na.rm = TRUE))

with(aver_steps_per_5min, 
     plot(interval, aver, type = 'l', xlab='Interval',
          ylab='Steps', main='Average number of steps per 5-minutes'))

##Find max aver step interval
interval_max <- 
  aver_steps_per_5min$interval[which.max(aver_steps_per_5min$aver)]
##################
##Imputing Missing Values
##Calculate and report the total number of NAs in the dataset
total_NA <- sum(is.na(activity$steps))

##filling in all of the missing values in the dataset.
##(average number of steps per 5-minutes interval)
NA_data <- activity[is.na(activity$steps),]

for(i in 1:2304){
  NA_data$steps[i] <- aver_steps_per_5min$aver[aver_steps_per_5min$interval 
                                               == NA_data$interval[i]]
}

##Create a new dataset that is equal to the original dataset
##but with the missing data filled in.
New_activity <- activity
New_activity[is.na(activity$steps),] <- NA_data


##Checking all NA values are filled
sum(is.na(New_activity))

##plotting histogram and calculate mean and median again
New_total_steps_per_day <- New_activity %>% group_by(date) %>%
  summarize(total=sum(steps))

hist(New_total_steps_per_day$total, 
     xlab = 'Steps', ylab = 'Frequency(days)',
     main = 'Total number of steps per day')

New_mean_per_day <- mean(New_total_steps_per_day$total)
New_median_per_day <- median(New_total_steps_per_day$total)
##################
##Are there differences in activity patterns 
##between weekdays and weekends?

##Create a new factor variable with two levels Weekdays and Weekends
weekday <- c('월요일', '화요일', '수요일', '목요일', '금요일')
weekend <- c('토요일', '일요일')
New_activity$date <- as.Date(New_activity$date, format = "%Y-%m-%d")

for(i in 1:nrow(New_activity)){
  if(weekdays(New_activity$date[i]) %in% weekday){
    New_activity$level[i] <- 'weekday'
  } else if(weekdays(New_activity$date[i]) %in% weekend){
    New_activity$level[i] <- 'weekend'
  }
}

##plotting time-series panel plot across 2 levels(weekday, weekend)
two_level_activity <- New_activity %>% group_by(level, interval) %>% 
  summarize(average=mean(steps))

g <- qplot(interval, average, data = two_level_activity, geom = 'line',
      facets=level~., 
      color=level, xlab = 'Interval', ylab = 'Steps',
      main = 'Average number of steps per 5-minutes interval')
g + theme(plot.title=element_text(hjust=0.5))


########################