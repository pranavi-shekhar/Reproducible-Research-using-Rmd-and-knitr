library(dplyr)
library(chron)

#1
data  = read.csv("activity.csv")
data$date = as.Date(data$date,format = "%Y-%m-%d")

# 2 and 3

totalsteps = filter(data , !is.na(steps)) %>% aggregate(steps ~ date , data=.,FUN = sum)
par(lwd = 3)
hist(totalsteps$steps , col = "darkslateblue" , main = "Total daily steps" , xlab = "Steps")

summary(totalsteps$totalsteps)

# 4 and 5

avgsteps = filter(data , !is.na(steps)) %>% group_by(interval)  %>% summarise(avg.steps = mean(steps))

plot(avgsteps$interval,avgsteps$avg.steps,type = "l", xlab = "Intervals", ylab = "Average steps" , main = "Average number of steps in each interval across all days")

filter(avgsteps , avg.steps == max(avgsteps$avg.steps))

# 6

sum(!complete.cases(data))

imputed.data = data %>% group_by(interval)  %>% mutate(steps=ifelse(is.na(steps),mean(steps,na.rm=TRUE),steps))

#7

totalsteps.imputed = aggregate(steps ~ date , data = imputed.data , FUN = sum)
par(lwd = 3)
hist(totalsteps.imputed$totalsteps , col = "darkslateblue" , main = "Total daily steps" , xlab = "Steps")


#8

week.data = mutate(imputed.data , day_type = ifelse(is.weekend(date) , "Weekend","Weekday")) 
week.data =  aggregate(steps ~ interval + day_type , data = week.data, FUN = mean)

xyplot(steps~interval|day_type , week.data , type="l" , layout = c(1,2) , xlab = "Interval" , ylab = "Number of steps" , main = "Average number of steps across each interval on weekdays and weekends")
 
