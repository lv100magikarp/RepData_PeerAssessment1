Assignment 1
============

In this assignment, we conduct a simple analysis  on the data of the number of 
steps taken per 5-minute interval by an anonymous person. 

First, we load the dataset.


```r
data <- read.csv('activity.csv')
```

##Total Steps per Day
We take a brief look at the data.


```r
sums <- tapply(data$steps,data$date,sum,na.rm=T)
    hist(sums,main='Histogram of Mean Number of Steps per Day',xlab='Steps')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean <- mean(sums)
med <- median(sums)
```

The mean and median of the total number of steps taken per day are 9354.2295082 and
10395 respectively.

##Daily Walking Pattern
It may be interesting looking at the person's daily walking pattern.


```r
avg.per.interval <- tapply(data$steps,data$interval,mean,na.rm=T)
intervals <- unique(data$interval)

plot(avg.per.interval~intervals, type='l',ylab='Steps', xlab='Time interval',
     main='Average Steps at Each Time Interval Across Days')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

We can see that the person is most active at around 8am to 9am, which is most
likely because of commuting to work/school.


```r
maximum <- intervals[which(avg.per.interval==max(avg.per.interval))]
max.int <- formatC(maximum,width=4,flag='0')
max.int <- strftime(strptime(max.int,format='%H%M'),format='%H:%M')
```

The time with the most steps taken is precisely the 5-minute interval 
staring at 08:35.

##Impute Missing Data
As we can see, there are quite a few missing values in the data, however, which 
might have made the previous analyses biased.


```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
na <- unique(c(which(is.na(data$step)),which(is.na(data$interval)),
               which(is.na(data$date))))
no.na <- length(na)
```

There are 2304 rows with missing values, which is not a negligible number.

Therefore, we impute the missing values and see what happens. A reasonable way 
to do so would be to use the average number of steps for each interval as a 
substitute.


```r
#impute data
simulated <- rep(avg.per.interval,length.out=length(data$steps))
    data[na,'steps'] <- simulated[na]

#see how the histogram, mean and median have changed
sums <- tapply(data$steps,data$date,sum,na.rm=T)
hist(sums,main='Histogram of Mean Number of Steps per Day',xlab='Steps')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
mean2 <- mean(sums)
med2 <- median(sums)
```

We observe that some weights have shifted to higher numbers of steps, meaning
that the average number of steps taken per day has in general increased. This is
also reflected in the increased mean (1.0766189 &times; 10<sup>4</sup>) and median (1.0766189 &times; 10<sup>4</sup>). This
result is expected, since before imputing the missing values, the missing values
were treated similarly to 0 when calculating the average total number of steps
per day.

##Comparison Between Weekdays and Weekends
We may also be curious about the differences of walking patterns between
weekdays and weekends. Below is a graphical comparison.


```r
#create factore variable with levels weekday and weekend
weekday <- weekdays(strptime(data$date,format='%Y-%m-%d'))

days <- rep('weekday',length.out=length(data$date))
days[weekday == 'Sunday'|weekday =='Saturday'] <- 'weekend'
days <- factor(days,levels=c('weekday','weekend'))

#Merge the factor variable into the data
library(dplyr)
data <- mutate(data,w.day.or.w.end=days)

#Calculate the average for each interval for weekdays and weekends separately
data2 <- group_by(select(data,-date),w.day.or.w.end,interval)
avgSteps <- summarise(data2,avg=mean(steps))

#Plot graph
library(lattice)
xyplot(avg~interval|w.day.or.w.end,data=avgSteps,type='l',layout=c(1,2),
       ylab='Average Number of Steps',xlab='Time Interval',
       main='Comparison of Average Number of Steps')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

We notice that the person's walking activity is spread more evenly across the 
day during weekends, and more centered around the standard commuting time 
(8am - 9am) during weekdays.
