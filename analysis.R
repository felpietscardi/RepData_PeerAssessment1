# 1. Load the data (i.e. `read.csv()`)
fileUrl         <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
dowloadedFile   <- "repdata_data_activity.zip"

if (!file.exists(dowloadedFile)) {
    download.file(url = fileUrl, destfile = dowloadedFile)
    unzip(dowloadedFile)
}

# 2. Process/transform the data (if necessary) into a format suitable for your analysis
library(dplyr)
library(ggplot2)

activityData <- read.csv(file = "activity.csv")

# 1. Make a histogram of the total number of steps taken each day
stepSummary <- activityData %>% na.omit() %>% group_by(date) %>% summarise(steps = sum(steps))

qplot(stepSummary$steps)

# 2. Calculate and report the **mean** and **median** total number of steps taken per day
activityData %>% na.omit() %>% group_by(date) %>% summarise(mean(steps), median(steps))


# 1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the 
# average number of steps taken, averaged across all days (y-axis)

activityDataOmit <- activityData %>% na.omit()
intervalAvg <- activityDataOmit %>% group_by(interval) %>% summarise(avg = mean(steps))

ggplot(data=intervalAvg, aes(x=interval, y=avg)) +
    geom_line() +
    xlab("5-Minute Interval") +
    ylab("Average number of steps taken") +
    ggtitle("Average Number of Steps Taken Over All Days")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

intervalAvg[which.max(intervalAvg$avg), ]

# Note that there are a number of days/intervals where there are missing
# values (coded as `NA`). The presence of missing days may introduce
# bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

count(activityData[is.na(activityData), ])

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.

assumedData <- activityData
# calculate the avg step
activityRemoved <- activityData[which(!is.na(activityData$steps)),]
dailyActivity <- tapply(activityRemoved$steps, activityRemoved$interval, mean)
# replace the value
assumedData[which(is.na(assumedData$steps)), 1] <- 
    dailyActivity[as.character(assumedData[which(is.na(assumedData$steps)),3])]

count(assumedData[is.na(assumedData), ])

http://stackoverflow.com/questions/26336122/r-replacing-na-values-by-mean-of-hour-with-dplyr

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
stepsByDayAssumed <- tapply(assumedData$steps, assumedData$date, sum)
qplot(stepsByDayAssumed)



# 4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates 
# from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

mean(stepsByDayAssumed)
median(stepsByDayAssumed)




# For this part the `weekdays()` function may be of some help here. Use
# the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

# 1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

# ![Sample panel plot](instructions_fig/sample_panelplot.png) 


# **Your plot will look different from the one above** because you will
# be using the activity monitor data. Note that the above plot was made
# using the lattice system but you can make the same version of the plot
# using any plotting system you choose.
