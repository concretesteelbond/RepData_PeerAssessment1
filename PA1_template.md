---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

This is the code I used to pull in the data and remove NA values for steps. 


```r
data <- read.csv("./activity/activity.csv")

clean_data <- na.omit(data)
```

## What is mean total number of steps taken per day?

First, I grouped the data by date, then summed up the step count for each date: 


```r
steps_per_day_without_clean_data <- clean_data %>% 
    select(steps, date) %>% 
    group_by(date) %>% 
    summarise(steps = sum(steps))
```

Then I plotted it on a historgram


```r
hist(steps_per_day_without_clean_data$steps, 
     xlab = "Number Of Steps", 
     breaks = 5, 
     main = "Histogram of Daily Step Counts")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The average number of steps per day are: 


```r
average_steps_per_day <- mean(steps_per_day_without_clean_data$steps)

average_steps_per_day
```

[1] 10766.19

The median number of steps per day are: 


```r
new_median_steps_per_day <- median(steps_per_day_without_clean_data$steps)

new_median_steps_per_day
```

[1] 10765

## What is the average daily activity pattern?

The step count was averaged for each interval: 


```r
average_steps_per_interval <- clean_data %>% 
    select(date, steps, interval) %>% 
    group_by(interval) %>% 
    summarise(steps = mean(steps))
```

And here's the timeseries plot of the average daily activity pattern, based on the data above: 


```r
average_daily_activity_pattern_chart <- average_steps_per_interval %>%
    ggplot( aes(x=interval, y=steps)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    xlab("Frequency (5 min)") +
    ylab("Step Count") +
    ggtitle("Average Daily Step Count")

average_daily_activity_pattern_chart
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Interval with highest step count

The averaged interval with the highest step count is: 


```r
highest_step_count_interval <- max(average_steps_per_interval$steps)

highest_step_count_interval
```

[1] 206.1698

## Imputing missing values

The number of missing values are: 


```r
rows_with_missing_values_count <- nrow(data) - nrow(na.omit(data)) 

rows_with_missing_values_count
```

[1] 2304

My strategy to fill in the data from the missing inteval rows with the average steps per interval. 

Then I merged that with clean data to form a new dataset: 


```r
updated_missing_data <- data %>% select(date, steps, interval) %>% 
    filter(is.na(steps)) %>%
    inner_join(y = average_steps_per_interval, by = "interval") %>%
    select(date, steps.y, interval) %>%
    rename(steps = steps.y)

updated_data <- rbind(updated_missing_data, clean_data)
```

Like the first time, I grouped the updated data by date and summed up the step count for each date. 

Then I plotted it on a histogram: 


```r
steps_per_day_with_updated_data <- updated_data %>% 
  select(steps, date) %>% 
  group_by(date) %>% 
  summarise(steps = sum(steps))

hist(steps_per_day_with_updated_data$steps, breaks = 5, main = "Histogram of Daily Step Counts (Updated Data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The new value for the average step count per day is: 


```r
new_average_steps_per_day <- mean(steps_per_day_with_updated_data$steps)

new_average_steps_per_day
```

```
## [1] 10766.19
```

The new value for the median step count per day is: 


```r
new_median_steps_per_day <- median(steps_per_day_with_updated_data$steps)

new_median_steps_per_day
```

```
## [1] 10766.19
```

The values remain relatively unchanged, although the mean is slightly higher. 

It appears adding the average interval to empty values was a good way to ensure the average wasn't skewed significantly. 

## Are there differences in activity patterns between weekdays and weekends?

I added a weekday and weekend column to for each value in the dataset with crafted missing values. 

Then I plotted them on 2 timeseries plots: 


```r
steps_per_day_with_updated_data <- updated_data %>%
    mutate(date = as.Date(date)) %>%
    mutate(day_section = case_when(wday(date) %in% 1:5 ~ "weekday", 
         wday(date) %in% 6:7 ~ "weekend"))

weekday_time_series <- steps_per_day_with_updated_data %>%
    filter(day_section == "weekday") %>%
    group_by(interval) %>%
    summarise(steps = mean(steps))

weekend_time_series <- steps_per_day_with_updated_data %>%
    filter(day_section == "weekend") %>%
    group_by(interval) %>%
    summarise(steps = mean(steps))
```


```r
chart2 <- weekday_time_series %>%
    ggplot( aes(x=interval, y=steps)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    xlab("Frequency (5 min)") +
    ylab("Step Count") +
    ggtitle("Weekday Step Count")

chart3 <- weekend_time_series %>%
    ggplot( aes(x=interval, y=steps)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    xlab("Frequency (5 min)") +
    ylab("Step Count") +
    ggtitle("Weekend Step Count")

grid.arrange(chart2, chart3, nrow = 2, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
