# Example on how to select date range:

banshee <- read.csv("~/Downloads/Banshee_untriplicated_YYYYMMDDHHMM3.csv")

install.packages("lubridate")
library(lubridate)

str(banshee)
# date_time: Factor w/ 1722 levels

#convert the first column to date
banshee$date_time_lubridate <- parse_date_time(banshee$date_time, "ymd HM")

str(banshee)
# the new format is $ date_time_lubridate: POSIXct (last column)

# Use dplyr package to select a range of dates
install.packages("dplyr")
library(dplyr)

#Select all the dates after 2019-02-21
filtered_banshee <- banshee %>% filter(date_time_lubridate >= "2018-08-04")

# Select between dates:
filtered_banshee_between <- banshee %>% filter(date_time_lubridate >= "2019-08-04" & 
                                                 date_time_lubridate <= "2019-08-07")

# Select between dates and use times:
filtered_banshee_between_times <- banshee %>% filter(date_time_lubridate >= "2019-08-04 3:00" & 
                                                 date_time_lubridate <= "2019-08-04 5:00")

# Now compare the number of rows in each of the datasets:
nrow(banshee)
nrow(filtered_banshee)
nrow(filtered_banshee_between)
nrow(filtered_banshee_between_times)

# Try a plot:
library(ggplot2)
ggplot(filtered_banshee_between, aes(x=date_time_lubridate, y=ch1)) + geom_point()

# Let's try to plot all of them:
filtered_banshee_between_2<-filtered_banshee_between[,-1]
library(reshape2)

melted.filtered_banshee_between_2 <- melt(filtered_banshee_between_2, id.vars = "date_time_lubridate")

ggplot(melted.filtered_banshee_between_2, aes(x=date_time_lubridate, y=value, col=variable)) + geom_point()+
  ylab("Microamperage")+
  xlab("Date (2019)")
