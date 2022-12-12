# graphing seasonal plots.

#starting with 2021 and working backwards

rm(list = ls())
#library(ggplot2)
#library(cowplot)
#library(dplyr)
library(tidyverse)
library(lubridate)
#theme_set(theme_cowplot())

TB_2021_05_26 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-05-26_Trout_Bog_Lv1.csv", header = T)
TB_2021_06_01 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-06-01_Trout_Bog_Lv1.csv", header = T)
TB_2021_06_08 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-06-08_Trout_Bog_Lv1.csv", header = T)
TB_2021_06_15 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-06-15_Trout_Bog_Lv1.csv", header = T)
TB_2021_06_22 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-06-22_Trout_Bog_Lv1.csv", header = T)
TB_2021_06_28 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-06-28_Trout_Bog_Lv1.csv", header = T)
TB_2021_07_06 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-07-06_Trout_Bog_Lv1.csv", header = T)
TB_2021_07_14 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-07-14_Trout_Bog_Lv1.csv", header = T)
TB_2021_07_20 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-07-20_Trout_Bog_Lv1.csv", header = T)
TB_2021_07_26 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-07-26_Trout_Bog_Lv1.csv", header = T)
TB_2021_08_03 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-08-03_Trout_Bog_Lv1.csv", header = T)
TB_2021_08_09 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-08-09_Trout_Bog_Lv1.csv", header = T)
TB_2021_08_18 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_1-1_Lv1.csv", header = T)
TB_2021_08_27 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-08-27_Trout_Bog_Lv1.csv", header = T)
TB_2021_09_30 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog/Lv1/2021-09-30_Trout_Bog_Lv1.csv", header = T)

TB_All <- rbind(TB_2021_05_26,TB_2021_06_01,TB_2021_06_08,TB_2021_06_15,TB_2021_06_22,TB_2021_06_28,TB_2021_07_06,TB_2021_07_14,TB_2021_07_26,TB_2021_08_03,TB_2021_08_09,TB_2021_08_18,TB_2021_08_27)

TB_All$date_lubridate <- parse_date_time(TB_All$DATE, "mdy")

TB_All <- TB_All[which(TB_All$Depth < 6.74), ]

colnames(TB_All)
TB_All <-TB_All[ c(5:25) ]



ggplot(TB_All, aes(x = date_lubridate, y = Depth, colour = Turbidity)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

# Creating Tibble from data
str(TB_All)

TB_Tbl <- as_tibble(TB_All)


estimate_Turbidity_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$Turbidity, xout = target_depth)$y
}

estimate_Turbidity_by_date(ymd("2021-05-26"), c(0, 1, 1.5, 2))

#interpolate turbidity across depth

Turbidity_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6.72, length.out = 250))
) %>%
  group_by(date) %>%
  mutate(Turbidity = estimate_Turbidity_by_date(date[1], depth))

ggplot(Turbidity_interp_depth, aes(x = date, y = depth, colour = Turbidity)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date turbidity interpolation:


estimate_Turbidity_by_depth <- function(target_depth, target_date) {
  data_for_depth <- Turbidity_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$Turbidity, xout = as.Date(target_date))$y
}


estimate_Turbidity_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2021-05-25"), ymd("2021-05-28"), by = 1))


Turbidity_raster <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2021-05-26"), ymd("2021-08-27"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(Turbidity_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(Turbidity = estimate_Turbidity_by_depth(depth[1], date))


ggplot(Turbidity_raster, aes(x=date, y=-depth, fill=Turbidity))+
  geom_tile()+
  theme_bw()+
  ggtitle("Turbidity (FNU)")+
  scale_fill_continuous(low="white", high="dark green")+
  ylab("Depth (m)")+
  xlab("Date")

#exported as 6x6in pdf

###############
#Time for ORP (search-replace Turbidity with ORP)
###############
estimate_ORP_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$ORP, xout = target_depth)$y
}

estimate_ORP_by_date(ymd("2021-05-26"), c(0, 1, 1.5, 2))

#interpolate ORP across depth

ORP_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6.72, length.out = 250))
) %>%
  group_by(date) %>%
  mutate(ORP = estimate_ORP_by_date(date[1], depth))

ggplot(ORP_interp_depth, aes(x = date, y = depth, colour = ORP)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date ORP interpolation:


estimate_ORP_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ORP_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$ORP, xout = as.Date(target_date))$y
}


estimate_ORP_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2021-05-25"), ymd("2021-05-28"), by = 1))


ORP_raster <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2021-05-26"), ymd("2021-08-27"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(ORP_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(ORP = estimate_ORP_by_depth(depth[1], date))


ggplot(ORP_raster, aes(x=date, y=-depth, fill=ORP))+
  geom_tile()+
  theme_bw()+
  ggtitle("ORP (mV)")+
  scale_fill_continuous(low="black", high="red")+
  ylab("Depth (m)")+
  xlab("Date")

###############
#Time for ODO.1 
###############
estimate_ODO.1_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$ODO.1, xout = target_depth)$y
}

estimate_ODO.1_by_date(ymd("2021-05-26"), c(0, 1, 1.5, 2))

#interpolate ODO.1 across depth

ODO.1_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6.72, length.out = 250))
) %>%
  group_by(date) %>%
  mutate(ODO.1 = estimate_ODO.1_by_date(date[1], depth))

ggplot(ODO.1_interp_depth, aes(x = date, y = depth, colour = ODO.1)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date ODO.1 interpolation:


estimate_ODO.1_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ODO.1_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$ODO.1, xout = as.Date(target_date))$y
}


estimate_ODO.1_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2021-05-25"), ymd("2021-05-28"), by = 1))


ODO.1_raster <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2021-05-26"), ymd("2021-08-27"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(ODO.1_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(ODO.1 = estimate_ODO.1_by_depth(depth[1], date))


ggplot(ODO.1_raster, aes(x=date, y=-depth, fill=ODO.1))+
  geom_tile()+
  theme_bw()+
  ggtitle("Dissolved Oxygen (mg/L)")+
  scale_fill_continuous(low="grey", high="red")+
  ylab("Depth (m)")+
  xlab("Date")

###############
#Time for Temp 
###############
estimate_Temp_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$Temp, xout = target_depth)$y
}

estimate_Temp_by_date(ymd("2021-05-26"), c(0, 1, 1.5, 2))

#interpolate Temp across depth

Temp_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6.72, length.out = 250))
) %>%
  group_by(date) %>%
  mutate(Temp = estimate_Temp_by_date(date[1], depth))

ggplot(Temp_interp_depth, aes(x = date, y = depth, colour = Temp)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date Temp interpolation:


estimate_Temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- Temp_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$Temp, xout = as.Date(target_date))$y
}


estimate_Temp_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2021-05-25"), ymd("2021-05-28"), by = 1))


Temp_raster <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2021-05-26"), ymd("2021-08-27"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(Temp_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(Temp = estimate_Temp_by_depth(depth[1], date))


ggplot(Temp_raster, aes(x=date, y=-depth, fill=Temp))+
  geom_tile()+
  theme_bw()+
  ggtitle("Temperature (Celsius)")+
  scale_fill_continuous(low="grey", high="orange")+
  ylab("Depth (m)")+
  xlab("Date")

###############
#Time for pH 
###############
estimate_pH_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$pH, xout = target_depth)$y
}

estimate_pH_by_date(ymd("2021-05-26"), c(0, 1, 1.5, 2))

#interpolate pH across depth

pH_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6.72, length.out = 250))
) %>%
  group_by(date) %>%
  mutate(pH = estimate_pH_by_date(date[1], depth))

ggplot(pH_interp_depth, aes(x = date, y = depth, colour = pH)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date pH interpolation:


estimate_pH_by_depth <- function(target_depth, target_date) {
  data_for_depth <- pH_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$pH, xout = as.Date(target_date))$y
}


estimate_pH_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2021-05-25"), ymd("2021-05-28"), by = 1))


pH_raster <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2021-05-26"), ymd("2021-08-27"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(pH_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(pH = estimate_pH_by_depth(depth[1], date))


ggplot(pH_raster, aes(x=date, y=-depth, fill=pH))+
  geom_tile()+
  theme_bw()+
  ggtitle("pH")+
  scale_fill_continuous(low="black", high="grey")+
  ylab("Depth (m)")+
  xlab("Date")

###############
#Time for Cond 
###############
estimate_Cond_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$Cond, xout = target_depth)$y
}

estimate_Cond_by_date(ymd("2021-05-26"), c(0, 1, 1.5, 2))

#interpolate Cond across depth

Cond_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6.72, length.out = 250))
) %>%
  group_by(date) %>%
  mutate(Cond = estimate_Cond_by_date(date[1], depth))

ggplot(Cond_interp_depth, aes(x = date, y = depth, colour = Cond)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date Cond interpolation:


estimate_Cond_by_depth <- function(target_depth, target_date) {
  data_for_depth <- Cond_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$Cond, xout = as.Date(target_date))$y
}


estimate_Cond_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2021-05-25"), ymd("2021-05-28"), by = 1))


Cond_raster <- crossing(
  # dates can now be any value
  tibble(date = seq(ymd("2021-05-26"), ymd("2021-08-27"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(Cond_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(Cond = estimate_Cond_by_depth(depth[1], date))


ggplot(Cond_raster, aes(x=date, y=-depth, fill=Cond))+
  geom_tile()+
  theme_bw()+
  ggtitle("Conductivity (microsiemens/cm)")+
  scale_fill_continuous(low="snow2", high="black")+
  ylab("Depth (m)")+
  xlab("Date")


