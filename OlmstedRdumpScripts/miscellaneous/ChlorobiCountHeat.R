#Seasonal plotting for Trout Bog Chlorobi 2018

rm(list = ls())
#library(ggplot2)
#library(cowplot)
#library(dplyr)
library(tidyverse)
library(lubridate)
#theme_set(theme_cowplot())


path.to.files <- "/Users/cnolmsted/Documents/R_scripts/ChlorobiCountPloting/Data/"
FilesToLoad <- list.files(path = path.to.files)
TB_all_2018 <- read.csv(file=paste0(path.to.files,FilesToLoad[1]))

TB_all_2018$date_lubridate <- parse_date_time(TB_all_2018$date, "mdy")
TB_All <-TB_all_2018[ c(2:6) ]

ggplot(TB_All, aes(x = date_lubridate, y = depth, colour = ChlorobiCount)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")


# Creating Tibble from data
str(TB_All)

TB_Tbl <- as_tibble(TB_All)
#adding non-chlorobi cell count and percentage
TB_Tbl <- mutate(TB_Tbl, NonChlorobiCount = TotalCellCount - ChlorobiCount)
TB_Tbl <- mutate(TB_Tbl, NonChlorobiPercent = NonChlorobiCount / TotalCellCount)

#############
ChlorobiCount
#############

estimate_ChlorobiCount_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$ChlorobiCount, xout = target_depth)$y
}

estimate_ChlorobiCount_by_date(ymd("2018-06-20"), c(0, 1, 1.5, 2))

#interpolate ChlorobiCount across depth

ChlorobiCount_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ChlorobiCount = estimate_ChlorobiCount_by_date(date[1], depth))

ggplot(ChlorobiCount_interp_depth, aes(x = date, y = depth, colour = ChlorobiCount)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date ChlorobiCount interpolation:


estimate_ChlorobiCount_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ChlorobiCount_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$ChlorobiCount, xout = as.Date(target_date))$y
}


estimate_ChlorobiCount_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2018-06-17"), ymd("2018-07-28"), by = 1))


ChlorobiCount_raster <- crossing(
  # dates can now be any value
  #tibble(date = seq(ymd("2018-06-20"), ymd("2018-10-13"), by = 1)),
  tibble(date = seq(ymd("2018-06-20"), ymd("2018-08-28"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(ChlorobiCount_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(ChlorobiCount = estimate_ChlorobiCount_by_depth(depth[1], date))


ggplot(ChlorobiCount_raster, aes(x=date, y=-depth, fill=ChlorobiCount))+
  geom_tile()+
  theme_bw()+
  ggtitle("ChlorobiCount (cells/mL)")+
  scale_fill_continuous(low="white", high="dark green",labels = scales::scientific)+
  ylab("Depth (m)")+
  xlab("Date")

#############
#ChlorobiPercent
#############

estimate_ChlorobiPercent_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$ChlorobiPercent, xout = target_depth)$y
}

estimate_ChlorobiPercent_by_date(ymd("2018-06-20"), c(0, 1, 1.5, 2))

#interpolate ChlorobiPercent across depth

ChlorobiPercent_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ChlorobiPercent = estimate_ChlorobiPercent_by_date(date[1], depth))

ggplot(ChlorobiPercent_interp_depth, aes(x = date, y = depth, colour = ChlorobiPercent)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date ChlorobiPercent interpolation:


estimate_ChlorobiPercent_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ChlorobiPercent_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$ChlorobiPercent, xout = as.Date(target_date))$y
}


estimate_ChlorobiPercent_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2018-06-17"), ymd("2018-07-28"), by = 1))


ChlorobiPercent_raster <- crossing(
  # dates can now be any value
  #tibble(date = seq(ymd("2018-06-20"), ymd("2018-10-13"), by = 1)),
  tibble(date = seq(ymd("2018-06-20"), ymd("2018-08-28"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(ChlorobiPercent_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(ChlorobiPercent = estimate_ChlorobiPercent_by_depth(depth[1], date))


ggplot(ChlorobiPercent_raster, aes(x=date, y=-depth, fill=ChlorobiPercent))+
  geom_tile()+
  theme_bw()+
  ggtitle("ChlorobiPercent (% of Total Cells)")+
  scale_fill_continuous(low="white", high="dark green")+
  ylab("Depth (m)")+
  xlab("Date")

#############
#TotalCellCount
#############

estimate_TotalCellCount_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$TotalCellCount, xout = target_depth)$y
}

estimate_TotalCellCount_by_date(ymd("2018-06-20"), c(0, 1, 1.5, 2))

#interpolate TotalCellCount across depth

TotalCellCount_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(TotalCellCount = estimate_TotalCellCount_by_date(date[1], depth))

ggplot(TotalCellCount_interp_depth, aes(x = date, y = depth, colour = TotalCellCount)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date TotalCellCount interpolation:


estimate_TotalCellCount_by_depth <- function(target_depth, target_date) {
  data_for_depth <- TotalCellCount_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$TotalCellCount, xout = as.Date(target_date))$y
}


estimate_TotalCellCount_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2018-06-17"), ymd("2018-07-28"), by = 1))


TotalCellCount_raster <- crossing(
  # dates can now be any value
  #tibble(date = seq(ymd("2018-06-20"), ymd("2018-10-13"), by = 1)),
  tibble(date = seq(ymd("2018-06-20"), ymd("2018-08-28"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(TotalCellCount_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(TotalCellCount = estimate_TotalCellCount_by_depth(depth[1], date))


ggplot(TotalCellCount_raster, aes(x=date, y=-depth, fill=TotalCellCount))+
  geom_tile()+
  theme_bw()+
  ggtitle("TotalCellCount (cells/mL)")+
  scale_fill_continuous(low="white", high="black")+
  ylab("Depth (m)")+
  xlab("Date")

#############
#NonChlorobiCount
#############

estimate_NonChlorobiCount_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$NonChlorobiCount, xout = target_depth)$y
}

estimate_NonChlorobiCount_by_date(ymd("2018-06-20"), c(0, 1, 1.5, 2))

#interpolate NonChlorobiCount across depth

NonChlorobiCount_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(NonChlorobiCount = estimate_NonChlorobiCount_by_date(date[1], depth))

ggplot(NonChlorobiCount_interp_depth, aes(x = date, y = depth, colour = NonChlorobiCount)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date NonChlorobiCount interpolation:


estimate_NonChlorobiCount_by_depth <- function(target_depth, target_date) {
  data_for_depth <- NonChlorobiCount_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$NonChlorobiCount, xout = as.Date(target_date))$y
}


estimate_NonChlorobiCount_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2018-06-17"), ymd("2018-07-28"), by = 1))


NonChlorobiCount_raster <- crossing(
  # dates can now be any value
  #tibble(date = seq(ymd("2018-06-20"), ymd("2018-10-13"), by = 1)),
  tibble(date = seq(ymd("2018-06-20"), ymd("2018-08-28"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(NonChlorobiCount_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(NonChlorobiCount = estimate_NonChlorobiCount_by_depth(depth[1], date))


ggplot(NonChlorobiCount_raster, aes(x=date, y=-depth, fill=NonChlorobiCount))+
  geom_tile()+
  theme_bw()+
  ggtitle("NonChlorobiCount (cells/mL)")+
  scale_fill_continuous(low="white", high="brown")+
  ylab("Depth (m)")+
  xlab("Date")

#############
#NonChlorobiPercent
#############

estimate_NonChlorobiPercent_by_date <- function(target_date, target_depth) {
  data_for_date <- TB_Tbl %>% 
    filter(date_lubridate == target_date) %>%
    arrange(depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$NonChlorobiPercent, xout = target_depth)$y
}

estimate_NonChlorobiPercent_by_date(ymd("2018-06-20"), c(0, 1, 1.5, 2))

#interpolate NonChlorobiPercent across depth

NonChlorobiPercent_interp_depth <- crossing(
  # the same dates as TB_Tbl
  tibble(date = unique(TB_Tbl$date_lubridate)),
  # depths can now be any value
  tibble(depth = seq(0, 6, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(NonChlorobiPercent = estimate_NonChlorobiPercent_by_date(date[1], depth))

ggplot(NonChlorobiPercent_interp_depth, aes(x = date, y = depth, colour = NonChlorobiPercent)) +
  geom_point() +
  scale_y_reverse() +
  scale_colour_gradient(
    high = "green", 
    low = "blue")

#date NonChlorobiPercent interpolation:


estimate_NonChlorobiPercent_by_depth <- function(target_depth, target_date) {
  data_for_depth <- NonChlorobiPercent_interp_depth %>% 
    filter(depth == target_depth) %>%
    arrange(date)
  approx(x=as.Date(data_for_depth$date), y=data_for_depth$NonChlorobiPercent, xout = as.Date(target_date))$y
}


estimate_NonChlorobiPercent_by_depth(
  target_depth = 0, 
  target_date = seq(ymd("2018-06-17"), ymd("2018-07-28"), by = 1))


NonChlorobiPercent_raster <- crossing(
  # dates can now be any value
  #tibble(date = seq(ymd("2018-06-20"), ymd("2018-10-13"), by = 1)),
  tibble(date = seq(ymd("2018-06-20"), ymd("2018-08-28"), by = 1)),
  # depths must be the same as in temp_interp_depth
  tibble(depth = unique(NonChlorobiPercent_interp_depth$depth))
) %>%
  group_by(depth) %>%
  mutate(NonChlorobiPercent = estimate_NonChlorobiPercent_by_depth(depth[1], date))


ggplot(NonChlorobiPercent_raster, aes(x=date, y=-depth, fill=NonChlorobiPercent))+
  geom_tile()+
  theme_bw()+
  ggtitle("NonChlorobiPercent (cells/mL)")+
  scale_fill_continuous(low="white", high="brown")+
  ylab("Depth (m)")+
  xlab("Date")



