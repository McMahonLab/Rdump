#Frequency decomposition and correlations between Microgeist loggers channels and light/temperature

install.packages("astsa")
install.packages("printr")
install.packages("TTR")
rm(list = ls())

library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)

#trying this: https://rpubs.com/davoodastaraky/TSA1

#doesnt work: kings <- scan('http://robjhyndman.com/tsdldata/misc/kings.dat', skip=3)
kings <- c(60, 43, 67, 50, 56, 42, 50, 65, 68, 43, 65, 34, 47, 34, 49, 41, 13, 35, 53, 56, 16, 43, 69, 59, 48, 59, 86, 55, 68, 51, 33, 49, 67, 77, 81, 67, 71, 81, 68, 70, 77, 56)
head(kings)
#also doesn't work: births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births <- c(26.663, 23.598, 26.931, 24.740, 25.806, 24.364, 24.477, 23.901, 23.175, 23.227, 21.672, 21.870, 21.439, 21.089, 23.709, 21.669, 21.752, 20.761, 23.479, 23.824, 23.105, 23.110, 21.759, 22.073, 21.937, 20.035, 23.590, 21.672, 22.222, 22.123, 23.950, 23.504, 22.238, 23.142, 21.059, 21.573, 21.548, 20.000, 22.424, 20.615, 21.761, 22.874, 24.104, 23.748, 23.262, 22.907, 21.519, 22.025, 22.604, 20.894, 24.677, 23.673, 25.320, 23.583, 24.671, 24.454, 24.122, 24.252, 22.084, 22.991, 23.287, 23.049, 25.076, 24.037, 24.430, 24.667, 26.451, 25.618, 25.014, 25.110, 22.964, 23.981, 23.798, 22.270, 24.775, 22.646, 23.988, 24.737, 26.276, 25.816, 25.210, 25.199, 23.162, 24.707, 24.364, 22.644, 25.565, 24.062, 25.431, 24.635, 27.009, 26.606, 26.268, 26.462, 25.246, 25.180, 24.657, 23.304, 26.982, 26.199, 27.210, 26.122, 26.706, 26.878, 26.152, 26.379, 24.712, 25.688, 24.990, 24.239, 26.721, 23.475, 24.767, 26.219, 28.361, 28.599, 27.914, 27.784, 25.693, 26.881, 26.217, 24.218, 27.914, 26.975, 28.527, 27.139, 28.982, 28.169, 28.056, 29.136, 26.291, 26.987, 26.589, 24.848, 27.543, 26.896, 28.878, 27.390, 28.065, 28.141, 29.048, 28.484, 26.634, 27.735, 27.132, 24.924, 28.963, 26.589, 27.931, 28.009, 29.229, 28.759, 28.405, 27.945, 25.912, 26.619, 26.076, 25.286, 27.660, 25.951, 26.398, 25.565, 28.865, 30.000, 29.261, 29.012, 26.992, 27.897)
births <- ts(births, frequency = 12, start = c(1946, 1))
births
#obviously not going to work: gift <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
gift <- c(1664.81, 2397.53, 2840.71, 3547.29, 3752.96, 3714.74, 4349.61, 3566.34, 5021.82, 6423.48, 7600.60, 19756.21, 2499.81, 5198.24, 7225.14, 4806.03, 5900.88, 4951.34, 6179.12, 4752.15, 5496.43, 5835.10, 12600.08, 28541.72, 4717.02, 5702.63, 9957.58, 5304.78, 6492.43, 6630.80, 7349.62, 8176.62, 8573.17, 9690.50, 15151.84, 34061.01, 5921.10, 5814.58, 12421.25, 6369.77, 7609.12, 7224.75, 8121.22, 7979.25, 8093.06, 8476.70, 17914.66, 30114.41, 4826.64, 6470.23, 9638.77, 8821.17, 8722.37, 10209.48, 11276.55, 12552.22, 11637.39, 13606.89, 21822.11, 45060.69, 7615.03, 9849.69, 14558.40, 11587.33, 9332.56, 13082.09, 16732.78, 19888.61, 23933.38, 25391.35, 36024.80, 80721.71, 10243.24, 11266.88, 21826.84, 17357.33, 15997.79, 18601.53, 26155.15, 28586.52, 30505.41, 30821.33, 46634.38, 104660.67)
gift <- ts(gift, frequency=12, start=c(1987,1))
gift

plot.ts(kings)
plot.ts(births)
plot.ts(gift)

logGift <- log(gift)
plot.ts(logGift)

kingsSMA3 <- SMA(kings, n=3)
plot.ts(kingsSMA3)
kingsSMA3 <- SMA(kings, n=8)
plot.ts(kingsSMA3)

birthsComp <- decompose(births)
birthsComp
plot(birthsComp)

birthsSeasonAdj <- births - birthsComp$seasonal
plot.ts(birthsSeasonAdj)

# Now trying on my own data:
#focusing first on 2020 dataset Ch16

setwd("/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers")
geist <- read.csv(file="2020_Banshee_YYYYMMDDHHMM.csv", header=T)
DOdata <- read.csv(file="2020_TB_DO_20cm_EDIT.csv", header=T)
#DOdata <- read.csv(file="2020_TB_DO_2m_EDIT.csv", header=T)
#DOdata <- read.csv(file="2020_SB_DO_20cm_EDIT.csv", header=T)
#geist <- read.csv(file="2020_Specter_YYYYMMDDHHMM.csv", header=T)
#HOBO <- read.csv(file="2020HOBO_Specter.csv", header=T)
HOBO <- read.csv(file="2020HOBO_Banshee.csv", header=T)
#Removing random extra columns from hobo
HOBO <- HOBO[,c(1,2,3)]

#grabing desired data and making lubridate format
geist$date_time_lubridate <- parse_date_time(geist$date_time, "ymd HM")

Ch16 <- geist[,c(18,17)]
head(Ch16)
plot(Ch16)
Ch16ts <- ts(Ch16, frequency = )
head(Ch16ts)

plot(Ch16ts)

Ch16Comp <- decompose(Ch16ts)


