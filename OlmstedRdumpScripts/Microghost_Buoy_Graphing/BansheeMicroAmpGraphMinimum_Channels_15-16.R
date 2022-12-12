#!/usr/bin/env Rscript
#Clean data list slate:
rm(list = ls())
setwd("/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers")
library(lubridate)
library(ggplot2)
library(cowplot)
library(reshape2)
library(dplyr)
library(RColorBrewer)
geist <- read.csv(file="Banshee_untriplicated_YYYYMMDDHHMM3.csv", header=T)
DOdata <- read.csv(file="DO_DATA_TB_YYYYMMDDHHMM2.csv", header=T)
#geist <- read.csv(file="Specter_untriplicated_YYYYMMDDHHMM3.csv", header=T)
HOBO <- read.csv(file="HOBO_DATA_TB_YYYYMMDDHHMM2.csv", header=T)
#Removing random extra columns from hobo
HOBO <- HOBO[,c(1,2,3)]
metadata <- read.csv(file="metadata_2019_Microgeists.csv")

#trying totally manual pallet
#different cathode = greenish , membrane = blueish, deeper anode = darker
cbPalette <- c("chartreuse3","chartreuse4")
colScale2 <- scale_colour_manual(values=cbPalette)

#removing membrane channels
geist <- geist[,c(1,16,17)]
#grabing desired data and making lubridate format
geist$date_time_lubridate <- parse_date_time(geist$date_time, "ymd HM")
filtered_geist_between <- geist %>% filter(date_time_lubridate >= as_datetime("2019-08-04") & date_time_lubridate <= as_datetime("2019-08-06"))
#getting rid of first column
filtered_geist_between_2<-filtered_geist_between[,-1]
melted.filtered_geist_between_2 <- melt(filtered_geist_between_2, id.vars = "date_time_lubridate")
#graphing microamperage
graph2 <- ggplot(melted.filtered_geist_between_2, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Microamperage")+
  labs(col = "Channel")+
  xlab("Date (2019)")
graph2
g3 <- graph2 + colScale2 + geom_point(color="black",size = 0.1) + theme(plot.margin = unit(c(0, 0, 0, 0),"pt"))+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())

#getting a graph of the Light data
HOBO$date_time_lubridate <- parse_date_time(HOBO$date_time, "ymd HM")
filtered_hobo_between <- HOBO %>% filter(date_time_lubridate >= as_datetime("2019-08-04") & 
                                           date_time_lubridate <= as_datetime("2019-08-06"))
#getting only light data and lubridates and then melting
filtered_hobo_between<-filtered_hobo_between[,c(4,3)]
melted.filtered_hobo_between <- melt(filtered_hobo_between, id.vars = "date_time_lubridate")
p3 <- ggplot(melted.filtered_hobo_between, aes(x=date_time_lubridate, y=value, col="Light")) + geom_line(size=2)+
  ylab("lux")+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  theme(plot.margin = unit(c(10, 0, 0, 0),"pt"))+
  theme(legend.title = element_blank()) +scale_color_manual(values = "black")

#getting DO data
DOdata$date_time_lubridate <- parse_date_time(DOdata$date_time, "ymd HM")
filtered_DO_between <- DOdata %>% filter(date_time_lubridate >= as_datetime("2019-08-04") & date_time_lubridate <= as_datetime("2019-08-06"))
filtered_DO_between <-filtered_DO_between[,c(5,3)]
melted.filtered_DO_between <- melt(filtered_DO_between, id.vars = "date_time_lubridate")
#just DO data:
p5 <- ggplot(filtered_DO_between, aes(x=date_time_lubridate, y=DO_mgPerL, col="DO")) + geom_line(size=2) + ylab("mg/L") + xlab("Date (2019)") + theme(plot.margin = unit(c(0, 0, 10, 0),"pt")) +scale_color_manual(values = "red") +theme(legend.title = element_blank()) 

plot_grid(p3, g3, p5 , labels = "AUTO",nrow=3, align = "v", axis="b", rel_heights = c(1,3, 1))
ggsave(filename = "Microgeist_Graphs/Ban15-16_2019-08-04_to_2019-08-06", plot = last_plot(), device = "pdf", width = 13, height = 7.5, units = "in")
