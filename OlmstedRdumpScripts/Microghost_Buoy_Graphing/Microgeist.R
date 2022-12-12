#Clean data list slate:
rm(list = ls())
setwd("/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers")
Banshee <- read.csv(file="Banshee_untriplicated_YYYYMMDDHHMM3.csv", header=T)
DOdata <- read.csv(file="DO_DATA_TB_YYYYMMDDHHMM2.csv", header=T)
Specter <- read.csv(file="Specter_untriplicated_YYYYMMDDHHMM3.csv", header=T)
HOBO <- read.csv(file="HOBO_DATA_TB_YYYYMMDDHHMM2.csv", header=T)
#Removing random extra columns from hobo
HOBO <- HOBO[,c(1,2,3)]
metadata <- read.csv(file="metadata_2019_Microgeists.csv")

install.packages("lubridate")
library(lubridate)

library(ggplot2)
library(cowplot)
library(reshape2)

str(Banshee)
# date_time: Factor w/ 1722 levels
#convert the first column to date
Banshee$date_time_lubridate <- parse_date_time(Banshee$date_time, "ymd HM")
str(Banshee)
# the new format is $ date_time_lubridate: POSIXct (last column)
# Use dplyr package to select a range of dates
install.packages("dplyr")
library(dplyr)
#Select all the dates after 2019-02-21
filtered_banshee <- Banshee %>% filter(date_time_lubridate >= "2018-08-04")

# Select between dates:
filtered_banshee_between <- Banshee %>% filter(date_time_lubridate >= "2019-08-04" & 
                                                 date_time_lubridate <= "2019-08-06")

# Select between dates and use times:
filtered_banshee_between_times <- Banshee %>% filter(date_time_lubridate >= "2019-08-04 3:00" & 
                                                       date_time_lubridate <= "2019-08-04 5:00")

# Try a plot:
library(ggplot2)
ggplot(filtered_banshee_between, aes(x=date_time_lubridate, y=ch1)) + geom_line()
# Let's try to plot all of them:
filtered_banshee_between_2<-filtered_banshee_between[,-1]
library(reshape2)
melted.filtered_banshee_between_2 <- melt(filtered_banshee_between_2, id.vars = "date_time_lubridate")

graph1 <- ggplot(melted.filtered_banshee_between_2, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Microamperage")+
  xlab("Date (2019)")
#getting colors
library(RColorBrewer)
install.packages("viridis")
library(viridis)
viridis_pal(option = "magma")(16)  
myColors <- viridis_pal(option = "magma")(16)  
names(myColors) <- levels(metadata$channel)
colScale <- scale_colour_manual(name = "channel",values = myColors)
graph1 + colScale1 

#trying totally manual pallet
#different cathode = greenish , membrane = blueish, deeper anode = darker
cbPalette <- c("lightblue2","gold1","cadetblue4","gold4","deepskyblue1","red","deepskyblue3","mediumvioletred","dodgerblue4","magenta4","chartreuse1","tomato4","mediumblue","orangered4","chartreuse3","chartreuse4")
colScale2 <- scale_colour_manual(values=cbPalette)
graph1 + colScale2 + geom_point(color="black",size = 0.1)

# Redoing the above for Specter, same dates
Specter$date_time_lubridate <- parse_date_time(Specter$date_time, "ymd HM")
filtered_specter_between <- Specter %>% filter(date_time_lubridate >= "2019-08-04" & 
                                                 date_time_lubridate <= "2019-08-06")
filtered_specter_between_2<-filtered_specter_between[,-1]
melted.filtered_specter_between_2 <- melt(filtered_specter_between_2, id.vars = "date_time_lubridate")
graph2 <- ggplot(melted.filtered_specter_between_2, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Microamperage")+
  xlab("Date (2019)")
g3<-graph2 + colScale2 + geom_point(color="black",size = 0.1) + theme(legend.position = "none") + theme(plot.margin = unit(c(0, 0, 0, 0)

#getting a graph of the Light data
HOBO$date_time_lubridate <- parse_date_time(HOBO$date_time, "ymd HM")
filtered_hobo_between <- HOBO %>% filter(date_time_lubridate >= "2019-08-04" & 
                                                 date_time_lubridate <= "2019-08-06")

#getting only light data and lubridates and then melting
filtered_hobo_between<-filtered_hobo_between[,c(4,3)]
melted.filtered_hobo_between <- melt(filtered_hobo_between, id.vars = "date_time_lubridate")
p3 <- ggplot(melted.filtered_hobo_between, aes(x=date_time_lubridate, y=value)) + geom_line(size=2)+
  ylab("Intensity(lux)")+
  xlab("Date (2019)") + theme(plot.margin = unit(c(0, 0, 0, 0)
p3
plot_grid(g3, p3, labels = "AUTO",nrow=2, align = "v", axis="b")

#also want DO data
DOdata$date_time_lubridate <- parse_date_time(DOdata$date_time, "ymd HM")
filtered_DO_between <- DOdata %>% filter(date_time_lubridate >= "2019-08-04" & 
                                           date_time_lubridate <= "2019-08-06")
filtered_DO_between<-filtered_DO_between[,c(5,3)]
melted.filtered_DO_between <- melt(filtered_DO_between, id.vars = "date_time_lubridate")
DO_LIGHT <- rbind(melted.filtered_hobo_between,melted.filtered_DO_between)
p4 <- ggplot(DO_LIGHT, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Intensity(lux)")+
  xlab("Date (2019)")
p4
# there is litterally no way to plot 2 y on same x. Those bastards.
#just DO data:
p5 <- ggplot(filtered_DO_between, aes(x=date_time_lubridate, y=DO_mgPerL)) + geom_line(size=2, colour = "red") + ylab("DO(mg/L)") + xlab("Date (2019)") + theme(plot.margin = unit(c(0, 0, 0, 0)
plot_grid(p3, g3, p5, labels = "AUTO",nrow=3, align = "v", axis="b", rel_heights = c(1,3, 1))



#two axes
DOHOBOmergedOuter <- merge(x=filtered_hobo_between,y=filtered_DO_between,by="date_time_lubridate",all=TRUE)
#Nope. cant geom_line with NA values so I must drop them... but I cant

#going to try and merge data but first adding a row to banshee and specter to say buoy name
Banshee$buoy <-rep("banshee",nrow(Banshee))
#put it in the 2nd row
Banshee <- Banshee[,c(1,18,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

Specter$buoy <-rep("specter",nrow(Specter))
#put it in the 2nd row
Specter <- Specter[,c(1,18,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

#Removing random extra columns from hobo
HOBO <- HOBO[,c(1,2,3)]

#Just a ggplot example from another file:
#ggplot(data=SS_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.4))

#merging for excel purposes
Ban_hobo <- merge(Banshee,HOBO,by=c("date_time")) #inner join
Ban_hobo_do <- merge(Ban_hobo,DOdata,by=c("date_time")) #inner join
write.csv(Ban_hobo_do, file = "Banshee_Light_DO.csv")
Spe_hobo <- merge(Specter,HOBO,by=c("date_time")) #inner join
Spe_hobo_do <- merge(Spe_hobo,DOdata,by=c("date_time")) #inner join
write.csv(Spe_hobo_do, file = "Specter_Light_DO.csv")


#melting Banshee and Specter into "longform"
Ban_long <- melt(Banshee)
Spe_long <- melt(Specter)
#renaming columns to match 
names(Ban_long) <- c("date_time","buoy","channel","microamperage")
names(Spe_long) <- c("date_time","buoy","channel","microamperage")
#time to merge everything
Ban_Spe <- rbind(Ban_long,Spe_long)
Ban_Spe_met <- merge(Ban_Spe,metadata,by=c("channel"))
Ban_Spe_met_HOBO <- merge(Ban_Spe_met,HOBO_DO,by=c("date_time")) #inner join

#actually this last two might not be necessary, perhaps I can keep all those datapoints
HOBO_DO <- merge(HOBO,DOdata,by=c("date_time"))

#ggplot(data=Ban_Spe_met, aes(x=date_time,y=microamperage, group = channel, color=channel)) + geom_point() + geom_path() + ggtitle("All data") + labs(x="date_time",y='microamperage')

#plotting all microamperage data
Bplot <- ggplot(data=Ban_long, aes(x=date_time,y=microamperage, group = channel, color=channel))
Bplot <- Bplot + geom_point() + geom_path() + ggtitle("Banshee_all") 
Bplot
#Making limits
min <- as.POSIXct('2019-08-04 00:00')
max <- as.POSIXct('2019-08-07 00:00')
B8_4to8_7lims <- Bplot +scale_x_datetime(limits = as.POSIXct(c('2019-08-04 00:00','2019-08-07 00:00')))
B8_4to8_7lims
B8_4to8_7lims <- Bplot + scale_x_datetime(limits = c(min,max))
#nothing working with changing the scale on the x axis manually.
tmp <- seq(as.POSIXct('2019-08-04 00:00'), as.POSIXct('2019-08-07 00:00'),len=27552)
df <- data.frame(date_time=tmp, x=seq(27552)) 
df4 <- subset(df, date_time >= as.POSIXct('2019-08-04 00:00') & date_time <=
                as.POSIXct('2019-08-07 00:00'))
df4 <- subset(Ban_long, date_time >= as.POSIXct('2019-08-04 00:00') & date_time <=
                as.POSIXct('2019-08-07 00:00'))
