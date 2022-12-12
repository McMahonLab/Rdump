#Manual Frequency decomposition and correlations between Microgeist loggers channels and light/temperature
#Calculate the rolling mean for every point which excludes the diurnal variation
#Subtract rolling mean from the data, measure synchrony between DO or light or other variables
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(stringr)
library(cowplot)
theme_set(theme_cowplot())
rm(list = ls())
#Only like 1 day of current data for banshee --focusing on specter which was deployed first
#Even specter's current data gives out like 10 days before I pulled it out and harvessted the electrodes. 
#dates for which electrical data exists: 
TotInt <- interval(ymd_hms("2021-07-15 14:29:00"), ymd_hms("2021-08-02 09:27:00"))
ProbInt1 <- interval(ymd_hms("2021-07-15 17:33:00"), ymd_hms("2021-07-15 18:14:00"))
ProbInt2 <- interval(ymd_hms("2021-07-16 10:52:00"), ymd_hms("2021-07-16 11:50:00"))
ProbInt3 <- interval(ymd_hms("2021-08-01 14:17:00"), ymd_hms("2021-08-01 14:56:00"))


# Now trying on my own data:
#focusing first on 2020 dataset Ch16

setwd("/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers")
geist <- read.csv(file="2021SpecterAmps_YYYYMMDDHHMM.csv", header=T)
#DOdata <- read.csv(file="2020_TB_DO_20cm_EDIT.csv", header=T)
DOdata <- read.csv(file="TB_2021_DOTemp75cmFormatted.csv", header=T)
#DOdata <- read.csv(file="2020_SB_DO_20cm_EDIT.csv", header=T)
#geist <- read.csv(file="2020_Specter_YYYYMMDDHHMM.csv", header=T)
#HOBO <- read.csv(file="2020HOBO_Specter.csv", header=T)
HOBO <- read.csv(file="HOBOformatted2021.csv", header=T)
#Removing random extra columns from hobo
HOBO <- HOBO[,c(1,2,3)]

#function for finding nearest values:
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

#grabing desired data and making lubridate format
geist$date_time_lubridate <- parse_date_time(geist$date_time, "ymd HM")
#removing problem intervals from Geist
geistNoRemoval <- geist
geist <- geist[which(geist$date_time_lubridate %within% TotInt == TRUE), ]
geist <- geist[which(geist$date_time_lubridate %within% ProbInt1 == FALSE), ]
geist <- geist[which(geist$date_time_lubridate %within% ProbInt2 == FALSE), ]
geist <- geist[which(geist$date_time_lubridate %within% ProbInt3 == FALSE), ]

plot(geist$date_time_lubridate,geist$ch16)
#rollmean(geist$ch16,72)
#testing: Rollgeist$Rollch1 <- rollmean(geist$ch1,72,fill=NA)
#testing: Rollgeist$Rollchother <- f1(rollmean(geist$ch1,72,fill=NA)) #works
#Can't do this, but I wish I could:
#Rollgeist <- geist
#for (i in 1:16) {
#  Rollgeist$paste("ch",i,sep="") <- f1(rollmean(geist$paste("ch",i,sep=""),72,fill=NA))
#}
#Want to do this:
#Rollgeist <- geist
#Rollgeist$Rollch1 <- f1(rollmean(geist$ch1,72,fill=NA))
#Rollgeist$Rollch2 <- f1(rollmean(geist$ch2,72,fill=NA))
#Rollgeist$Rollch3 <- f1(rollmean(geist$ch3,72,fill=NA))
#Rollgeist$Rollch4 <- f1(rollmean(geist$ch4,72,fill=NA))
#Rollgeist$Rollch5 <- f1(rollmean(geist$ch5,72,fill=NA))
#Rollgeist$Rollch6 <- f1(rollmean(geist$ch6,72,fill=NA))
#Rollgeist$Rollch7 <- f1(rollmean(geist$ch7,72,fill=NA))
#Rollgeist$Rollch8 <- f1(rollmean(geist$ch8,72,fill=NA))
#Rollgeist$Rollch9 <- f1(rollmean(geist$ch9,72,fill=NA))
#Rollgeist$Rollch10 <- f1(rollmean(geist$ch10,72,fill=NA))
#Rollgeist$Rollch11 <- f1(rollmean(geist$ch11,72,fill=NA))
#Rollgeist$Rollch12 <- f1(rollmean(geist$ch12,72,fill=NA))
#Rollgeist$Rollch13 <- f1(rollmean(geist$ch13,72,fill=NA))
#Rollgeist$Rollch14 <- f1(rollmean(geist$ch14,72,fill=NA))
#Rollgeist$Rollch15 <- f1(rollmean(geist$ch15,72,fill=NA))
#Rollgeist$Rollch16 <- f1(rollmean(geist$ch16,72,fill=NA))

#does nearly the same thing but in more complicated but reproducible steps:
Rollgeist <- geist
Rollgeist <- Rollgeist[2:18]
for(i in 1:16) {       # for-loop over columns
  Rollgeist[ , i] <- f1(rollmean(Rollgeist[ , i],72,fill=NA))
}
Rollgeist <- merge(geist, Rollgeist, by = "date_time_lubridate")
#renaming columns
colnames(Rollgeist) <- gsub('.x','',names(Rollgeist))
colnames(Rollgeist) <- gsub('.y','Roll',names(Rollgeist))
plot(Rollgeist$date_time_lubridate,Rollgeist$ch16)
plot(Rollgeist$date_time_lubridate,Rollgeist$ch16Roll)

#458 current points
#looking at subsets:
int <- interval(ymd("2021-07-21"), ymd("2021-07-23"))

RollgeistInt <- Rollgeist[which(Rollgeist$date_time_lubridate %within% int == TRUE), ]

g <- ggplot(RollgeistInt, aes(date_time_lubridate))
g <- g + geom_line(aes(y=ch16), colour="red")
g <- g + geom_line(aes(y=ch16Roll), colour="blue")
g

NoTrend <- Rollgeist
colnames(NoTrend)
for(i in 3:18) {       # for-loop over columns
  NoTrend[ , i] <- NoTrend[ ,i] - NoTrend[ ,i+16]
}
NoTrend <- NoTrend[1:18]

NoTrendInt <- NoTrend[which(NoTrend$date_time_lubridate %within% int == TRUE), ]
ggplot(RollgeistInt, aes(date_time_lubridate)) + geom_line(aes(y=ch16), colour="red") + geom_line(aes(y=ch16Roll), colour="blue")

#melt NoTrendInt
NoTrendInt2 <- NoTrendInt[,c(1,3:18)]
melted.NoTrendInt <- melt(NoTrendInt2, id.vars = "date_time_lubridate")


#copied from BanAll_2020-08-16_09-03
#trying totally manual pallet
#different cathode = greenish , membrane = blueish, deeper anode = darker
cbPalette <- c("lightblue2","gold1","cadetblue4","gold4","deepskyblue1","red","deepskyblue3","mediumvioletred","dodgerblue4","magenta4","chartreuse1","sienna3","mediumblue","orangered4","chartreuse3","chartreuse4")
colScale2 <- scale_colour_manual(values=cbPalette)


#grabing desired data and making lubridate format
geist$date_time_lubridate <- parse_date_time(geist$date_time, "ymd HM")
#filtered_geist_between <- geist %>% filter(date_time_lubridate >= as_datetime("2020-08-16") & date_time_lubridate <= as_datetime("2020-09-03"))
filtered_geist_between <- geist %>% filter(date_time_lubridate %within% int == TRUE) #same thing as above line

#getting rid of first column
filtered_geist_between_2<-filtered_geist_between[,-1]
filtered_geist_between_2<-filtered_geist_between_2[,c(6,7,10,17)] #select if you want only particular channels
#filtered_geist_between_2<-filtered_geist_between_2[,c(1,8,9,16,17)]
#filtered_geist_between_2<-filtered_geist_between_2[,c(2,11,14,17)] 
#filtered_geist_between_2<-filtered_geist_between_2[,c(4,12,17)] 
#filtered_geist_between_2<-filtered_geist_between_2[,c(5,13,17)] 
#filtered_geist_between_2<-filtered_geist_between_2[,c(3,15,17)] 
melted.filtered_geist_between_2 <- melt(filtered_geist_between_2, id.vars = "date_time_lubridate")
#graphing microamperage
graph2 <- ggplot(melted.filtered_geist_between_2, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Microamperage")+
  labs(col = "Channel")+
  xlab("Date (2020)")
graph2
g3 <- graph2 + colScale2 + geom_point(color="black",size = 0.1) + theme(plot.margin = unit(c(0, 0, 0, 0),"pt"))+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())
g3
#getting a graph of the Light data
HOBO$date_time_lubridate <- parse_date_time(HOBO$date_time, "ymd HMS")
HOBO <- HOBO %>% filter(date_time_lubridate %within% TotInt == TRUE)
filtered_hobo_between <- HOBO %>% filter(date_time_lubridate %within% int == TRUE)
#getting only light data and lubridates and then melting
filtered_hobo_between<-filtered_hobo_between[,c(4,3)]
melted.filtered_hobo_between <- melt(filtered_hobo_between, id.vars = "date_time_lubridate")
p3 <- ggplot(melted.filtered_hobo_between, aes(x=date_time_lubridate, y=value, col="Light")) + geom_line(size=2)+
  ylab("lux")+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  theme(plot.margin = unit(c(10, 0, 0, 0),"pt"))+
  theme(legend.title = element_blank()) +scale_color_manual(values = "black")

#getting DO data
DOdata$date_time_lubridate <- parse_date_time(DOdata$date_time, "ymd HMS")
DOdata <- DOdata %>% filter(date_time_lubridate %within% TotInt == TRUE)
filtered_DO_between <- DOdata %>% filter(date_time_lubridate %within% int == TRUE)
filtered_DO_between <-filtered_DO_between[,c(5,3)]
melted.filtered_DO_between <- melt(filtered_DO_between, id.vars = "date_time_lubridate")
#just DO data:
p5 <- ggplot(filtered_DO_between, aes(x=date_time_lubridate, y=DO_mgPerL, col="DO")) + geom_line(size=2) + ylab("mg/L") + xlab("Date (2019)") + theme(plot.margin = unit(c(0, 0, 10, 0),"pt")) +scale_color_manual(values = "red") +theme(legend.title = element_blank()) 
p5
plot_grid(p3, g3, p5 , labels = "AUTO",nrow=3, align = "v", axis="b", rel_heights = c(1,3, 1))#trying totally manual pallet
#different cathode = greenish , membrane = blueish, deeper anode = darker
cbPalette <- c("lightblue2","gold1","cadetblue4","gold4","deepskyblue1","red","deepskyblue3","mediumvioletred","dodgerblue4","magenta4","chartreuse1","sienna3","mediumblue","orangered4","chartreuse3","chartreuse4")
colScale2 <- scale_colour_manual(values=cbPalette)


#grabing desired data and making lubridate format
geist$date_time_lubridate <- parse_date_time(geist$date_time, "ymd HM")
filtered_geist_between <- geist %>% filter(date_time_lubridate %within% int == TRUE)
#getting rid of first column
filtered_geist_between_2<-filtered_geist_between[,-1]
melted.filtered_geist_between_2 <- melt(filtered_geist_between_2, id.vars = "date_time_lubridate")
#graphing microamperage
graph2 <- ggplot(melted.filtered_geist_between_2, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Microamperage")+
  labs(col = "Channel")+
  xlab("Date (2020)")
graph2
g3 <- graph2 + colScale2 + geom_point(color="black",size = 0.1) + theme(plot.margin = unit(c(0, 0, 0, 0),"pt"))+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())

#getting a graph of the Light data
HOBO$date_time_lubridate <- parse_date_time(HOBO$date_time, "ymd HMS")
filtered_hobo_between <- HOBO %>% filter(date_time_lubridate %within% int == TRUE)
#getting only light data and lubridates and then melting
filtered_hobo_between<-filtered_hobo_between[,c(4,3)]
melted.filtered_hobo_between <- melt(filtered_hobo_between, id.vars = "date_time_lubridate")
p3 <- ggplot(melted.filtered_hobo_between, aes(x=date_time_lubridate, y=value, col="Light")) + geom_line(size=2)+
  ylab("lux")+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  theme(plot.margin = unit(c(10, 0, 0, 0),"pt"))+
  theme(legend.title = element_blank()) +scale_color_manual(values = "black")

#getting DO data
DOdata$date_time_lubridate <- parse_date_time(DOdata$date_time, "ymd HMS")
filtered_DO_between <- DOdata %>% filter(date_time_lubridate %within% int == TRUE)
filtered_DO_between <-filtered_DO_between[,c(5,3)]
melted.filtered_DO_between <- melt(filtered_DO_between, id.vars = "date_time_lubridate")
#just DO data:
p5 <- ggplot(filtered_DO_between, aes(x=date_time_lubridate, y=DO_mgPerL, col="DO")) + geom_line(size=2) + ylab("mg/L") + xlab("Date (2020)") + theme(plot.margin = unit(c(0, 0, 10, 0),"pt")) +scale_color_manual(values = "red") +theme(legend.title = element_blank()) 

plot_grid(p3, g3, p5 , labels = "AUTO",nrow=3, align = "v", axis="b", rel_heights = c(1,3, 1))


# includes everything but without any trends:
Gch <- ggplot(melted.NoTrendInt, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Microamperage")+
  labs(col = "Channel")+
  xlab("Date (2020)")
Gch
Gch <- Gch + colScale2 + geom_point(color="black",size = 0.1) + theme(plot.margin = unit(c(0, 0, 0, 0),"pt"))+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())
Gch

plot_grid(p3, Gch, p5 , labels = "AUTO",nrow=3, align = "v", axis="b", rel_heights = c(1,3, 1))


#Run next 5 lines only if you want particular channel with no trend:
#NoTrendIntParticular <- NoTrendInt2[,c(1,2,9,10,17)] #select the particular channels, keep only date_time_lubridate and the channels you want
NoTrendIntParticular <- NoTrendInt2[,c(1,7,8,11)] #select the particular channels, keep only date_time_lubridate and the channels you want
#NoTrendIntParticular <- NoTrendInt2[,c(1,3,12,15)] #select the particular channels, keep only date_time_lubridate and the channels you want
#NoTrendIntParticular <- NoTrendInt2[,c(1,5,13)] #select the particular channels, keep only date_time_lubridate and the channels you want
#NoTrendIntParticular <- NoTrendInt2[,c(1,6,14)] #select the particular channels, keep only date_time_lubridate and the channels you want
#NoTrendIntParticular <- NoTrendInt2[,c(1,4,16)] #select the particular channels, keep only date_time_lubridate and the channels you want


melted.NoTrendIntParticular <- melt(NoTrendIntParticular, id.vars = "date_time_lubridate")

GchP <- ggplot(melted.NoTrendIntParticular, aes(x=date_time_lubridate, y=value, col=variable)) + geom_line(size=2)+
  ylab("Microamperage")+
  labs(col = "Channel")+
  xlab("Date (2020)")
GchP
GchP <- GchP + colScale2 + geom_point(color="black",size = 0.1) + theme(plot.margin = unit(c(0, 0, 0, 0),"pt"))+
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank())
GchP

plot_grid(p3, GchP, p5 , labels = "AUTO",nrow=3, align = "v", axis="b", rel_heights = c(1,3, 1))




#calculating synchrony:
#Pearson on No-Trend data to light, first X days, next X days, etc (each day?)
#Pearson on No-Trend data to oxygen 10cm and 2m, next X days, etc (each day?)
#Pearson Light to DO 20cm and 2m, also temp2m and temp20cm

#dataframe with DO75cm, Temp75cm, Tempsurface, light surface, and all channels and lubridates where each is linearly interpolated for each channel measurement

DO75cmTemp75cm <- DOdata[,c(5,3,2)]
colnames(DO75cmTemp75cm)
colnames(DO75cmTemp75cm) <- c("date_time_lubridate","DO_75cm","Temp_75cm")
SurfaceLuxTemp <- HOBO[,c(4,3,2)]
colnames(SurfaceLuxTemp) <- c("date_time_lubridate","Intensity_lux","Temp_Surface")
colnames(NoTrend)
NoTrend2 <- NoTrend[,c(1,3:18)]

#Making a daylight with no shadows simulated data. 
plot(HOBO$date_time_lubridate,HOBO$Intensity_lux)
#HoboCloudless <- HOBO[,c(4,3)]
#write.csv(HoboCloudless,"/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers/FrequencyDecompositionAndSynchrony/Cloudless.csv")
#edited Cloudless.csv
#Sunsim <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers/FrequencyDecompositionAndSynchrony/Cloudless.csv", header=T)
#Sunsim <- Sunsim[,c(2:4)]
#Sunsim$date_time_lubridate <- parse_date_time(Sunsim$date_time_lubridate, "mdy HM")
#Add Shadow
#Shadow = if intensity_lux is less than Sunsim, Shadow = Sunsim - intensity_lux else = 0
#Sunsim$Shadow <- ifelse(Sunsim$Intensity_lux < Sunsim$SunSimulated, Sunsim$SunSimulated - Sunsim$Intensity_lux, 0)
#Sunsim <- Sunsim[,c(1,3,4)]

AmpLux <- merge(NoTrend2,SurfaceLuxTemp, by="date_time_lubridate", all=TRUE)
#AmpLux <- merge(AmpLux,Sunsim, by="date_time_lubridate", all=TRUE)
AmpLuxDOTemp <- merge(AmpLux,DO75cmTemp75cm, by="date_time_lubridate", all=TRUE)

#grabbing nearest values to fill in NAs in AmpLuxDOTemp using the f1 function
for(i in 18:21) {       # for-loop over columns
  AmpLuxDOTemp[ , i] <- f1(AmpLuxDOTemp[ , i])
}
#grabing data for which only electrical data exists (1374 points)
AmpLuxDOTemp <- AmpLuxDOTemp[complete.cases(AmpLuxDOTemp), ]
FirstThird <- AmpLuxDOTemp[c(1:458),]
MiddleThird <- AmpLuxDOTemp[c(459:916),]
LastThird <- AmpLuxDOTemp[c(917:1374),]

#excluding 8-2:
AmpLuxDOTemp <- AmpLuxDOTemp[c(1:1343),]
FirstThird <- AmpLuxDOTemp[c(1:447),]
MiddleThird <- AmpLuxDOTemp[c(448:894),]
LastThird <- AmpLuxDOTemp[c(895:1343),]

Heatmap <- data.frame(This=character(), withThis=character(), Interval=character(),N=numeric(), r=numeric(), p=numeric(), stringsAsFactors=FALSE) # Blank DF

Heatmap[nrow(Heatmap) + 1,] = c("ch1","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch1","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch1, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch1, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch1, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch1","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch1","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch1, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch2","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch2","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch2, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch2, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch2, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch2","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch2","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch2, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch3","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch3","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch3, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch3, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch3, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch3","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch3","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch3, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch4","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch4","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch4, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch4, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch4, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch4","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch4","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch4, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch5","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch5","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch5, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch5, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch5, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch5","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch5","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch5, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch6","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch6","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch6, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch6, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch6, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch6","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch6","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch6, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch7","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch7","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch7, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch7, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch7, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch7","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch7","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch7, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch8","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch8","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch8, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch8, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch8, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch8","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch8","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch8, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch9","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch9","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch9, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch9, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch9, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch9","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch9","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch9, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch10","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch10","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch10, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch10, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch10, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch10","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch10","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch10, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch11","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch11","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch11, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch11, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch11, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch11","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch11","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch11, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch12","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch12","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch12, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch12, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch12, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch12","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch12","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch12, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch13","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch13","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch13, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch13, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch13, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch13","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch13","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch13, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch14","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch14","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch14, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch14, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch14, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch14","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch14","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch14, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch15","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch15","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch15, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch15, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch15, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch15","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch15","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch15, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch16","Intensity_lux","total",sum(complete.cases(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Intensity_lux), na.rm = TRUE), cor(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Intensity_lux, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch16","DO_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch16, AmpLuxDOTemp$DO_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch16, AmpLuxDOTemp$DO_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch16, AmpLuxDOTemp$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch16","Temp_Surface","total",sum(complete.cases(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Temp_Surface), na.rm = TRUE), cor(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Temp_Surface, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("ch16","Temp_75cm","total",sum(complete.cases(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Temp_75cm), na.rm = TRUE), cor(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Temp_75cm, use = "complete.obs"), cor.test(AmpLuxDOTemp$ch16, AmpLuxDOTemp$Temp_75cm, use = "complete.obs")$p.value)

Heatmap[nrow(Heatmap) + 1,] = c("fch1","Intensity_lux","total",sum(complete.cases(FirstThird$ch1, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch1, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch1, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch1","DO_75cm","total",sum(complete.cases(FirstThird$ch1, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch1, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch1, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch1","Temp_Surface","total",sum(complete.cases(FirstThird$ch1, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch1, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch1, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch1","Temp_75cm","total",sum(complete.cases(FirstThird$ch1, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch1, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch1, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch2","Intensity_lux","total",sum(complete.cases(FirstThird$ch2, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch2, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch2, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch2","DO_75cm","total",sum(complete.cases(FirstThird$ch2, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch2, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch2, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch2","Temp_Surface","total",sum(complete.cases(FirstThird$ch2, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch2, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch2, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch2","Temp_75cm","total",sum(complete.cases(FirstThird$ch2, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch2, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch2, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch3","Intensity_lux","total",sum(complete.cases(FirstThird$ch3, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch3, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch3, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch3","DO_75cm","total",sum(complete.cases(FirstThird$ch3, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch3, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch3, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch3","Temp_Surface","total",sum(complete.cases(FirstThird$ch3, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch3, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch3, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch3","Temp_75cm","total",sum(complete.cases(FirstThird$ch3, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch3, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch3, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch4","Intensity_lux","total",sum(complete.cases(FirstThird$ch4, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch4, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch4, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch4","DO_75cm","total",sum(complete.cases(FirstThird$ch4, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch4, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch4, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch4","Temp_Surface","total",sum(complete.cases(FirstThird$ch4, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch4, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch4, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch4","Temp_75cm","total",sum(complete.cases(FirstThird$ch4, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch4, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch4, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch5","Intensity_lux","total",sum(complete.cases(FirstThird$ch5, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch5, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch5, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch5","DO_75cm","total",sum(complete.cases(FirstThird$ch5, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch5, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch5, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch5","Temp_Surface","total",sum(complete.cases(FirstThird$ch5, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch5, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch5, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch5","Temp_75cm","total",sum(complete.cases(FirstThird$ch5, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch5, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch5, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch6","Intensity_lux","total",sum(complete.cases(FirstThird$ch6, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch6, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch6, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch6","DO_75cm","total",sum(complete.cases(FirstThird$ch6, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch6, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch6, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch6","Temp_Surface","total",sum(complete.cases(FirstThird$ch6, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch6, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch6, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch6","Temp_75cm","total",sum(complete.cases(FirstThird$ch6, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch6, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch6, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch7","Intensity_lux","total",sum(complete.cases(FirstThird$ch7, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch7, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch7, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch7","DO_75cm","total",sum(complete.cases(FirstThird$ch7, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch7, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch7, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch7","Temp_Surface","total",sum(complete.cases(FirstThird$ch7, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch7, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch7, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch7","Temp_75cm","total",sum(complete.cases(FirstThird$ch7, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch7, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch7, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch8","Intensity_lux","total",sum(complete.cases(FirstThird$ch8, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch8, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch8, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch8","DO_75cm","total",sum(complete.cases(FirstThird$ch8, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch8, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch8, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch8","Temp_Surface","total",sum(complete.cases(FirstThird$ch8, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch8, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch8, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch8","Temp_75cm","total",sum(complete.cases(FirstThird$ch8, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch8, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch8, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch9","Intensity_lux","total",sum(complete.cases(FirstThird$ch9, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch9, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch9, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch9","DO_75cm","total",sum(complete.cases(FirstThird$ch9, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch9, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch9, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch9","Temp_Surface","total",sum(complete.cases(FirstThird$ch9, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch9, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch9, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch9","Temp_75cm","total",sum(complete.cases(FirstThird$ch9, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch9, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch9, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch10","Intensity_lux","total",sum(complete.cases(FirstThird$ch10, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch10, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch10, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch10","DO_75cm","total",sum(complete.cases(FirstThird$ch10, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch10, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch10, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch10","Temp_Surface","total",sum(complete.cases(FirstThird$ch10, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch10, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch10, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch10","Temp_75cm","total",sum(complete.cases(FirstThird$ch10, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch10, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch10, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch11","Intensity_lux","total",sum(complete.cases(FirstThird$ch11, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch11, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch11, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch11","DO_75cm","total",sum(complete.cases(FirstThird$ch11, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch11, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch11, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch11","Temp_Surface","total",sum(complete.cases(FirstThird$ch11, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch11, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch11, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch11","Temp_75cm","total",sum(complete.cases(FirstThird$ch11, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch11, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch11, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch12","Intensity_lux","total",sum(complete.cases(FirstThird$ch12, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch12, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch12, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch12","DO_75cm","total",sum(complete.cases(FirstThird$ch12, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch12, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch12, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch12","Temp_Surface","total",sum(complete.cases(FirstThird$ch12, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch12, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch12, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch12","Temp_75cm","total",sum(complete.cases(FirstThird$ch12, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch12, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch12, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch13","Intensity_lux","total",sum(complete.cases(FirstThird$ch13, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch13, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch13, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch13","DO_75cm","total",sum(complete.cases(FirstThird$ch13, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch13, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch13, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch13","Temp_Surface","total",sum(complete.cases(FirstThird$ch13, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch13, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch13, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch13","Temp_75cm","total",sum(complete.cases(FirstThird$ch13, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch13, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch13, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch14","Intensity_lux","total",sum(complete.cases(FirstThird$ch14, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch14, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch14, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch14","DO_75cm","total",sum(complete.cases(FirstThird$ch14, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch14, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch14, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch14","Temp_Surface","total",sum(complete.cases(FirstThird$ch14, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch14, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch14, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch14","Temp_75cm","total",sum(complete.cases(FirstThird$ch14, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch14, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch14, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch15","Intensity_lux","total",sum(complete.cases(FirstThird$ch15, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch15, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch15, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch15","DO_75cm","total",sum(complete.cases(FirstThird$ch15, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch15, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch15, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch15","Temp_Surface","total",sum(complete.cases(FirstThird$ch15, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch15, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch15, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch15","Temp_75cm","total",sum(complete.cases(FirstThird$ch15, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch15, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch15, FirstThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch16","Intensity_lux","total",sum(complete.cases(FirstThird$ch16, FirstThird$Intensity_lux), na.rm = TRUE), cor(FirstThird$ch16, FirstThird$Intensity_lux, use = "complete.obs"), cor.test(FirstThird$ch16, FirstThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch16","DO_75cm","total",sum(complete.cases(FirstThird$ch16, FirstThird$DO_75cm), na.rm = TRUE), cor(FirstThird$ch16, FirstThird$DO_75cm, use = "complete.obs"), cor.test(FirstThird$ch16, FirstThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch16","Temp_Surface","total",sum(complete.cases(FirstThird$ch16, FirstThird$Temp_Surface), na.rm = TRUE), cor(FirstThird$ch16, FirstThird$Temp_Surface, use = "complete.obs"), cor.test(FirstThird$ch16, FirstThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("fch16","Temp_75cm","total",sum(complete.cases(FirstThird$ch16, FirstThird$Temp_75cm), na.rm = TRUE), cor(FirstThird$ch16, FirstThird$Temp_75cm, use = "complete.obs"), cor.test(FirstThird$ch16, FirstThird$Temp_75cm, use = "complete.obs")$p.value)

Heatmap[nrow(Heatmap) + 1,] = c("mch1","Intensity_lux","total",sum(complete.cases(MiddleThird$ch1, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch1, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch1, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch1","DO_75cm","total",sum(complete.cases(MiddleThird$ch1, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch1, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch1, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch1","Temp_Surface","total",sum(complete.cases(MiddleThird$ch1, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch1, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch1, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch1","Temp_75cm","total",sum(complete.cases(MiddleThird$ch1, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch1, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch1, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch2","Intensity_lux","total",sum(complete.cases(MiddleThird$ch2, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch2, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch2, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch2","DO_75cm","total",sum(complete.cases(MiddleThird$ch2, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch2, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch2, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch2","Temp_Surface","total",sum(complete.cases(MiddleThird$ch2, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch2, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch2, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch2","Temp_75cm","total",sum(complete.cases(MiddleThird$ch2, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch2, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch2, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch3","Intensity_lux","total",sum(complete.cases(MiddleThird$ch3, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch3, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch3, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch3","DO_75cm","total",sum(complete.cases(MiddleThird$ch3, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch3, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch3, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch3","Temp_Surface","total",sum(complete.cases(MiddleThird$ch3, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch3, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch3, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch3","Temp_75cm","total",sum(complete.cases(MiddleThird$ch3, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch3, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch3, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch4","Intensity_lux","total",sum(complete.cases(MiddleThird$ch4, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch4, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch4, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch4","DO_75cm","total",sum(complete.cases(MiddleThird$ch4, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch4, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch4, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch4","Temp_Surface","total",sum(complete.cases(MiddleThird$ch4, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch4, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch4, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch4","Temp_75cm","total",sum(complete.cases(MiddleThird$ch4, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch4, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch4, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch5","Intensity_lux","total",sum(complete.cases(MiddleThird$ch5, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch5, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch5, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch5","DO_75cm","total",sum(complete.cases(MiddleThird$ch5, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch5, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch5, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch5","Temp_Surface","total",sum(complete.cases(MiddleThird$ch5, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch5, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch5, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch5","Temp_75cm","total",sum(complete.cases(MiddleThird$ch5, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch5, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch5, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch6","Intensity_lux","total",sum(complete.cases(MiddleThird$ch6, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch6, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch6, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch6","DO_75cm","total",sum(complete.cases(MiddleThird$ch6, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch6, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch6, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch6","Temp_Surface","total",sum(complete.cases(MiddleThird$ch6, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch6, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch6, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch6","Temp_75cm","total",sum(complete.cases(MiddleThird$ch6, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch6, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch6, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch7","Intensity_lux","total",sum(complete.cases(MiddleThird$ch7, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch7, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch7, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch7","DO_75cm","total",sum(complete.cases(MiddleThird$ch7, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch7, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch7, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch7","Temp_Surface","total",sum(complete.cases(MiddleThird$ch7, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch7, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch7, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch7","Temp_75cm","total",sum(complete.cases(MiddleThird$ch7, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch7, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch7, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch8","Intensity_lux","total",sum(complete.cases(MiddleThird$ch8, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch8, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch8, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch8","DO_75cm","total",sum(complete.cases(MiddleThird$ch8, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch8, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch8, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch8","Temp_Surface","total",sum(complete.cases(MiddleThird$ch8, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch8, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch8, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch8","Temp_75cm","total",sum(complete.cases(MiddleThird$ch8, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch8, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch8, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch9","Intensity_lux","total",sum(complete.cases(MiddleThird$ch9, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch9, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch9, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch9","DO_75cm","total",sum(complete.cases(MiddleThird$ch9, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch9, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch9, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch9","Temp_Surface","total",sum(complete.cases(MiddleThird$ch9, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch9, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch9, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch9","Temp_75cm","total",sum(complete.cases(MiddleThird$ch9, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch9, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch9, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch10","Intensity_lux","total",sum(complete.cases(MiddleThird$ch10, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch10, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch10, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch10","DO_75cm","total",sum(complete.cases(MiddleThird$ch10, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch10, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch10, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch10","Temp_Surface","total",sum(complete.cases(MiddleThird$ch10, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch10, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch10, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch10","Temp_75cm","total",sum(complete.cases(MiddleThird$ch10, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch10, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch10, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch11","Intensity_lux","total",sum(complete.cases(MiddleThird$ch11, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch11, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch11, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch11","DO_75cm","total",sum(complete.cases(MiddleThird$ch11, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch11, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch11, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch11","Temp_Surface","total",sum(complete.cases(MiddleThird$ch11, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch11, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch11, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch11","Temp_75cm","total",sum(complete.cases(MiddleThird$ch11, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch11, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch11, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch12","Intensity_lux","total",sum(complete.cases(MiddleThird$ch12, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch12, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch12, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch12","DO_75cm","total",sum(complete.cases(MiddleThird$ch12, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch12, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch12, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch12","Temp_Surface","total",sum(complete.cases(MiddleThird$ch12, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch12, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch12, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch12","Temp_75cm","total",sum(complete.cases(MiddleThird$ch12, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch12, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch12, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch13","Intensity_lux","total",sum(complete.cases(MiddleThird$ch13, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch13, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch13, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch13","DO_75cm","total",sum(complete.cases(MiddleThird$ch13, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch13, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch13, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch13","Temp_Surface","total",sum(complete.cases(MiddleThird$ch13, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch13, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch13, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch13","Temp_75cm","total",sum(complete.cases(MiddleThird$ch13, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch13, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch13, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch14","Intensity_lux","total",sum(complete.cases(MiddleThird$ch14, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch14, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch14, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch14","DO_75cm","total",sum(complete.cases(MiddleThird$ch14, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch14, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch14, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch14","Temp_Surface","total",sum(complete.cases(MiddleThird$ch14, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch14, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch14, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch14","Temp_75cm","total",sum(complete.cases(MiddleThird$ch14, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch14, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch14, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch15","Intensity_lux","total",sum(complete.cases(MiddleThird$ch15, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch15, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch15, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch15","DO_75cm","total",sum(complete.cases(MiddleThird$ch15, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch15, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch15, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch15","Temp_Surface","total",sum(complete.cases(MiddleThird$ch15, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch15, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch15, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch15","Temp_75cm","total",sum(complete.cases(MiddleThird$ch15, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch15, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch15, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch16","Intensity_lux","total",sum(complete.cases(MiddleThird$ch16, MiddleThird$Intensity_lux), na.rm = TRUE), cor(MiddleThird$ch16, MiddleThird$Intensity_lux, use = "complete.obs"), cor.test(MiddleThird$ch16, MiddleThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch16","DO_75cm","total",sum(complete.cases(MiddleThird$ch16, MiddleThird$DO_75cm), na.rm = TRUE), cor(MiddleThird$ch16, MiddleThird$DO_75cm, use = "complete.obs"), cor.test(MiddleThird$ch16, MiddleThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch16","Temp_Surface","total",sum(complete.cases(MiddleThird$ch16, MiddleThird$Temp_Surface), na.rm = TRUE), cor(MiddleThird$ch16, MiddleThird$Temp_Surface, use = "complete.obs"), cor.test(MiddleThird$ch16, MiddleThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("mch16","Temp_75cm","total",sum(complete.cases(MiddleThird$ch16, MiddleThird$Temp_75cm), na.rm = TRUE), cor(MiddleThird$ch16, MiddleThird$Temp_75cm, use = "complete.obs"), cor.test(MiddleThird$ch16, MiddleThird$Temp_75cm, use = "complete.obs")$p.value)

Heatmap[nrow(Heatmap) + 1,] = c("lch1","Intensity_lux","total",sum(complete.cases(LastThird$ch1, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch1, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch1, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch1","DO_75cm","total",sum(complete.cases(LastThird$ch1, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch1, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch1, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch1","Temp_Surface","total",sum(complete.cases(LastThird$ch1, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch1, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch1, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch1","Temp_75cm","total",sum(complete.cases(LastThird$ch1, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch1, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch1, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch2","Intensity_lux","total",sum(complete.cases(LastThird$ch2, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch2, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch2, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch2","DO_75cm","total",sum(complete.cases(LastThird$ch2, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch2, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch2, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch2","Temp_Surface","total",sum(complete.cases(LastThird$ch2, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch2, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch2, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch2","Temp_75cm","total",sum(complete.cases(LastThird$ch2, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch2, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch2, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch3","Intensity_lux","total",sum(complete.cases(LastThird$ch3, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch3, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch3, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch3","DO_75cm","total",sum(complete.cases(LastThird$ch3, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch3, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch3, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch3","Temp_Surface","total",sum(complete.cases(LastThird$ch3, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch3, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch3, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch3","Temp_75cm","total",sum(complete.cases(LastThird$ch3, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch3, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch3, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch4","Intensity_lux","total",sum(complete.cases(LastThird$ch4, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch4, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch4, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch4","DO_75cm","total",sum(complete.cases(LastThird$ch4, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch4, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch4, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch4","Temp_Surface","total",sum(complete.cases(LastThird$ch4, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch4, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch4, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch4","Temp_75cm","total",sum(complete.cases(LastThird$ch4, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch4, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch4, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch5","Intensity_lux","total",sum(complete.cases(LastThird$ch5, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch5, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch5, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch5","DO_75cm","total",sum(complete.cases(LastThird$ch5, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch5, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch5, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch5","Temp_Surface","total",sum(complete.cases(LastThird$ch5, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch5, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch5, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch5","Temp_75cm","total",sum(complete.cases(LastThird$ch5, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch5, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch5, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch6","Intensity_lux","total",sum(complete.cases(LastThird$ch6, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch6, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch6, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch6","DO_75cm","total",sum(complete.cases(LastThird$ch6, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch6, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch6, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch6","Temp_Surface","total",sum(complete.cases(LastThird$ch6, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch6, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch6, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch6","Temp_75cm","total",sum(complete.cases(LastThird$ch6, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch6, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch6, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch7","Intensity_lux","total",sum(complete.cases(LastThird$ch7, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch7, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch7, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch7","DO_75cm","total",sum(complete.cases(LastThird$ch7, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch7, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch7, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch7","Temp_Surface","total",sum(complete.cases(LastThird$ch7, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch7, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch7, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch7","Temp_75cm","total",sum(complete.cases(LastThird$ch7, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch7, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch7, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch8","Intensity_lux","total",sum(complete.cases(LastThird$ch8, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch8, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch8, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch8","DO_75cm","total",sum(complete.cases(LastThird$ch8, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch8, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch8, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch8","Temp_Surface","total",sum(complete.cases(LastThird$ch8, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch8, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch8, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch8","Temp_75cm","total",sum(complete.cases(LastThird$ch8, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch8, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch8, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch9","Intensity_lux","total",sum(complete.cases(LastThird$ch9, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch9, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch9, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch9","DO_75cm","total",sum(complete.cases(LastThird$ch9, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch9, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch9, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch9","Temp_Surface","total",sum(complete.cases(LastThird$ch9, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch9, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch9, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch9","Temp_75cm","total",sum(complete.cases(LastThird$ch9, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch9, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch9, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch10","Intensity_lux","total",sum(complete.cases(LastThird$ch10, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch10, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch10, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch10","DO_75cm","total",sum(complete.cases(LastThird$ch10, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch10, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch10, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch10","Temp_Surface","total",sum(complete.cases(LastThird$ch10, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch10, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch10, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch10","Temp_75cm","total",sum(complete.cases(LastThird$ch10, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch10, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch10, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch11","Intensity_lux","total",sum(complete.cases(LastThird$ch11, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch11, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch11, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch11","DO_75cm","total",sum(complete.cases(LastThird$ch11, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch11, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch11, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch11","Temp_Surface","total",sum(complete.cases(LastThird$ch11, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch11, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch11, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch11","Temp_75cm","total",sum(complete.cases(LastThird$ch11, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch11, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch11, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch12","Intensity_lux","total",sum(complete.cases(LastThird$ch12, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch12, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch12, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch12","DO_75cm","total",sum(complete.cases(LastThird$ch12, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch12, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch12, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch12","Temp_Surface","total",sum(complete.cases(LastThird$ch12, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch12, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch12, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch12","Temp_75cm","total",sum(complete.cases(LastThird$ch12, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch12, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch12, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch13","Intensity_lux","total",sum(complete.cases(LastThird$ch13, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch13, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch13, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch13","DO_75cm","total",sum(complete.cases(LastThird$ch13, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch13, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch13, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch13","Temp_Surface","total",sum(complete.cases(LastThird$ch13, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch13, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch13, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch13","Temp_75cm","total",sum(complete.cases(LastThird$ch13, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch13, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch13, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch14","Intensity_lux","total",sum(complete.cases(LastThird$ch14, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch14, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch14, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch14","DO_75cm","total",sum(complete.cases(LastThird$ch14, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch14, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch14, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch14","Temp_Surface","total",sum(complete.cases(LastThird$ch14, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch14, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch14, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch14","Temp_75cm","total",sum(complete.cases(LastThird$ch14, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch14, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch14, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch15","Intensity_lux","total",sum(complete.cases(LastThird$ch15, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch15, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch15, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch15","DO_75cm","total",sum(complete.cases(LastThird$ch15, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch15, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch15, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch15","Temp_Surface","total",sum(complete.cases(LastThird$ch15, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch15, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch15, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch15","Temp_75cm","total",sum(complete.cases(LastThird$ch15, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch15, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch15, LastThird$Temp_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch16","Intensity_lux","total",sum(complete.cases(LastThird$ch16, LastThird$Intensity_lux), na.rm = TRUE), cor(LastThird$ch16, LastThird$Intensity_lux, use = "complete.obs"), cor.test(LastThird$ch16, LastThird$Intensity_lux, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch16","DO_75cm","total",sum(complete.cases(LastThird$ch16, LastThird$DO_75cm), na.rm = TRUE), cor(LastThird$ch16, LastThird$DO_75cm, use = "complete.obs"), cor.test(LastThird$ch16, LastThird$DO_75cm, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch16","Temp_Surface","total",sum(complete.cases(LastThird$ch16, LastThird$Temp_Surface), na.rm = TRUE), cor(LastThird$ch16, LastThird$Temp_Surface, use = "complete.obs"), cor.test(LastThird$ch16, LastThird$Temp_Surface, use = "complete.obs")$p.value)
Heatmap[nrow(Heatmap) + 1,] = c("lch16","Temp_75cm","total",sum(complete.cases(LastThird$ch16, LastThird$Temp_75cm), na.rm = TRUE), cor(LastThird$ch16, LastThird$Temp_75cm, use = "complete.obs"), cor.test(LastThird$ch16, LastThird$Temp_75cm, use = "complete.obs")$p.value)


Heatmap$This <- factor(Heatmap$This, levels = c("ch1","fch1","mch1","lch1","ch2","fch2","mch2","lch2","ch3","fch3","mch3","lch3","ch4","fch4","mch4","lch4","ch5","fch5","mch5","lch5","ch6","fch6","mch6","lch6","ch7","fch7","mch7","lch7","ch8","fch8","mch8","lch8","ch9","fch9","mch9","lch9","ch10","fch10","mch10","lch10","ch11","fch11","mch11","lch11","ch12","fch12","mch12","lch12","ch13","fch13","mch13","lch13","ch14","fch14","mch14","lch14","ch15","fch15","mch15","lch15","ch16","fch16","mch16","lch16"))

b <- c(-1, 0, 1)
x <- Heatmap$This
y <- Heatmap$withThis
r <- as.numeric(Heatmap$r)
p <- as.numeric(Heatmap$p) # p <- METHeatmap$p %>% p.adjust(method = "BH")
BH <- Heatmap$p %>% p.adjust(method = "BH")
N <- as.numeric(Heatmap$N)
data <- data.frame(x,y,r,p,BH) 

ggplot(data, aes(x, y)) + 
  geom_tile(aes(fill = r), color = "white",lwd=.5) + 
  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"),"(",N,")")), fontface = "bold", color = ifelse(BH <= 0.05 & N >= 5,'yellow','white'),size = 3.5) +
  scale_fill_gradient2(limits = c(-1,1),low="red", high="dodgerblue", mid="black", breaks=b, labels=format(b)) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(1.08, 0.13),
        legend.direction = "vertical") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  theme(plot.margin = unit(c(1, 5, .5, 1), "cm")) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 15, title.position = "top", title.hjust = 0.22))








