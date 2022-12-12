#Clean data list slate:
rm(list = ls())
library(ggplot2)
library(cowplot)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(dplyr)
#adding in the rest of TB dates to make master plots of stuff, i.e. dates before 07_12 and after 07_30. Go back up and reread in the TB individual csv's but not the TB_all.
TB_2018_06_21 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-06-21_Trout_Bog_Edit_1.csv", header=T)
TB_2018_06_25 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-06-25_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_02 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-07-02_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_09 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-07-09_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_16 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-07-16_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_23 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-07-23_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_30 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-07-30_Trout_Bog_Edit_1.csv", header=T)
TB_2018_08_07 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-08-07_Trout_Bog_Edit_1.csv", header=T)
TB_2018_08_13 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data_Editing/Trout_Bog/2018-08-13_Trout_Bog_Edit_1.csv", header=T)

#trimming off the data above where I think the depth is accurate.
TB_2018_06_21 <- TB_2018_06_21[which(TB_2018_06_21$Depth > .22), ]
ggplot(data=TB_2018_06_21, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-06-21 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
#TB_2018_06_25 <- TB_2018_06_25[which(TB_2018_06_25$Depth > .22), ] Didn't need to do this
ggplot(data=TB_2018_06_25, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-06-25 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_02 <- TB_2018_07_02[which(TB_2018_07_02$Depth > .29), ]
ggplot(data=TB_2018_07_02, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-02 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_09 <- TB_2018_07_09[which(TB_2018_07_09$Depth > .3), ]
TB_2018_07_16 <- TB_2018_07_16[which(TB_2018_07_16$Depth > .3), ]
ggplot(data=TB_2018_07_16, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-16 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_23 <- TB_2018_07_23[which(TB_2018_07_23$Depth > .3), ]
ggplot(data=TB_2018_07_23, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-23 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_30 <- TB_2018_07_30[which(TB_2018_07_30$Depth > .31), ]
ggplot(data=TB_2018_07_30, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-30 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_08_07 <- TB_2018_08_07[which(TB_2018_08_07$Depth > .29), ]
ggplot(data=TB_2018_08_07, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-08-07 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_08_13 <- TB_2018_08_13[which(TB_2018_08_13$Depth > .28), ]
ggplot(data=TB_2018_08_13, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-08-13 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')

TB_all <- rbind(TB_2018_06_21,TB_2018_06_25,TB_2018_07_02,TB_2018_07_09,TB_2018_07_16,TB_2018_07_23,TB_2018_07_30,TB_2018_08_07,TB_2018_08_13)
TB_all <- TB_all[which(TB_all$Depth < 6), ]
t <- ggplot(data=TB_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35))
ggplot(data=TB_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(x = 0, y = 0)
o <- ggplot(data=TB_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="ODO(mg/ml)",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35))
ggplot(data=TB_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="ODO(mg/ml)",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(x = 0, y = 0)
p <- ggplot(data=TB_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35))
ggplot(data=TB_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(y = 0)
orp <- ggplot(data=TB_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="ORP",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35))
ggplot(data=TB_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="ORP",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(y = 0)
ggplot(data=TB_all, aes(x=Cond,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Conductivity",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(y = 0)
ggplot(data=TB_all, aes(x=Sal,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Salinity",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(y = 0)
ggplot(data=TB_all, aes(x=TDS,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="TDS",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(y = 0)
#Adding Temp_C column rather than F.
TB_all <- mutate(TB_all, TempC = (Temp-32)*5/9)
ggplot(data=TB_all, aes(x=TempC,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Temperature(˚C)",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(y = 0)

#trying somthing new: (NOPE)
#TB_all$DATE = as.Date(TB_all$DATE, format = "%m/%d/%y")
#ggplot(data=TB_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Turbidity(FNU)",y='Depth(m)')+ theme(legend.position=c(.7,.35)) 

#grabbing color oxygen profiles:
ggplot(data=TB_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(x = 0, y = 0)
#making them color mapped:
TB_all$DATE = as.Date(TB_all$DATE, format = "%m/%d/%y")
ggplot(data=TB_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.1,.35)) + expand_limits(x = 0, y = 0)



#getting counts of chlorobi:
chlorbs <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/ChlorobiCounts.csv", header=T)
# if I want to make dates into a color map, I run this: chlorbs$DATE = as.Date(chlorbs$DATE, format = "%m/%d/%y")

ggplot(data=chlorbs, aes(x=Percent,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Percent Chlorobi",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(x = 0, y = 0)
#why the hell are dates out of order?

c1 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c1.csv", header=T)
c2 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c2.csv", header=T)
c3 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c3.csv", header=T)
c4 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c4.csv", header=T)
c5 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c5.csv", header=T)
c6 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c6.csv", header=T)
c7 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c7.csv", header=T)
c8 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c8.csv", header=T)
c9 <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Dataframes/c9.csv", header=T)
c_all <- rbind(c1,c2,c3,c4,c5,c6,c7,c8,c9)
c <- ggplot(data=c_all, aes(x=Percent,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Percent Chlorobi",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35))
ggplot(data=c_all, aes(x=Percent,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + labs(x="Percent Chlorobi",y='Depth(m)') + scale_color_grey(start=.95,end=.1) + theme(legend.position=c(.7,.35)) + expand_limits(x = 0, y = 7)

#okay ... why DID THAT WORK???

plot_grid(t, c, labels = "AUTO",ncol=2, align = "h")



#whyyyyy