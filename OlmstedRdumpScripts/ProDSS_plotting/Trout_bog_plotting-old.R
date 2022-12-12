#Clean data list slate:
rm(list = ls())

TB_2018_07_09 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-07-09_Trout_Bog_Edit_1.csv", header=T)
library(ggplot2)
ggplot(data=TB_2018_07_09, aes(x=Turbidity,y=Depth)) + geom_point()
library(cowplot)
#to reverse the Y axis: 
ggplot(data=TB_2018_07_09, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() 
#to take a quick look [""] is looking in a dataframe, $ tells the collumn, > 3 
TB_2018_07_09[which(TB_2018_07_09$Turbidity > 3), ]
#To take out the depths at the beginning I don't want
TB_2018_07_09 <- TB_2018_07_09[which(TB_2018_07_09$Depth > .3), ]
#to reorder points based on depth rather than as they are (based on time)
TB_2018_07_09 <- TB_2018_07_09[order(TB_2018_07_09$Depth), ] 
#redo, and add the line
ggplot(data=TB_2018_07_09, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_line()
#oops, orders based on x axis, path connects based on the order in the dataset, so as I ordered them.
ggplot(data=TB_2018_07_09, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path()
#next if I want all the dates on one graph...
#create a new variable that contains all of the other ones using rbind(TB_date1,TBdate2, etc for all dataframes as variables)
# ggplot (data=TB, aes(x=Turbidity,y=Depth, group = DATE)) + same as before
TB_2018_07_16 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-07-16_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_23 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-07-23_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_30 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-07-30_Trout_Bog_Edit_1.csv", header=T)
#also changing the x, y, and title
ggplot(data=TB_2018_07_09, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-09 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_16 <- TB_2018_07_16[which(TB_2018_07_16$Depth > .3), ]
ggplot(data=TB_2018_07_16, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-16 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_23 <- TB_2018_07_23[which(TB_2018_07_23$Depth > .3), ]
ggplot(data=TB_2018_07_23, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-23 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_30 <- TB_2018_07_30[which(TB_2018_07_30$Depth > .31), ]
ggplot(data=TB_2018_07_30, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-30 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')

TB_all <- rbind(TB_2018_07_09,TB_2018_07_16,TB_2018_07_23,TB_2018_07_30)
ggplot(data=TB_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.4))
ggplot(data=TB_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
ggplot(data=TB_all, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
ggplot(data=TB_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
head(TB_2018_07_23)
ggplot(data=TB_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.4))

ML_2018_07_03 <- read.csv(file="/Volumes/Storage\ /Bog_Data/Mary_Lake/2018-07-03_Mary_Lake_Edit_1.csv", header=T)
ML_2018_07_19 <- read.csv(file="/Volumes/Storage\ /Bog_Data/Mary_Lake/2018-07-19_Mary_Lake_Edit_1.csv", header=T)
#also 07-012
ggplot(data=ML_2018_07_19, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path()
ML_2018_07_19 <- ML_2018_07_19[which(ML_2018_07_19$Depth > .28), ]
ML_all <- rbind(ML_2018_07_03,ML_2018_07_12,ML_2018_07_19)
ggplot(data=ML_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=ML_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=ML_all, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=ML_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=ML_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))

CB_2018_07_12 <- read.csv(file="/Volumes/Storage\ /Bog_Data/Crystal_Bog/2018-07-12_Crystal_Bog_Edit_1.csv", header=T)
CB_2018_07_30 <- read.csv(file="/Volumes/Storage\ /Bog_Data/Crystal_Bog/2018-07-30_Crystal_Bog_Edit_1.csv", header=T)
ggplot(data=CB_2018_07_30, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path()
CB_2018_07_30 <- CB_2018_07_30[which(CB_2018_07_30$Depth > .28), ]
CB_all <- rbind(CB_2018_07_12,CB_2018_07_30)
ggplot(data=CB_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=CB_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=CB_all, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=CB_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=CB_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))

NS_2018_07_03 <- read.csv(file="/Volumes/Storage\ /Bog_Data/North_Sparkling_Bog/2018-07-03_North_Sparkling_Bog_Edit_1.csv", header=T)
NS_2018_07_14 <- read.csv(file="/Volumes/Storage\ /Bog_Data/North_Sparkling_Bog/2018-07-14_North_Sparkling_Bog_Edit_1.csv", header=T)
NS_2018_07_18 <- read.csv(file="/Volumes/Storage\ /Bog_Data/North_Sparkling_Bog/2018-07-18_North_Sparkling_Bog_Edit_1.csv", header=T)
ggplot(data=NS_2018_07_18, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path()
NS_2018_07_18 <- NS_2018_07_18[which(NS_2018_07_18$Depth > .28), ]
NS_all <- rbind(NS_2018_07_03,NS_2018_07_14,NS_2018_07_18)
ggplot(data=NS_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=NS_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=NS_all, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=NS_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))
ggplot(data=NS_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.8,.4))

SS_2018_07_03 <- read.csv(file="/Volumes/Storage\ /Bog_Data/South_Sparkling_Bog/2018-07-03_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2018_07_14 <- read.csv(file="/Volumes/Storage\ /Bog_Data/South_Sparkling_Bog/2018-07-14_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2018_07_18 <- read.csv(file="/Volumes/Storage\ /Bog_Data/South_Sparkling_Bog/2018-07-18_South_Sparkling_Bog_Edit_1.csv", header=T)
ggplot(data=SS_2018_07_18, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path()
SS_2018_07_18 <- SS_2018_07_18[which(SS_2018_07_18$Depth > .30), ]
SS_all <- rbind(SS_2018_07_03,SS_2018_07_14,SS_2018_07_18)
ggplot(data=SS_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.4))
ggplot(data=SS_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.25))
ggplot(data=SS_all, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
ggplot(data=SS_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
ggplot(data=SS_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))

#adding in the rest of TB dates to make master plots of stuff, i.e. dates before 07_12 and after 07_30. Go back up and reread in the TB individual csv's but not the TB_all.
TB_2018_06_21 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-06-21_Trout_Bog_Edit_1.csv", header=T)
TB_2018_06_25 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-06-25_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_02 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-07-02_Trout_Bog_Edit_1.csv", header=T)
TB_2018_07_09 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-07-09_Trout_Bog_Edit_1.csv", header=T)
TB_2018_08_07 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-08-07_Trout_Bog_Edit_1.csv", header=T)
TB_2018_08_13 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-08-13_Trout_Bog_Edit_1.csv", header=T)
TB_2018_08_28 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-08-28_Trout_Bog_Edit_1.csv", header=T)

#trimming off the data above where I think the depth is accurate.
ggplot(data=TB_2018_07_09, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-09 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_09 <- TB_2018_07_09[which(TB_2018_07_09$Depth > .3), ]
TB_2018_07_16 <- TB_2018_07_16[which(TB_2018_07_16$Depth > .3), ]
ggplot(data=TB_2018_07_16, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-16 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_23 <- TB_2018_07_23[which(TB_2018_07_23$Depth > .3), ]
ggplot(data=TB_2018_07_23, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-23 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_30 <- TB_2018_07_30[which(TB_2018_07_30$Depth > .31), ]
ggplot(data=TB_2018_07_30, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-30 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_06_21 <- TB_2018_06_21[which(TB_2018_06_21$Depth > .22), ]
ggplot(data=TB_2018_06_21, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-06-21 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
#TB_2018_06_25 <- TB_2018_06_25[which(TB_2018_06_25$Depth > .22), ] Didn't need to do this
ggplot(data=TB_2018_06_25, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-06-25 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_07_02 <- TB_2018_07_02[which(TB_2018_07_02$Depth > .29), ]
ggplot(data=TB_2018_07_02, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-07-02 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_08_07 <- TB_2018_08_07[which(TB_2018_08_07$Depth > .29), ]
ggplot(data=TB_2018_08_07, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-08-07 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
TB_2018_08_13 <- TB_2018_08_13[which(TB_2018_08_13$Depth > .28), ]
ggplot(data=TB_2018_08_13, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-08-13 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
#TB_2018_08_28 <- TB_2018_08_28[which(TB_2018_08_28$Depth > .28), ] didn't need to do this
ggplot(data=TB_2018_08_28, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-08-28 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')


#Same commands as above, but adding in other dates to see what's going on
TB_all <- rbind(TB_2018_06_21,TB_2018_06_25,TB_2018_07_02,TB_2018_07_09,TB_2018_07_16,TB_2018_07_23,TB_2018_07_30,TB_2018_08_07,TB_2018_08_13)
ggplot(data=TB_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.35))
TB_all <- rbind(TB_2018_06_21,TB_2018_06_25,TB_2018_07_02,TB_2018_07_09,TB_2018_07_16)
TB_all <- rbind(TB_2018_07_30,TB_2018_08_07,TB_2018_08_13,TB_2018_08_28)

#Noticed decreasing turbidity 09-27, time to make some plots to see what o2 and such look like
TB_2018_09_11 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-09-11_Trout_Bog_Edit_1.csv", header=T)
TB_2018_09_27 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-09-27_Trout_Bog_Edit_1.csv", header=T)
TB_2018_10_13 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-10-13_Trout_Bog_Edit_1.csv", header=T)
#adding more again, this time where I did 0, =+.025 (or roughly), for everything. 
TB_2018_11_30 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-11-30_Trout_Bog_Edit_1.csv", header=T)
TB_2018_12_17 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2018-12-17_Trout_Bog_Edit_1.csv", header=T)
TB_2019_02_23 <- read.csv(file="/Volumes/Storage/Bog_Data/Trout_Bog/2019-02-23_Trout_Bog_Edit_1.csv", header=T)

TB_all <- rbind(TB_2018_11_30,TB_2018_12_17,TB_2019_02_23)


ggplot(data=TB_2018_09_11, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-09-11 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
ggplot(data=TB_2018_09_27, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-09-27 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')
ggplot(data=TB_2018_10_13, aes(x=Turbidity,y=Depth)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("2018-10-13 Trout Bog Turbidity Profile") + labs(x="Turbidity(FNU)",y='Depth(m)')

TB_all <- rbind(TB_2018_09_27,TB_2018_10_13,TB_2018_11_30,TB_2018_12_17)
#rerun the TB_all turbidiy graphing command above and do the other-variable graphs below
ggplot(data=TB_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))

#Doing South Sparkling profiles, adding in the other ones
SS_2018_06_28 <- read.csv(file="/Volumes/Storage/Bog_Data/South_Sparkling_Bog/2018-06-28_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2018_07_31 <- read.csv(file="/Volumes/Storage/Bog_Data/South_Sparkling_Bog/2018-07-31_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2018_08_08 <- read.csv(file="/Volumes/Storage/Bog_Data/South_Sparkling_Bog/2018-08-08_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2018_10_13 <- read.csv(file="/Volumes/Storage/Bog_Data/South_Sparkling_Bog/2018-10-13_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2018_11_30 <- read.csv(file="/Volumes/Storage/Bog_Data/South_Sparkling_Bog/2018-11-30_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2018_12_17 <- read.csv(file="/Volumes/Storage/Bog_Data/South_Sparkling_Bog/2018-12-17_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2019_01_08 <- read.csv(file="/Volumes/Storage/Bog_Data/South_Sparkling_Bog/2019-01-08_South_Sparkling_Bog_EXO_SD_FLAME_Edit_1.csv", header=T)


#do somthing like this for all of them
SS_2018_07_31 <- SS_2018_07_31[which(SS_2018_07_31$Depth > .28), ]
SS_2018_08_08 <- SS_2018_08_08[which(SS_2018_08_08$Depth < 7.5), ]
SS_all <- rbind(SS_2018_06_28,SS_2018_07_03,SS_2018_07_14,SS_2018_07_18,SS_2018_07_31,SS_2018_08_08,SS_2018_10_13)
SS_all <- rbind(SS_2018_10_13,SS_2018_11_30,SS_2018_12_17)
SS_all <- rbind(SS_2018_10_13,SS_2018_11_30,SS_2018_12_17)

#go back up to do the geom plotting of SS_all

#Trying to graph EXO data:
ggplot(data=SS_2019_01_08, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.4))
ggplot(data=SS_2019_01_08, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling ODO Profile") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))


#looking at 2019 Turbidities which may be off by a bit

TB_2019_06_17 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-06-17_Trout_Bog_Edit_1.csv", header=T)
TB_2019_06_24 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-06-24_Trout_Bog_Edit_1.csv", header=T)
TB_2019_06_30 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-06-30_Trout_Bog_Edit_1.csv", header=T)
TB_2019_07_08 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-07-08_Trout_Bog_Edit_1.csv", header=T)
TB_2019_07_15 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-07-15_Trout_Bog_Edit_1.csv", header=T)
TB_2019_07_17 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-07-17_Trout_Bog_Edit_1.csv", header=T)
TB_2019_07_18 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-07-18_Trout_Bog_Edit_1.csv", header=T)
TB_2019_07_21 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-07-21_Trout_Bog_Edit_1.csv", header=T)
TB_2019_07_22 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-07-22_Trout_Bog_Edit_1.csv", header=T)
TB_2019_07_23 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-07-23_Trout_Bog_Edit_1.csv", header=T)
TB_2019_08_05 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-08-05_Trout_Bog_Edit_1.csv", header=T)
TB_2019_08_13 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-08-13_Trout_Bog_Edit_1.csv", header=T)
TB_2019_08_19 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/Trout_Bog/2019-08-19_Trout_Bog_Edit_1.csv", header=T)


#this time, for each trout bog data set Edit_2, I will try to standardize turbidity assuming constant average turbidity between 5m and 6m and that constant turbidity will be 4, thus i add (4-AVERAGEturb(5m-6m)) to everything
#idea almost good, except better to just standardize based off how much standardization was off from DI water.

TB_all <- rbind(TB_2019_07_21,TB_2019_07_23,TB_2019_08_05,TB_2019_08_13,TB_2019_08_19)

ggplot(data=TB_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.6))
ggplot(data=TB_all, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
ggplot(data=TB_all, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
ggplot(data=TB_all, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.6,.4))
head(TB_2018_07_23)
ggplot(data=TB_all, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.4))

#Trimming TB_all to be only above 6.5
TB_all <- TB_all[which(TB_all$Depth < 7.4), ]
TB_all <- TB_all[which(TB_all$Turbidity < 5), ]
#looking at 2019 south sparkling 
SS_2019_05_30 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/South_Sparkling_Bog/2019-05-30_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2019_06_06 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/South_Sparkling_Bog/2019-06-06_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2019_06_12 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/South_Sparkling_Bog/2019-06-12_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2019_06_20 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/South_Sparkling_Bog/2019-06-20_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2019_06_27 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/South_Sparkling_Bog/2019-06-27_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2019_07_03 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/South_Sparkling_Bog/2019-07-03_South_Sparkling_Bog_Edit_1.csv", header=T)
SS_2019_07_11 <- read.csv(file="/Users/cnolmsted/Desktop/Bog_Data/South_Sparkling_Bog/2019-07-11_South_Sparkling_Bog_Edit_1.csv", header=T)

SS_all <- rbind(SS_2019_05_30,SS_2019_06_06,SS_2019_06_12,SS_2019_06_20,SS_2019_06_27,SS_2019_07_03,SS_2019_07_11)
SS_all <- SS_all[which(SS_all$Turbidity < 20), ]
