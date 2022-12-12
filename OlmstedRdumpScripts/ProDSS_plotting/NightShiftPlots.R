#Clean data list slate:
rm(list = ls())
library("dplyr") 

#hook up external hard drive "ACHERON , OLMSTED2018-X"
TB_2019_07_30_0 <- read.csv(file="/Volumes/Storage/Bog_Data_Editing/Trout_Bog/2019-07-30to31_Trout_Bog/Lv1/2019-07-30_0_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_30_1 <- read.csv(file="/Volumes/Storage/Bog_Data_Editing/Trout_Bog/2019-07-30to31_Trout_Bog/Lv1/2019-07-30_1_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_30_2 <- read.csv(file="/Volumes/Storage/Bog_Data_Editing/Trout_Bog/2019-07-30to31_Trout_Bog/Lv1/2019-07-30_2_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_31_3 <- read.csv(file="/Volumes/Storage/Bog_Data_Editing/Trout_Bog/2019-07-30to31_Trout_Bog/Lv1/2019-07-31_3_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_31_4 <- read.csv(file="/Volumes/Storage/Bog_Data_Editing/Trout_Bog/2019-07-30to31_Trout_Bog/Lv1/2019-07-31_4_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_31_5 <- read.csv(file="/Volumes/Storage/Bog_Data_Editing/Trout_Bog/2019-07-30to31_Trout_Bog/Lv1/2019-07-31_5_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_31_6 <- read.csv(file="/Volumes/Storage/Bog_Data_Editing/Trout_Bog/2019-07-30to31_Trout_Bog/Lv1/2019-07-31_6_Trout_Bog_Lv1.csv", header = T)


TB_2019_07_30_0$TimeDay <- c("7:35am 7-30")
TB_2019_07_30_1$TimeDay <- c("3:00pm 7-30")
TB_2019_07_30_2$TimeDay <- c("8:40pm 7-30")
TB_2019_07_31_3$TimeDay <- c("12:10am 7-31")
TB_2019_07_31_4$TimeDay <- c("4:20am 7-31")
TB_2019_07_31_5$TimeDay <- c("7:35am 7-31")
TB_2019_07_31_6$TimeDay <- c("7:40am 7-31")


TB_all_2019 <- rbind(TB_2019_07_30_0, TB_2019_07_30_1, TB_2019_07_30_2, TB_2019_07_31_3, TB_2019_07_31_4, TB_2019_07_31_5)
TB_all_2019 <- TB_all_2019[which(TB_all_2019$Turbidity < 3.5), ]

TB_all_2019$TimeDay <- factor(TB_all_2019$TimeDay, levels = c("7:35am 7-30", "3:00pm 7-30", "8:40pm 7-30", "12:10am 7-31", "4:20am 7-31", "7:35am 7-31", "7:40am 7-31"))

#Export as 5 x 7.5

ggplot(data=TB_all_2019, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2019, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2019, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2019, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2019, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_all_2019, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")



