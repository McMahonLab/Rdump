#Clean data list slate:
rm(list = ls())
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(lubridate)
theme_set(theme_cowplot())

TB_2021_08_18_1_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_1-1_Lv1.csv", header = T)
TB_2021_08_18_1_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_1-2_Lv1.csv", header = T)
TB_2021_08_18_1_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_1-3_Lv1.csv", header = T)
TB_2021_08_18_2_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_2-1_Lv1.csv", header = T)
TB_2021_08_18_2_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_2-2_Lv1.csv", header = T)
TB_2021_08_18_2_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_2-3_Lv1.csv", header = T)
TB_2021_08_18_3_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_3-1_Lv1.csv", header = T)
TB_2021_08_18_3_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_3-2_Lv1.csv", header = T)
TB_2021_08_18_3_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-18_3-3_Lv1.csv", header = T)
TB_2021_08_19_4_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_1-1_Lv1.csv", header = T)
TB_2021_08_19_4_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_1-2_Lv1.csv", header = T)
TB_2021_08_19_4_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_1-3_Lv1.csv", header = T)
TB_2021_08_19_5_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_2-1_Lv1.csv", header = T)
TB_2021_08_19_5_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_2-2_Lv1.csv", header = T)
TB_2021_08_19_5_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_2-3_Lv1.csv", header = T)
TB_2021_08_19_6_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_3-1_Lv1.csv", header = T)
TB_2021_08_19_6_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_3-2_Lv1.csv", header = T)
TB_2021_08_19_6_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_3-3_Lv1.csv", header = T)
TB_2021_08_19_7_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_4-1_Lv1.csv", header = T)
TB_2021_08_19_7_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_4-2_Lv1.csv", header = T)
TB_2021_08_19_7_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_4-3_Lv1.csv", header = T)
Meta_1_SB_2021_09_05 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-05_SouthSparklingBog_Metatranscriptomics1_Lv1.csv", header = T)
Meta_2_SB_2021_09_05 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-05_SouthSparklingBog_Metatranscriptomics2_Lv1.csv", header = T)
Meta_3_SB_2021_09_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-06_SouthSparklingBog_Metatranscriptomics3_Lv1.csv", header = T)
Meta_4_SB_2021_09_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-06_SouthSparklingBog_Metatranscriptomics4_Lv1.csv", header = T)
Meta_5_SB_2021_09_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-06_SouthSparklingBog_Metatranscriptomics5_Lv1.csv", header = T)
Meta_1_TB_2021_09_05 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-05_TroutBog_Metatranscriptomics1_Lv1.csv", header = T)
Meta_2_TB_2021_09_05 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-05_TroutBog_Metatranscriptomics2_Lv1.csv", header = T)
Meta_3_TB_2021_09_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-06_TroutBog_Metatranscriptomics3_Lv1.csv", header = T)
Meta_4_TB_2021_09_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/Metatranscriptomics/BogProfiles/Lv1/2021-09-06_TroutBog_Metatranscriptomics4_Lv1.csv", header = T)

TB_2021_08_18_1_1$TimeDay <- c("2:33pm 8-18")
TB_2021_08_18_1_2$TimeDay <- c("2:54pm 8-18")
TB_2021_08_18_1_3$TimeDay <- c("3:15pm 8-18")
TB_2021_08_18_2_1$TimeDay <- c("7:29pm 8-18")
TB_2021_08_18_2_2$TimeDay <- c("7:46pm 8-18")
TB_2021_08_18_2_3$TimeDay <- c("8:06pm 8-18")
TB_2021_08_18_3_1$TimeDay <- c("10:39pm 8-18")
TB_2021_08_18_3_2$TimeDay <- c("10:58pm 8-18")
TB_2021_08_18_3_3$TimeDay <- c("11:21pm 8-18")
TB_2021_08_19_4_1$TimeDay <- c("3:20am 8-19")
TB_2021_08_19_4_2$TimeDay <- c("3:42am 8-19")
TB_2021_08_19_4_3$TimeDay <- c("4:02am 8-19")
TB_2021_08_19_5_1$TimeDay <- c("5:25am 8-19")
TB_2021_08_19_5_2$TimeDay <- c("5:45am 8-19")
TB_2021_08_19_5_3$TimeDay <- c("6:02am 8-19")
TB_2021_08_19_6_1$TimeDay <- c("8:54am 8-19")
TB_2021_08_19_6_2$TimeDay <- c("9:15am 8-19")
TB_2021_08_19_6_3$TimeDay <- c("9:40am 8-19")
TB_2021_08_19_7_1$TimeDay <- c("1:50pm 8-19")
TB_2021_08_19_7_2$TimeDay <- c("2:08pm 8-19")
TB_2021_08_19_7_3$TimeDay <- c("2:25am 8-19")
Meta_1_SB_2021_09_05$TimeDay <- c("5:38pm 9-5")
Meta_2_SB_2021_09_05$TimeDay <- c("8:14pm 9-5")
Meta_3_SB_2021_09_06$TimeDay <- c("4:00am 9-6")
Meta_4_SB_2021_09_06$TimeDay <- c("7:54am 9-6")
Meta_5_SB_2021_09_06$TimeDay <- c("10:11am 9-6")
Meta_1_TB_2021_09_05$TimeDay <- c("2:51pm 9-5")
Meta_2_TB_2021_09_05$TimeDay <- c("6:54pm 9-5")
Meta_3_TB_2021_09_06$TimeDay <- c("2:40am 9-6")
Meta_4_TB_2021_09_06$TimeDay <- c("6:14am 9-6")


TB_all_2021 <- rbind(TB_2021_08_18_1_1,TB_2021_08_18_1_2,TB_2021_08_18_1_3,TB_2021_08_18_2_1,TB_2021_08_18_2_2,TB_2021_08_18_2_3,TB_2021_08_18_3_1,TB_2021_08_18_3_2,TB_2021_08_18_3_3,TB_2021_08_19_4_1,TB_2021_08_19_4_2,TB_2021_08_19_4_3,TB_2021_08_19_5_1,TB_2021_08_19_5_2,TB_2021_08_19_5_3,TB_2021_08_19_6_1,TB_2021_08_19_6_2,TB_2021_08_19_6_3,TB_2021_08_19_7_1,TB_2021_08_19_7_2,TB_2021_08_19_7_3)
TB_all_2021 <- TB_all_2021[which(TB_all_2021$Turbidity < 5), ]
TB_all_2021$TimeDay <- factor(TB_all_2021$TimeDay, levels = c("2:33pm 8-18", "2:54pm 8-18", "3:15pm 8-18", "7:29pm 8-18", "7:46pm 8-18", "8:06pm 8-18", "10:39pm 8-18", "10:58pm 8-18", "11:21pm 8-18", "3:20am 8-19", "3:42am 8-19", "4:02am 8-19", "5:25am 8-19", "5:45am 8-19", "6:02am 8-19", "8:54am 8-19", "9:15am 8-19", "9:40am 8-19", "1:50pm 8-19", "2:08pm 8-19", "2:25am 8-19"))

Meta_all_SB <- rbind(Meta_1_SB_2021_09_05,Meta_2_SB_2021_09_05,Meta_3_SB_2021_09_06,Meta_4_SB_2021_09_06,Meta_5_SB_2021_09_06)
Meta_all_TB <- rbind(Meta_1_TB_2021_09_05,Meta_2_TB_2021_09_05,Meta_3_TB_2021_09_06,Meta_4_TB_2021_09_06)
Meta_all_SB <- Meta_all_SB[which(Meta_all_SB$Turbidity < 16), ]
Meta_all_TB <- Meta_all_TB[which(Meta_all_TB$Turbidity < 5), ]
Meta_all_SB$TimeDay <- factor(Meta_all_SB$TimeDay, levels = c("5:38pm 9-5", "8:14pm 9-5", "4:00am 9-6","7:54am 9-6", "10:11am 9-6"))
Meta_all_TB$TimeDay <- factor(Meta_all_TB$TimeDay, levels = c("2:51pm 9-5", "6:54pm 9-5", "2:40am 9-6", "6:14am 9-6"))

ggplot(data=Meta_all_SB, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_SB, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_SB, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_SB, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_SB, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=Meta_all_TB, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_TB, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_TB, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_TB, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=Meta_all_TB, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")


#Export as 5 x 7.5

ggplot(data=TB_all_2021, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_all_2021, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")



TB_3rd_2021 <- rbind(TB_2021_08_18_1_3,TB_2021_08_18_2_3,TB_2021_08_18_3_3,TB_2021_08_19_4_3,TB_2021_08_19_5_3,TB_2021_08_19_6_3,TB_2021_08_19_7_3)
TB_3rd_2021 <- TB_3rd_2021[which(TB_3rd_2021$Turbidity < 5), ]

TB_3rd_2021$TimeDay <- factor(TB_3rd_2021$TimeDay, levels = c("2:33pm 8-18", "2:54pm 8-18", "3:15pm 8-18", "7:29pm 8-18", "7:46pm 8-18", "8:06pm 8-18", "10:39pm 8-18", "10:58pm 8-18", "11:21pm 8-18", "3:20am 8-19", "3:42am 8-19", "4:02am 8-19", "5:25am 8-19", "5:45am 8-19", "6:02am 8-19", "8:54am 8-19", "9:15am 8-19", "9:40am 8-19", "1:50pm 8-19", "2:08pm 8-19", "2:25am 8-19"))

#Export as 5 x 7.5

ggplot(data=TB_3rd_2021, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_3rd_2021, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

TB_2nd_2021 <- rbind(TB_2021_08_18_1_2,TB_2021_08_18_2_2,TB_2021_08_18_3_2,TB_2021_08_19_4_2,TB_2021_08_19_5_2,TB_2021_08_19_6_2,TB_2021_08_19_7_2)
TB_2nd_2021 <- TB_2nd_2021[which(TB_2nd_2021$Turbidity < 5), ]

TB_2nd_2021$TimeDay <- factor(TB_2nd_2021$TimeDay, levels = c("2:33pm 8-18", "2:54pm 8-18", "3:15pm 8-18", "7:29pm 8-18", "7:46pm 8-18", "8:06pm 8-18", "10:39pm 8-18", "10:58pm 8-18", "11:21pm 8-18", "3:20am 8-19", "3:42am 8-19", "4:02am 8-19", "5:25am 8-19", "5:45am 8-19", "6:02am 8-19", "8:54am 8-19", "9:15am 8-19", "9:40am 8-19", "1:50pm 8-19", "2:08pm 8-19", "2:25am 8-19"))

#Export as 5 x 7.5

ggplot(data=TB_2nd_2021, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_2nd_2021, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")



TB_1st_2021 <- rbind(TB_2021_08_18_1_1,TB_2021_08_18_2_1,TB_2021_08_18_3_1,TB_2021_08_19_4_1,TB_2021_08_19_5_1,TB_2021_08_19_6_1,TB_2021_08_19_7_1)
TB_1st_2021 <- TB_1st_2021[which(TB_1st_2021$Turbidity < 5), ]

TB_1st_2021$TimeDay <- factor(TB_1st_2021$TimeDay, levels = c("2:33pm 8-18", "2:54pm 8-18", "3:15pm 8-18", "7:29pm 8-18", "7:46pm 8-18", "8:06pm 8-18", "10:39pm 8-18", "10:58pm 8-18", "11:21pm 8-18", "3:20am 8-19", "3:42am 8-19", "4:02am 8-19", "5:25am 8-19", "5:45am 8-19", "6:02am 8-19", "8:54am 8-19", "9:15am 8-19", "9:40am 8-19", "1:50pm 8-19", "2:08pm 8-19", "2:25am 8-19"))

#Export as 5 x 7.5

ggplot(data=TB_1st_2021, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_1st_2021, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_1st_2021, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_1st_2021, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_1st_2021, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_1st_2021, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

#trying to average/stdev across depth usinig Patricia's suggestion:
#Method: average/stDev all values in discrete depth ranges and graph them.
#mutate(Depth_avg = round(depth) %>% group_by(Depth_avg) %>% mutate(avg = avg(profile1, profile2, profile3)))
#Something like this

TB_2021_08_18_1 <- mutate(Depth_avg = round(Depth) %>% group_by(Depth_avg) %>% mutate(avg = avg(TB_2021_08_18_1_1, TB_2021_08_18_1_2, TB_2021_08_18_1_3)))


MSNfunc <- function(x) c(mean=mean(x), n=length(x), StDev = sd(x))

sapply(Num_TB_2021_08_18_1_1, MSNfunc)
Num_TB_2021_08_18_1_1[which(Num_TB_2021_08_18_1_1$Depth >= 0 & Num_TB_2021_08_18_1_1$Depth <= .1), ]



mutate(avg = mean(TB_2021_08_18_1_1$Depth, TB_2021_08_18_1_2$Depth, TB_2021_08_18_1_3$Depth))

Num_TB_2021_08_18_1_1 <- TB_2021_08_18_1_1[c(5:11,14,15,17:24)]
Num_TB_2021_08_18_1_2 <- TB_2021_08_18_1_2[c(5:11,14,15,17:24)]
Num_TB_2021_08_18_1_3 <- TB_2021_08_18_1_3[c(5:11,14,15,17:24)]


#Patricia: linear interpolation estimates.

#instead just going to read all the files at the same time from one folder.
#reading one file at a time into the same object
rm(list = ls())
path.to.files <- "/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/"
FilesToLoad <- list.files(path = path.to.files)
TB_all_2021 <- read.csv(file=paste0(path.to.files,FilesToLoad[1]))
TB_all_2021$Profile <- str_replace(FilesToLoad[1],"_Lv1.csv","")
TB_all_2021$first_time_point <- format(strptime(TB_all_2021$TIME[1],"%H:%M:%S"),'%H:%M')

for (file in 2:length(FilesToLoad)){
  my_file <- read.csv(file=paste0(path.to.files,FilesToLoad[file]))
  my_file$Profile <- str_replace(FilesToLoad[file],"_Lv1.csv","")
  my_file$first_time_point <- format(strptime(my_file$TIME[1],"%H:%M:%S"),'%H:%M')
  TB_all_2021 <- rbind(TB_all_2021, my_file)
  print(file)
}

unique(TB_all_2021$Profile)

TB_all_2021$TimeHourMinutes <- format(strptime(TB_all_2021$TIME,"%H:%M:%S"),'%H:%M')
TB_all_2021$DATE <- mdy(TB_all_2021$DATE)
TB_all_2021$Date_MonthDay <- format(as.Date(TB_all_2021$DATE), "%m-%d")

TB_all_2021$TimeDay <- paste(TB_all_2021$first_time_point, TB_all_2021$Date_MonthDay)
TB_all_2021 <- TB_all_2021 %>% filter(Turbidity < 5)

ggplot(TB_all_2021, aes(x=ORP, y=-Depth, col=Profile))+
  geom_point()

TB_all_2021 <- TB_all_2021 %>% separate(Profile,sep="_", into=c("Lake","Bog","Date","Profile")) #this step can split up a column default is to remove the original column    
TB_all_2021 <- TB_all_2021 %>% separate(Profile,sep="-", into=c("Profile","Replicate"))

TB_all_2021 <- TB_all_2021 %>%
  mutate(AveTimeDay = case_when(
    Profile == "1" ~ "3pm 8-18",
    Profile == "2" ~ "8pm 8-18",
    Profile == "3" ~ "11pm 8-18",
    Profile == "4" ~ "4am 8-19",
    Profile == "5" ~ "6am 8-19",
    Profile == "6" ~ "9am 8-19",
    Profile == "7" ~ "2pm 8-19"
  ))
#sunset was at 8:02pm in boulder junction on August 18th 2021 and 8:00 on the 19th
#sunrise was at 6:02am and 6:03am 

TB_all_2021$AveTimeDay <- factor(TB_all_2021$AveTimeDay, levels = c("3pm 8-18","8pm 8-18","11pm 8-18","4am 8-19","6am 8-19","9am 8-19","2pm 8-19"))

unique(TB_all_2021$Profile)

ggplot(TB_all_2021,  aes(x=ORP, y=-Depth,col=AveTimeDay, shape=Replicate))+
  geom_point()


TB_all_2021 %>% group_by(AveTimeDay, first_time_point) %>% tally()

TB_2n3_2021 <- TB_all_2021 %>% filter(Replicate != 1)


ORP.plot.with.dots <- ggplot(TB_all_2021, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point() +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.01, orientation =  "y")+
  theme_bw()+
  ggtitle("ORP  with underlying data shown")


ORP.plot <- ggplot(TB_all_2021, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  #geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) + #add/get rid of points if you want
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  ggtitle("ORP")

ORP.plot

#######Timegif


TB1 <- TB_all_2021 %>% filter(AveTimeDay == "3pm 8-18")
TB2 <- TB_all_2021 %>% filter(AveTimeDay == "3pm 8-18" | AveTimeDay == "8pm 8-18")
TB3 <- TB_all_2021 %>% filter(AveTimeDay == "3pm 8-18" | AveTimeDay == "8pm 8-18" | AveTimeDay == "11pm 8-18")
TB4 <- TB_all_2021 %>% filter(AveTimeDay == "3pm 8-18" | AveTimeDay == "8pm 8-18" | AveTimeDay == "11pm 8-18" | AveTimeDay == "4am 8-19")
TB5 <- TB_all_2021 %>% filter(AveTimeDay == "3pm 8-18" | AveTimeDay == "8pm 8-18" | AveTimeDay == "11pm 8-18" | AveTimeDay == "4am 8-19" | AveTimeDay == "6am 8-19")
TB6 <- TB_all_2021 %>% filter(AveTimeDay == "3pm 8-18" | AveTimeDay == "8pm 8-18" | AveTimeDay == "11pm 8-18" | AveTimeDay == "4am 8-19" | AveTimeDay == "6am 8-19" | AveTimeDay == "9am 8-19")
TB7 <- TB_all_2021 %>% filter(AveTimeDay == "3pm 8-18" | AveTimeDay == "8pm 8-18" | AveTimeDay == "11pm 8-18" | AveTimeDay == "4am 8-19" | AveTimeDay == "6am 8-19" | AveTimeDay == "9am 8-19" | AveTimeDay == "2pm 8-19")


ORP.plot <- ggplot(TB1, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") + scale_color_manual(values = c("#E37B72"))+ scale_fill_manual(values = c("#E37B72"))+ theme(legend.position="none") +xlim(50,350) + ylim(-7.3,0)
ORP.plot <- ggplot(TB2, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") + scale_color_manual(values = c("#E37B72","#BD9E44"))+ scale_fill_manual(values = c("#E37B72","#BD9E44"))+ theme(legend.position="none") +xlim(50,350) + ylim(-7.3,0)
ORP.plot <- ggplot(TB3, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") + scale_color_manual(values = c("#E37B72","#BD9E44","#6DB134"))+ scale_fill_manual(values = c("#E37B72","#BD9E44","#6DB134"))+ theme(legend.position="none") +xlim(50,350) + ylim(-7.3,0)
ORP.plot <- ggplot(TB4, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") + scale_color_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97"))+ scale_fill_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97"))+ theme(legend.position="none") +xlim(50,350) + ylim(-7.3,0)
ORP.plot <- ggplot(TB5, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") + scale_color_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97","#51B3E6"))+ scale_fill_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97","#51B3E6"))+ theme(legend.position="none") +xlim(50,350) + ylim(-7.3,0)
ORP.plot <- ggplot(TB6, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") + scale_color_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97","#51B3E6","#A08CF8"))+ scale_fill_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97","#51B3E6","#A08CF8"))+ theme(legend.position="none") +xlim(50,350) + ylim(-7.3,0)
ORP.plot <- ggplot(TB7, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") + scale_color_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97","#51B3E6","#A08CF8","#EA6CD3"))+ scale_fill_manual(values = c("#E37B72","#BD9E44","#6DB134","#57BE97","#51B3E6","#A08CF8","#EA6CD3"))+ theme(legend.position="none") +xlim(50,350) + ylim(-7.3,0)

ORP.plot






ORP.plot.without1 <-ggplot(TB_2n3_2021, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = 1, shape = TB_2n3_2021$Replicate) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y")+
  theme_bw()+
  ggtitle("ORP without Replicate 1 of any profiles")

ORP.plot.without1


#Turbidity
Turb.plot <- ggplot(TB_all_2021, aes(x = Turbidity, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  ggtitle("Turbidity")

Turb.plot

Turb.plot.without1.with.underlying.data.shown <-ggplot(TB_2n3_2021 %>% filter(Replicate != 1), aes(x = Turbidity, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = 1, shape = TB_2n3_2021$Replicate) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y")+
  theme_bw()+
  ggtitle("Turbidity without Replicate 1 of any profiles\nwith underlying data shown")

Turb.plot.without1.with.underlying.data.shown

#pH
pH.plot <- ggplot(TB_all_2021, aes(x = pH, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  ggtitle("pH")

pH.plot

pH.plot.without1.with.underlying.data.shown <-ggplot(TB_2n3_2021 %>% filter(Replicate != 1), aes(x = pH, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = 1, shape = TB_2n3_2021$Replicate) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y")+
  theme_bw()+
  ggtitle("pH without Replicate 1 of any profiles\nwith underlying data shown")

pH.plot.without1.with.underlying.data.shown

#ODO
ODO.plot <- ggplot(TB_all_2021, aes(x = ODO.1, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = .01) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  ggtitle("ODO.1")

ODO.plot

ODO.plot.without1.with.underlying.data.shown <-ggplot(TB_all_2021 %>% filter(Replicate != 1), aes(x = ODO.1, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = .01) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y")+
  theme_bw()+
  ggtitle("pH without Replicate 1 of any profiles\nwith underlying data shown")

ODO.plot.without1.with.underlying.data.shown

#Temp
Temp.plot <- ggplot(TB_all_2021, aes(x = Temp, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = .01) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  ggtitle("Temperature")

Temp.plot

Temp.plot.without1.with.underlying.data.shown <-ggplot(TB_all_2021 %>% filter(Replicate != 1), aes(x = Temp, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = .01) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y")+
  theme_bw()+
  ggtitle("Temperature without Replicate 1 of any profiles\nwith underlying data shown")

Temp.plot.without1.with.underlying.data.shown

#Conductivity
Cond.plot <- ggplot(TB_all_2021, aes(x = Cond, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = .01) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  ggtitle("Conductivity")

Cond.plot

Cond.plot.without1.with.underlying.data.shown <-ggplot(TB_all_2021 %>% filter(Replicate != 1), aes(x = Cond, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  geom_point(alpha = .5, size = .01) +
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay),
              method = "loess",span=0.03, orientation =  "y")+
  theme_bw()+
  ggtitle("Conductivity without Replicate 1 of any profiles\nwith underlying data shown")

Cond.plot.without1.with.underlying.data.shown


#Graphing ORP at just 2m over time:
TB_2m_only <- TB_all_2021 %>% filter(Depth > 1.95 & Depth < 2.05)
TB_2m_only$date_time <- paste(TB_2m_only$DATE, TB_2m_only$TIME, sep = " ")
#also works: TB_2m_only$date_time_lubridate <- parse_date_time(TB_2m_only$date_time, 'ymd %I:%M:%S %p')
TB_2m_only$date_time_lubridate <- ymd_hms(TB_2m_only$date_time)

ORP.plot <- ggplot(TB_2m_only, aes(x = date_time_lubridate, y = ORP)) + 
  scale_x_datetime(date_breaks = "2 hours", date_labels="%m/%d %l %p") + 
  geom_point(alpha = 0, size = 2, shape = TB_2m_only$Replicate) + 
  stat_smooth(method = "loess",orientation =  "x") +
  theme_bw() + 
  xlab("Date Time, Year: 2021")+ 
  ylab("ORP (mV)")+ 
  ggtitle("")


ORP.plot

#Making it averged per timepoint
TB_2m_only <- TB_2m_only %>%
  mutate(AveDateTime = case_when(
    Profile == "1" ~ "2021-8-18 3:00 PM",
    Profile == "2" ~ "2021-8-18 8:00 PM",
    Profile == "3" ~ "2021-8-18 11:00 PM",
    Profile == "4" ~ "2021-8-19 4:00 AM",
    Profile == "5" ~ "2021-8-19 6:00 AM",
    Profile == "6" ~ "2021-8-19 9:00 AM",
    Profile == "7" ~ "2021-8-19 2:00 PM"
  ))

TB_2m_only$ave_date_time_lubridate <- ymd_hm(TB_2m_only$AveDateTime)
colnames(TB_2m_only)
str(TB_2m_only_summary)
TB_2m_only_summary <- TB_2m_only[c(38,5:24)]
TB_2m_only_summary <- TB_2m_only_summary %>% group_by(ave_date_time_lubridate) %>% summarise_all(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE)))

ggplot(TB_2m_only_summary, aes(x = ave_date_time_lubridate, y = ORP_mean)) + geom_point(size=4)+
  geom_errorbar(aes(ymin=ORP_mean-ORP_sd, ymax=ORP_mean+ORP_sd)) +
  scale_x_datetime(date_breaks = "3 hours", date_labels="%m/%d %l %p") +
  xlab("Date Time, Year: 2021")+
  ylab("ORP (mV)")

# changing ORP and Turbidity full graphs for proposal:
ORP.plot <- ggplot(TB_all_2021, aes(x = ORP, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  #geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) + 
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("ORP (mV)")+
  ylab("Depth (m)")+
  ggtitle("")

ORP.plot

Turb.plot <- ggplot(TB_all_2021, aes(x = Turbidity, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
#  geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) + 
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("Turbidity (FNU)")+
  ylab("Depth (m)")+
  ggtitle("")

Turb.plot

ODO.plot <- ggplot(TB_all_2021, aes(x = ODO.1, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  #  geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) + 
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("DO (mg/L)")+
  ylab("Depth (m)")+
  ggtitle("")

ODO.plot

Cond.plot <- ggplot(TB_all_2021, aes(x = Cond, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  #  geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) + 
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("Cond (µS/cm)")+
  ylab("Depth (m)")+
  ggtitle("")

Cond.plot

pH.plot <- ggplot(TB_all_2021, aes(x = pH, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  #  geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) + 
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("pH")+
  ylab("Depth (m)")+
  ggtitle("")

pH.plot


Temp.plot <- ggplot(TB_all_2021, aes(x = Temp, y = -Depth, group=AveTimeDay, col=AveTimeDay)) + 
  #  geom_point(alpha = .5, size = 1, shape = TB_all_2021$Replicate) + 
  stat_smooth(aes(col=AveTimeDay, fill=AveTimeDay), method = "loess",span=0.03, orientation =  "y") +
  theme_bw() +
  theme(legend.title = element_blank())+
  xlab("Temperature ˚C")+
  ylab("Depth (m)")+
  ggtitle("")

Temp.plot






