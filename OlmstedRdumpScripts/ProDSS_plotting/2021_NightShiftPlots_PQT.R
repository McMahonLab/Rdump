#Clean data list slate:
rm(list = ls())
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(gapminder)
library(tidyverse)
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
TB_2021_08_19_1_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_1-1_Lv1.csv", header = T)
TB_2021_08_19_1_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_1-2_Lv1.csv", header = T)
TB_2021_08_19_1_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_1-3_Lv1.csv", header = T)
TB_2021_08_19_2_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_2-1_Lv1.csv", header = T)
TB_2021_08_19_2_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_2-2_Lv1.csv", header = T)
TB_2021_08_19_2_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_2-3_Lv1.csv", header = T)
TB_2021_08_19_3_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_3-1_Lv1.csv", header = T)
TB_2021_08_19_3_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_3-2_Lv1.csv", header = T)
TB_2021_08_19_3_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_3-3_Lv1.csv", header = T)
TB_2021_08_19_4_1 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_4-1_Lv1.csv", header = T)
TB_2021_08_19_4_2 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_4-2_Lv1.csv", header = T)
TB_2021_08_19_4_3 <- read.csv(file="/Users/cnolmsted/Desktop/BogData2021/Trout_Bog_2021-08-18to19_Lv1/Trout_Bog_2021-08-19_4-3_Lv1.csv", header = T)






TB_2021_08_18_1_1$TimeDay <- c("2:33pm 8-18")
TB_2021_08_18_1_2$TimeDay <- c("2:54pm 8-18")
TB_2021_08_18_1_3$TimeDay <- c("3:15pm 8-18")
TB_2021_08_18_2_1$TimeDay <- c("7:29pm 8-18")
TB_2021_08_18_2_2$TimeDay <- c("7:46pm 8-18")
TB_2021_08_18_2_3$TimeDay <- c("8:06pm 8-18")
TB_2021_08_18_3_1$TimeDay <- c("10:39pm 8-18")
TB_2021_08_18_3_2$TimeDay <- c("10:58pm 8-18")
TB_2021_08_18_3_3$TimeDay <- c("11:21pm 8-18")
TB_2021_08_19_1_1$TimeDay <- c("3:20am 8-19")
TB_2021_08_19_1_2$TimeDay <- c("3:42am 8-19")
TB_2021_08_19_1_3$TimeDay <- c("4:02am 8-19")
TB_2021_08_19_2_1$TimeDay <- c("5:25am 8-19")
TB_2021_08_19_2_2$TimeDay <- c("5:45am 8-19")
TB_2021_08_19_2_3$TimeDay <- c("6:02am 8-19")
TB_2021_08_19_3_1$TimeDay <- c("8:54am 8-19")
TB_2021_08_19_3_2$TimeDay <- c("9:15am 8-19")
TB_2021_08_19_3_3$TimeDay <- c("9:40am 8-19")
TB_2021_08_19_4_1$TimeDay <- c("1:50pm 8-19")
TB_2021_08_19_4_2$TimeDay <- c("2:08pm 8-19")
TB_2021_08_19_4_3$TimeDay <- c("2:25am 8-19")



TB_all_2021 <- rbind(TB_2021_08_18_1_1,TB_2021_08_18_1_2,TB_2021_08_18_1_3,TB_2021_08_18_2_1,TB_2021_08_18_2_2,TB_2021_08_18_2_3,TB_2021_08_18_3_1,TB_2021_08_18_3_2,TB_2021_08_18_3_3,TB_2021_08_19_1_1,TB_2021_08_19_1_2,TB_2021_08_19_1_3,TB_2021_08_19_2_1,TB_2021_08_19_2_2,TB_2021_08_19_2_3,TB_2021_08_19_3_1,TB_2021_08_19_3_2,TB_2021_08_19_3_3,TB_2021_08_19_4_1,TB_2021_08_19_4_2,TB_2021_08_19_4_3)
TB_all_2021 <- TB_all_2021[which(TB_all_2021$Turbidity < 5), ]

TB_all_2021$TimeDay <- factor(TB_all_2021$TimeDay, levels = c("2:33pm 8-18", "2:54pm 8-18", "3:15pm 8-18", "7:29pm 8-18", "7:46pm 8-18", "8:06pm 8-18", "10:39pm 8-18", "10:58pm 8-18", "11:21pm 8-18", "3:20am 8-19", "3:42am 8-19", "4:02am 8-19", "5:25am 8-19", "5:45am 8-19", "6:02am 8-19", "8:54am 8-19", "9:15am 8-19", "9:40am 8-19", "1:50pm 8-19", "2:08pm 8-19", "2:25am 8-19"))

#Export as 5 x 7.5

ggplot(data=TB_all_2021, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_all_2021, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_all_2021, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")



TB_3rd_2021 <- rbind(TB_2021_08_18_1_3,TB_2021_08_18_2_3,TB_2021_08_18_3_3,TB_2021_08_19_1_3,TB_2021_08_19_2_3,TB_2021_08_19_3_3,TB_2021_08_19_4_3)
TB_3rd_2021 <- TB_3rd_2021[which(TB_3rd_2021$Turbidity < 5), ]

TB_3rd_2021$TimeDay <- factor(TB_3rd_2021$TimeDay, levels = c("2:33pm 8-18", "2:54pm 8-18", "3:15pm 8-18", "7:29pm 8-18", "7:46pm 8-18", "8:06pm 8-18", "10:39pm 8-18", "10:58pm 8-18", "11:21pm 8-18", "3:20am 8-19", "3:42am 8-19", "4:02am 8-19", "5:25am 8-19", "5:45am 8-19", "6:02am 8-19", "8:54am 8-19", "9:15am 8-19", "9:40am 8-19", "1:50pm 8-19", "2:08pm 8-19", "2:25am 8-19"))

#Export as 5 x 7.5

ggplot(data=TB_3rd_2021, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_3rd_2021, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_3rd_2021, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

TB_2nd_2021 <- rbind(TB_2021_08_18_1_2,TB_2021_08_18_2_2,TB_2021_08_18_3_2,TB_2021_08_19_1_2,TB_2021_08_19_2_2,TB_2021_08_19_3_2,TB_2021_08_19_4_2)
TB_2nd_2021 <- TB_2nd_2021[which(TB_2nd_2021$Turbidity < 5), ]

TB_2nd_2021$TimeDay <- factor(TB_2nd_2021$TimeDay, levels = c("2:33pm 8-18", "2:54pm 8-18", "3:15pm 8-18", "7:29pm 8-18", "7:46pm 8-18", "8:06pm 8-18", "10:39pm 8-18", "10:58pm 8-18", "11:21pm 8-18", "3:20am 8-19", "3:42am 8-19", "4:02am 8-19", "5:25am 8-19", "5:45am 8-19", "6:02am 8-19", "8:54am 8-19", "9:15am 8-19", "9:40am 8-19", "1:50pm 8-19", "2:08pm 8-19", "2:25am 8-19"))

#Export as 5 x 7.5

ggplot(data=TB_2nd_2021, aes(x=Turbidity,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=ORP,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + theme(legend.position=c(.5,.3)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=Temp,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=ODO.1,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + theme(legend.position=c(.5,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")
ggplot(data=TB_2nd_2021, aes(x=pH,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + theme(legend.position=c(.03,.54)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")

ggplot(data=TB_2nd_2021, aes(x=Cond,y=Depth, group = TimeDay, color=TimeDay)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Conductivity Profiles") + labs(x="Conductivity(µS/cm)",y='Depth(m)') + theme(legend.position=c(.3,.4)) + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour")



TB_1st_2021 <- rbind(TB_2021_08_18_1_1,TB_2021_08_18_2_1,TB_2021_08_18_3_1,TB_2021_08_19_1_1,TB_2021_08_19_2_1,TB_2021_08_19_3_1,TB_2021_08_19_4_1)
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

# How to load all the CSV files from a folder.
path.to.files <- "~/Documents/Data-Charles/"

file.names.I.want.to.load <- list.files(path = path.to.files)

TB_all_2021 <- read.csv(file=paste0(path.to.files,file.names.I.want.to.load[1]))
TB_all_2021$Profile <- str_replace(file.names.I.want.to.load[1],"_Lv1.csv","")
TB_all_2021$first_time_point <- format(strptime(TB_all_2021$TIME[1],"%H:%M:%S"),'%H:%M')


for (file in 2:length(file.names.I.want.to.load)){
  my_file <- read.csv(file = paste0(path.to.files, file.names.I.want.to.load[file]))
  my_file$Profile <- str_replace(file.names.I.want.to.load[file],"_Lv1.csv","")
  my_file$first_time_point <- format(strptime(my_file$TIME[1],"%H:%M:%S"),'%H:%M')
  
  
  TB_all_2021 <- rbind(TB_all_2021, my_file)
  print(file)
}

TB_all_2021$TimeHourMinutes <- format(strptime(TB_all_2021$TIME,"%H:%M:%S"),'%H:%M')
TB_all_2021$DATE <- mdy(TB_all_2021$DATE)
TB_all_2021$Date_MonthDay <- format(as.Date(TB_all_2021$DATE), "%m-%d")

TB_all_2021$TimeDay <- paste(TB_all_2021$first_time_point, TB_all_2021$Date_MonthDay)


TB_all_2021 <- TB_all_2021 %>% filter(Turbidity < 5)


ggplot(TB_all_2021,  aes(x=ORP, y=-Depth,col=TimeDay))+
         geom_point()

TB_all_2021 <- separate(data=TB_all_2021,col=Profile,sep="_", into=c("Lake","Bog","Date","Profile"))
TB_all_2021  <- separate(data=TB_all_2021, col=Profile,sep="-", into=c("Profile","Replicate"))


ggplot(TB_all_2021,  aes(x=ORP, y=-Depth,col=Profile, shape=Replicate))+
  geom_point()


TB_all_2021 %>% group_by(Profile, first_time_point) %>% tally()

ORP.plot.with.dots <- ggplot(TB_all_2021, aes(x = ORP, y = -Depth, group=Profile, col=Profile)) + 
  geom_point() +
  stat_smooth(aes(col=Profile),
              method = "loess",span=0.01, orientation =  "y")+
  theme_bw()+
  ggtitle("ORP  with underlying data shown")


ORP.plot <- ggplot(TB_all_2021, aes(x = ORP, y = -Depth, group=Profile, col=Profile)) + 
  #geom_point() +
  stat_smooth(aes(col=Profile),
              method = "loess",span=0.01, orientation =  "y")+
  theme_bw()+
  ggtitle("ORP")

ORP.plot

ORP.plot.without1 <-ggplot(TB_all_2021 %>% filter(Replicate != 1), aes(x = ORP, y = -Depth, group=Profile, col=Profile)) + 
  #geom_point() +
  stat_smooth(aes(col=Profile),
              method = "loess",span=0.01, orientation =  "y")+
  theme_bw()+
  ggtitle("ORP without Replicate 1 of any profiles")

ORP.plot.without1

ORP.plot.without1.with.underlying.data.shown <-ggplot(TB_all_2021 %>% filter(Replicate != 1), aes(x = ORP, y = -Depth, group=Profile, col=Profile)) + 
  geom_point() +
  stat_smooth(aes(col=Profile),
              method = "loess",span=0.01, orientation =  "y")+
  theme_bw()+
  ggtitle("ORP without Replicate 1 of any profiles\nwith underlying data shown")


library(ggpubr)

ggarrange(ORP.plot.with.dots, ORP.plot.without1.with.underlying.data.shown, ORP.plot, ORP.plot.without1, labels=c("A","B","C","D"))

ggsave("~/Documents/Figures-Plots-ORP.pdf", width = 8.5, height = 8.5)




