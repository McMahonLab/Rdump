#Clean data list slate:
rm(list = ls())
TB_2018_07_09 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Culturing/2019-09-26_Charles.csv", header=T)
library(ggplot2)
library(cowplot)
ggplot(data=SS_all, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_grey(start=.8,end=.1) + theme(legend.position=c(.7,.4))
