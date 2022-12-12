setwd("/Users/cnolmsted/Documents/R_scripts/Nanodrop")
# see https://github.com/DropInBiofuels/BioInformatics/NanoDrop.R for inspiration

rm(list = ls())
library(reshape2)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
header <- read.table("2021-02-04.ndv",nrows = 4, sep="\t",header=F, stringsAsFactors = FALSE)
values <- read.table("2021-02-04.ndv", skip=4, sep="\t",header=T, dec = ",", comment.char="?")

values <- values[c(2,4,16),]


curve.values <- subset(values, select = c(Sample.ID, X220:X748)) # extract values for plots
#protein <- subset(values, select = c(X260.280)) # extract protein contamination
md <- melt(curve.values, id=(c("Sample.ID"))) # transform to ggplot format
md$variable<-substring(md$variable,2) # remove X in front of variables
md <- md[which(md$value > 0), ]
md <- md[which(md$variable > 310), ]
class(md$variable)
class(md$value)
md$variable <- as.numeric(md$variable)
md$value <- as.numeric(md$value)

md1 <- md[which(md$Sample.ID == "oldgrowth_469"), ]
md1 <- md[which(md$Sample.ID !=  "blank469"), ]
md2 <- md[which(md$Sample.ID == "blank469"), ]
#making smooth through every point:
spline_int1 <- as.data.frame(spline(md1$variable, md1$value))
spline_int2 <- as.data.frame(spline(md2$variable, md2$value))

ggplot(md) + 
  geom_point(aes(x = variable, y = value, colour = Sample.ID)) +
  geom_line(data = spline_int1, aes(x = x, y = y)) +
  geom_line(data = spline_int2, aes(x = x, y = y)) +
  scale_x_continuous(breaks = seq(300, 750, by = 20)) + theme(legend.position=c(.7,.6)) + labs(x="Wavelength (nm) ",y='Absorbance') +
  geom_vline(xintercept = c(469,654), colour = "brown", size = 1) + #BChl-e
  geom_vline(xintercept = c(427,655), colour = "green", size = 1) + #BChl-d
  geom_vline(xintercept = c(364,585), colour = "purple", size = 1) #BChl-a and Rpal peak
# export 8x6 as 2021-02-05_469.pdf







