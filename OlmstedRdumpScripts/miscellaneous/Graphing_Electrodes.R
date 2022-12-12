#----------Always start with a clean slate----------####
rm(list = ls())

setwd("/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult")
library(ggplot2)
library(cowplot)
library(reshape2)
# getting dataframes
ElectrodeProportionsAllTax <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/RealClean2018ElectrodeDataCombinedEasyLabels.csv", header=T)
OrderSumsProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/OrderSums_ElectrodeProportions.csv", header=T)
ClassSumsProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/ClassSums_ElectrodeProportions.csv", header=T)
PhylaSumsProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/PhylaSums_ElectrodeProportions.csv", header=T)
CustomSumsProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/CustomSumsEproportionsDeltaAlpha.csv", header=T)
MaxOtherProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/CustomSumsEproportionsMaxOther.csv", header=T)
UltraOtherProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/CustomSumsEproportionsUltraOther.csv", header=T)
Ultra2OtherProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/CustomSumsEproportionsUltra2Other.csv", header=T)
OrderedUltra2OtherProportions <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/OrderedSumsEproportionsUltra2Other.csv", header=T)
FirmecutesClassSums <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/FirmecutesClassSums.csv", header=T)
DeltaClassSums <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/DeltaproteobacteriaLineages.csv", header=T)
Greendudes <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/GreenDudes.csv", header=T)
custom2 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/CustomSumsTry2.csv", header=T)
custom2AB <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/CustomSumsTry2AB.csv", header=T)


# putting data in "long" format instead of "wide"
OrderSums.long <- melt(OrderSumsProportions,id.vars = "X")
ClassSums.long <- melt(ClassSumsProportions,id.vars = "X")
PhylaSums.long <- melt(PhylaSumsProportions,id.vars = "X")
CustomSumsProportions <- melt(CustomSumsProportions,id.vars = "X")
MaxOtherProportions <- melt(MaxOtherProportions,id.vars = "X")
Ultra2OtherProportions <- melt(Ultra2OtherProportions,id.vars = "X")
OrderedUltra2OtherProportions <- melt(OrderedUltra2OtherProportions,id.vars = "X")
FirmecutesClassSums <- melt(FirmecutesClassSums,id.vars = "X")
DeltaLin <- melt(DeltaClassSums,id.vars = "X")
Greendudes <- melt(Greendudes,id.vars = "X")
custom2 <- melt(custom2,id.vars = "X")
custom2AB <- melt(custom2AB,id.vars = "X")


ggplot(data=OrderSumsProportions) + geom_bar
ggplot(OrderSums.long, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity')
ggplot(PhylaSums.long, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity')
ggplot(ClassSums.long, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity')
ggplot(CustomSumsProportions, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity')
ggplot(MaxOtherProportions, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity') 

gg <- ggplot(MaxOtherProportions, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("Electrode 16S Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
gg+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

ff <- ggplot(UltraOtherProportions, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("Electrode 16S Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
ff+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

ff2 <- ggplot(Ultra2OtherProportions, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("Electrode 16S Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
ff2+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

ff3 <- ggplot(OrderedUltra2OtherProportions, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("Electrode 16S Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
ff3+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

firm <- ggplot(FirmecutesClassSums, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("Firmecute 16S Electrode Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
firm+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

Deltas <- ggplot(DeltaLin, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("Deltaproteobacteria 16S Electrode Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
Deltas+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

Greendudes <- ggplot(Greendudes, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("Green Bacteria 16S Electrode Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
Greendudes+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

custom2 <- ggplot(custom2, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("16S Electrode Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
custom2+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

custom2AB <- ggplot(custom2AB, aes(x = variable, y = value,fill=X)) + geom_bar(stat='identity', color="black", size=.2) + ggtitle("16S Electrode Profiles") + theme(legend.title=element_blank()) + xlab("Electrode abbreviation") + ylab("Proportion of 16S sequences") 
custom2AB+guides(fill=guide_legend(ncol=1,bycol=TRUE)) + scale_y_continuous(expand = c(0,0))

