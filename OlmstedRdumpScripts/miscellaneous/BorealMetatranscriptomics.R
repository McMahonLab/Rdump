# Averaging and standard deviations of EET genes throughout the GEODES experiment
#install.packages("patchwork")
rm(list = ls())
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_cowplot())



path.to.files <- "/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/ExpressionHypolimnionBacteria/FEET/"
FilesToLoad <- list.files(path = path.to.files)
TB_FEET_GEODES <- read.csv(file=paste0(path.to.files,FilesToLoad[1]), stringsAsFactors=FALSE)
TB_FEET_GEODES$GEODES <- str_replace(FilesToLoad[1],".csv","") %>% str_replace("FEETexpression","expression") %>% str_replace("REDOXexpression","expression") %>% str_replace("OxygenExpression","expression") %>% str_replace("expression","GEODES")

for (file in 2:length(FilesToLoad)){
  my_file <- read.csv(file=paste0(path.to.files,FilesToLoad[file]), stringsAsFactors=FALSE)
  my_file$GEODES <- str_replace(FilesToLoad[file],".csv","") %>% str_replace("FEETexpression","expression") %>% str_replace("REDOXexpression","expression") %>% str_replace("OxygenExpression","expression") %>% str_replace("expression","GEODES")
  TB_FEET_GEODES <- rbind(TB_FEET_GEODES, my_file)
  print(file)
}

path.to.files <- "/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/ExpressionEpilimnionBacteria/FEET/"
FilesToLoad <- list.files(path = path.to.files)

for (file in 2:length(FilesToLoad)){
  my_file <- read.csv(file=paste0(path.to.files,FilesToLoad[file]), stringsAsFactors=FALSE)
  my_file$GEODES <- str_replace(FilesToLoad[file],".csv","") %>% str_replace("FEETexpression","expression") %>% str_replace("REDOXexpression","expression") %>% str_replace("OxygenExpression","expression") %>% str_replace("expression","GEODES")
  TB_FEET_GEODES <- rbind(TB_FEET_GEODES, my_file)
  print(file)
}

path.to.files <- "/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/ExpressionHypolimnionBacteria/REDOX/"
FilesToLoad <- list.files(path = path.to.files)
TB_REDOX_GEODES <- read.csv(file=paste0(path.to.files,FilesToLoad[1]), stringsAsFactors=FALSE)
TB_REDOX_GEODES$GEODES <- str_replace(FilesToLoad[1],".csv","") %>% str_replace("FEETexpression","expression") %>% str_replace("REDOXexpression","expression") %>% str_replace("OxygenExpression","expression") %>% str_replace("expression","GEODES")

for (file in 2:length(FilesToLoad)){
  my_file <- read.csv(file=paste0(path.to.files,FilesToLoad[file]), stringsAsFactors=FALSE)
  my_file$GEODES <- str_replace(FilesToLoad[file],".csv","") %>% str_replace("FEETexpression","expression") %>% str_replace("REDOXexpression","expression") %>% str_replace("OxygenExpression","expression") %>% str_replace("expression","GEODES")
  TB_REDOX_GEODES <- rbind(TB_REDOX_GEODES, my_file)
  print(file)
}

path.to.files <- "/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/ExpressionEpilimnionBacteria/REDOX/"
FilesToLoad <- list.files(path = path.to.files)

for (file in 2:length(FilesToLoad)){
  my_file <- read.csv(file=paste0(path.to.files,FilesToLoad[file]), stringsAsFactors=FALSE)
  my_file$GEODES <- str_replace(FilesToLoad[file],".csv","") %>% str_replace("FEETexpression","expression") %>% str_replace("REDOXexpression","expression") %>% str_replace("OxygenExpression","expression") %>% str_replace("expression","GEODES")
  TB_REDOX_GEODES <- rbind(TB_REDOX_GEODES, my_file)
  print(file)
}

SupTbl6trout <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/METABOLIC/TroutBogOnly/TroutOnlySuppTbl6_TaxMetEetCombinedNA_Edit3.csv", header = T)
SupTbl6trout <- SupTbl6trout[c(0:102),]

TB_FEET_GEODES[ TB_FEET_GEODES == "zero" ] <- NA
TB_REDOX_GEODES[ TB_REDOX_GEODES == "zero" ] <- NA

GEODESdataall <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/sample_metadata.csv", header = T, stringsAsFactors=FALSE)
GEODESdata <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/sample_metadata.csv", header = T, stringsAsFactors=FALSE)
GEODESdata <- GEODESdata[c(40:70),]
GEODESdata <- GEODESdata[c(1:3)]

str(GEODESdata)

#merge then summarize like below
#UnfiltStErr <- SampTaxMetEetUnft %>% group_by(Lake) %>% summarise_all(funs(mean,sd))
colnames(TB_FEET_GEODES)
names(TB_FEET_GEODES)[names(TB_FEET_GEODES) == "GEODES"] <- "Sample"
names(TB_FEET_GEODES)[names(TB_FEET_GEODES) == "EETtpm"] <- "tpm"
names(TB_REDOX_GEODES)[names(TB_REDOX_GEODES) == "GEODES"] <- "Sample"
GEODESdata[ GEODESdata == "TroutHypo" ] <- "Hypolimnion"
GEODESdata[ GEODESdata == "Trout" ] <- "Epilimnion"
names(GEODESdata)[names(GEODESdata) == "Lake"] <- "Layer"

TB_FEET_GEODES <- merge(TB_FEET_GEODES,GEODESdata, by="Sample", all=TRUE)
TB_REDOX_GEODES <- merge(TB_REDOX_GEODES,GEODESdata, by="Sample", all=TRUE)

str(TB_FEET_GEODES)
str(TB_REDOX_GEODES)
TB_FEET_GEODES$NormReftpm <- as.numeric(as.character(TB_FEET_GEODES$NormReftpm))
TB_REDOX_GEODES$NormReftpm <- as.numeric(as.character(TB_REDOX_GEODES$NormReftpm))
TB_REDOX_GEODES$tpm <- as.numeric(as.character(TB_REDOX_GEODES$tpm))
TB_REDOX_GEODES$NormAveTpm <- as.numeric(as.character(TB_REDOX_GEODES$NormAveTpm))


TB_REDOX_GEODES <- TB_REDOX_GEODES[which(TB_REDOX_GEODES$PercentMatch > 50), ]
TB_FEET_GEODES <- TB_FEET_GEODES[which(TB_FEET_GEODES$PercentMatch > 50), ]

#I guess I don't need to look at the EET gene expression of all the bins which are basically the same between the epi and hypo, just one represenative.

#Grouping everything but the GEODES Sample
TB_REDOX_GEODES <- TB_REDOX_GEODES[c(2:4,6:11)]
TB_FEET_GEODES <- TB_FEET_GEODES[c(2:4,6:12)]

TB_FEET_Summary <- TB_FEET_GEODES %>% group_by(Layer,bin,orf,GEODESorf,FeGenie_HMM,Shaomei_Type,Timepoint) %>% summarise_all(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE)))
TB_FEET_Summary[ TB_FEET_Summary == "NaN" ] <- NA

TB_REDOX_Summary <- TB_REDOX_GEODES %>% group_by(Layer,bin,orf,GEODESorf,Oxidoreductase,Timepoint) %>% summarise_all(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE)))
TB_REDOX_Summary[ TB_REDOX_Summary == "NaN" ] <- NA

unique(SupTbl6trout$classification)
colnames(SupTbl6trout)

Taxonomy <- SupTbl6trout[c(3,5:12)]
names(Taxonomy)[names(Taxonomy) == "MAG"] <- "bin"

TB_FEET_Summary <- left_join(TB_FEET_Summary, Taxonomy, by=c("bin"="bin"))
TB_REDOX_Summary <- left_join(TB_REDOX_Summary, Taxonomy, by=c("bin"="bin"))

#TB_FEET_Summary[ TB_FEET_Summary == "Not found" ] <- ""
#TB_FEET_Summary$Oxidoreductase <- paste(TB_FEET_Summary$FeGenie_HMM, TB_FEET_Summary$Shaomei_Type, sep = " ")

TB_FEET_Summary <- TB_FEET_Summary %>% mutate(FeGenie_HMM=ifelse(FeGenie_HMM == "Not found", Shaomei_Type, FeGenie_HMM))
colnames(TB_FEET_Summary)
names(TB_FEET_Summary)[names(TB_FEET_Summary) == "FeGenie_HMM"] <- "Oxidoreductase"
TB_FEET_Summary <- TB_FEET_Summary[c(1:5,7:21)]

TB_Summary <- rbind(TB_FEET_Summary, TB_REDOX_Summary)

# graph structure [HypoExpression] [EpiExpression] [EpiTimeseriesExpression] EET = Green-Blue colors, REDOX = red-yellow colors

EpiOnly <- TB_FEET_GEODES %>% filter(Layer == "Epilimnion")
colnames(EpiOnly)
EpiOnly <- EpiOnly[c(1:8)]
EpiOnly <- EpiOnly %>% group_by(bin,orf,GEODESorf,FeGenie_HMM,Shaomei_Type) %>% summarise_all(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE)))
EpiOnly[ EpiOnly == "NaN" ] <- NA
EpiOnly <- EpiOnly %>% mutate(FeGenie_HMM=ifelse(FeGenie_HMM == "Not found", Shaomei_Type, FeGenie_HMM))
names(EpiOnly)[names(EpiOnly) == "FeGenie_HMM"] <- "Oxidoreductase"
EpiOnly <- EpiOnly[c(1:4,6:11)]
EpiRED <- TB_REDOX_GEODES %>% filter(Layer == "Epilimnion")
colnames(EpiRED)
EpiRED <- EpiRED[c(1:7)]
EpiRED <- EpiRED %>% group_by(bin,orf,GEODESorf,Oxidoreductase) %>% summarise_all(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE)))
EpiRED[ EpiRED == "NaN" ] <- NA

#making files which are useful for graphing the 3 panels:
Epi_total <- rbind(EpiOnly, EpiRED)
Epi_total <- left_join(Epi_total, Taxonomy, by=c("bin"="bin"))
Hypolimn <- TB_Summary %>% filter(Layer == "Hypolimnion")
EpiSeries <- TB_Summary %>% filter(Layer == "Epilimnion")

#Ambigufying Geobacter-like OMCs 
Epi_total[ Epi_total == "637127415 NP_953769 {omc} cytochrome c family protein [Geobacter sulfurreducens PCA: NC_002939]" ] <- "Geobacter-like OMC"
Epi_total[ Epi_total == "637778189 YP_383877 {omc} hypothetical protein [Geobacter metallireducens GS-15: NC_007517]" ] <- "Geobacter-like OMC"
Hypolimn[ Hypolimn == "637127415 NP_953769 {omc} cytochrome c family protein [Geobacter sulfurreducens PCA: NC_002939]" ] <- "Geobacter-like OMC"
Hypolimn[ Hypolimn == "637778189 YP_383877 {omc} hypothetical protein [Geobacter metallireducens GS-15: NC_007517]" ] <- "Geobacter-like OMC"
EpiSeries[ EpiSeries == "637127415 NP_953769 {omc} cytochrome c family protein [Geobacter sulfurreducens PCA: NC_002939]" ] <- "Geobacter-like OMC"
EpiSeries[ EpiSeries == "637778189 YP_383877 {omc} hypothetical protein [Geobacter metallireducens GS-15: NC_007517]" ] <- "Geobacter-like OMC"

Hypolimn$Timepoint <- as.character(Hypolimn$Timepoint)


unique(Epi_total$Oxidoreductase)
unique(Hypolimn$Oxidoreductase)
unique(EpiSeries$Oxidoreductase)
ifelse(unique(Epi_total$Oxidoreductase) == unique(EpiSeries$Oxidoreductase) , "yes", "no")
#all the same
sort(unique(Hypolimn$Oxidoreductase))
# OxiEET, RedEET, NovEET, OxygenRed, NitrogenRed, SulfurRed, OtherRed, NitrogenOxi,SulfurOxi, OtherOxi
# green/aqua/etc/purple, purple/red/orange/yellow, black, grey, brown, sand

Hypolimn$Oxidoreductase <- factor(Hypolimn$Oxidoreductase, levels = c("Cyc2_repCluster1","Cyc2_repCluster2","Cyc2_repCluster3","Cyc1","FoxZ","MtoA","MtrB_TIGR03509","MtrA","MtrC_TIGR03507","DFE_0448","DFE_0449","DFE_0461","DFE_0462","CbcB","CbcL","CymA","EetA","EetB","Geobacter-like OMC","ImcH","Outer Surface MHC","Outer Surface MHC in PCC","Periplasmic MHC in PCC","PCC porin","coxA","coxB","ccoN","ccoO","ccoP","cyoA","cyoB","cyoC","cyoD","cydA","cydB","napA","napB","narG","narH","nrfH","nrfA","nirB","nirD","nirK","nirS","octR","norB","norC","nosZ","aprA","sat","arsC1","arsC2","bcrA","bcrB","bcrC","bcrD","cld","YgfK","sqr","dsrA","dsrB","dsrD","sdo","soxB","soxC","soxY","mmoB","mxaF","pmoA","pmoB","pmoC","coxL","coxM","coxS"))
EpiSeries$Oxidoreductase <- factor(EpiSeries$Oxidoreductase, levels = c("Cyc2_repCluster1","Cyc2_repCluster2","Cyc2_repCluster3","Cyc1","FoxZ","MtoA","MtrB_TIGR03509","MtrA","MtrC_TIGR03507","DFE_0448","DFE_0449","DFE_0461","DFE_0462","CbcB","CbcL","CymA","EetA","EetB","Geobacter-like OMC","ImcH","Outer Surface MHC","Outer Surface MHC in PCC","Periplasmic MHC in PCC","PCC porin","coxA","coxB","ccoN","ccoO","ccoP","cyoA","cyoB","cyoC","cyoD","cydA","cydB","napA","napB","narG","narH","nrfH","nrfA","nirB","nirD","nirK","nirS","octR","norB","norC","nosZ","aprA","sat","arsC1","arsC2","bcrA","bcrB","bcrC","bcrD","cld","YgfK","sqr","dsrA","dsrB","dsrD","sdo","soxB","soxC","soxY","mmoB","mxaF","pmoA","pmoB","pmoC","coxL","coxM","coxS"))
Epi_total$Oxidoreductase <- factor(Epi_total$Oxidoreductase, levels = c("Cyc2_repCluster1","Cyc2_repCluster2","Cyc2_repCluster3","Cyc1","FoxZ","MtoA","MtrB_TIGR03509","MtrA","MtrC_TIGR03507","DFE_0448","DFE_0449","DFE_0461","DFE_0462","CbcB","CbcL","CymA","EetA","EetB","Geobacter-like OMC","ImcH","Outer Surface MHC","Outer Surface MHC in PCC","Periplasmic MHC in PCC","PCC porin","coxA","coxB","ccoN","ccoO","ccoP","cyoA","cyoB","cyoC","cyoD","cydA","cydB","napA","napB","narG","narH","nrfH","nrfA","nirB","nirD","nirK","nirS","octR","norB","norC","nosZ","aprA","sat","arsC1","arsC2","bcrA","bcrB","bcrC","bcrD","cld","YgfK","sqr","dsrA","dsrB","dsrD","sdo","soxB","soxC","soxY","mmoB","mxaF","pmoA","pmoB","pmoC","coxL","coxM","coxS"))



GeneList <- c("Cyc2_repCluster1","Cyc2_repCluster2","Cyc2_repCluster3","Cyc1","FoxZ","MtoA","MtrB_TIGR03509","MtrA","MtrC_TIGR03507","DFE_0448","DFE_0449","DFE_0461","DFE_0462","CbcB","CbcL","CymA","EetA","EetB","Geobacter-like OMC","ImcH","Outer Surface MHC","Outer Surface MHC in PCC","Periplasmic MHC in PCC","PCC porin","coxA","coxB","ccoN","ccoO","ccoP","cyoA","cyoB","cyoC","cyoD","cydA","cydB","napA","napB","narG","narH","nrfH","nrfA","nirB","nirD","nirK","nirS","octR","norB","norC","nosZ","aprA","sat","arsC1","arsC2","bcrA","bcrB","bcrC","bcrD","cld","YgfK","sqr","dsrA","dsrB","dsrD","sdo","soxB","soxC","soxY","mmoB","mxaF","pmoA","pmoB","pmoC","coxL","coxM","coxS")
cbPalette <- c("chartreuse1","chartreuse1","chartreuse1","chartreuse3","blue1","aquamarine2","darkorchid1","magenta","maroon1","red","red","red","red","darkgoldenrod1","lightgoldenrod1","yellow1","indianred","indianred1","darkred","tomato","darkorange","darkorange","orange","darkmagenta","black","black","black","black","black","black","black","black","black","black","black","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey30","grey58","grey58","slategrey","slategrey","sienna","sienna","sienna","sienna","lightsteelblue","slategrey","lightcyan3","lightcyan3","lightcyan3","lightcyan3","lightcyan3","lightcyan3","lightcyan3","lightcyan3","tan3","tan3","tan3","tan3","tan3","gray91","gray91","gray91")

#colScale2 <- scale_colour_manual(values=cbPalette)

#coloring function
scale_col_genes <- function(...){
    ggplot2:::manual_scale(
        'col', 
        values = setNames(cbPalette , GeneList), 
        ...
    )
}

#selecting the bin
#geothrix
GraphingFile <- Hypolimn %>% filter(bin == "3300020713-bin_7")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020713-bin_7")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020713-bin_7")

#Chlorobi GSB-A deep one
GraphingFile <- Hypolimn %>% filter(bin == "3300020734-bin_13")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020734-bin_13")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020734-bin_13")
#Chlorobi GSB-B Boom-bust one
GraphingFile <- Hypolimn %>% filter(bin == "3300021132-bin_18")
GraphingFile2 <- Epi_total %>% filter(bin == "3300021132-bin_18")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300021132-bin_18")


#Ferrovaceae 3300020726-bin_3
GraphingFile <- Hypolimn %>% filter(bin == "3300020726-bin_3")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020726-bin_3")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020726-bin_3")

#methanotroph 3300020682-bin_2
GraphingFile <- Hypolimn %>% filter(bin == "3300020682-bin_2")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020682-bin_2")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020682-bin_2")
#methanotroph 3300020682-bin_7
GraphingFile <- Hypolimn %>% filter(bin == "3300020682-bin_7")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020682-bin_7")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020682-bin_7")
#methanotroph 3300020734-bin_3
GraphingFile <- Hypolimn %>% filter(bin == "3300020734-bin_3")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020734-bin_3")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020734-bin_3")

#Methylotroph 3300020713-bin_6
GraphingFile <- Hypolimn %>% filter(bin == "3300020713-bin_6")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020713-bin_6")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020713-bin_6")
#Methylotroph 3300020734-bin_17
GraphingFile <- Hypolimn %>% filter(bin == "3300020734-bin_17")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020734-bin_17")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020734-bin_17")

#Steroidobacteraceae 3300021142-bin_34
GraphingFile <- Hypolimn %>% filter(bin == "3300021142-bin_34")
GraphingFile2 <- Epi_total %>% filter(bin == "3300021142-bin_34")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300021142-bin_34")

#Prolixibacteraceae 3300021135-bin_4
GraphingFile <- Hypolimn %>% filter(bin == "3300021135-bin_4")
GraphingFile2 <- Epi_total %>% filter(bin == "3300021135-bin_4")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300021135-bin_4")

#Polynucleobacter 3300020700-bin_2
GraphingFile <- Hypolimn %>% filter(bin == "3300020700-bin_2")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020700-bin_2")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020700-bin_2")
#Polynucleobacter 3300021135-bin_38
GraphingFile <- Hypolimn %>% filter(bin == "3300021135-bin_38")
GraphingFile2 <- Epi_total %>% filter(bin == "3300021135-bin_38")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300021135-bin_38")

#rhodoferax  3300020723-bin_4
GraphingFile <- Hypolimn %>% filter(bin == "3300020723-bin_4")
GraphingFile2 <- Epi_total %>% filter(bin == "3300020723-bin_4")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300020723-bin_4")
#Rhodoferax 3300021135-bin_29
GraphingFile <- Hypolimn %>% filter(bin == "3300021135-bin_29")
GraphingFile2 <- Epi_total %>% filter(bin == "3300021135-bin_29")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300021135-bin_29")

#Verrucomicrobia 3300021139-bin_24
GraphingFile <- Hypolimn %>% filter(bin == "3300021139-bin_24")
GraphingFile2 <- Epi_total %>% filter(bin == "3300021139-bin_24")
GraphingFile3 <- EpiSeries %>% filter(bin == "3300021139-bin_24")


#norm by average tpm
hypo <- ggplot(GraphingFile, aes(x = Family, y = NormAveTpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormAveTpm_mean-NormAveTpm_sd, ymax=NormAveTpm_mean+NormAveTpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("TPM normalized by total average ORF TPM") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

EpiTot <- ggplot(GraphingFile2, aes(x = Family, y = NormAveTpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormAveTpm_mean-NormAveTpm_sd, ymax=NormAveTpm_mean+NormAveTpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion (average)") + ylab("")+ theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 3))+
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, -.5), "cm"))+
  scale_col_genes()

Epies <- ggplot(GraphingFile3, aes(x = Timepoint, y = NormAveTpm_mean,col = Oxidoreductase)) + geom_point(size=4) + geom_text(label = GraphingFile3$Oxidoreductase, nudge_y = -0.07*max(GraphingFile3$NormAveTpm_mean, na.rm = TRUE), size = 2)+
  geom_errorbar(aes(ymin=NormAveTpm_mean-NormAveTpm_sd, ymax=NormAveTpm_mean+NormAveTpm_sd), width=2) + xlab("Epilimnion (sample hour)") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, -1.25), "cm"))+
  scale_col_genes()

#Transcripts per kilobase million
#putting them together
plot_grid(hypo, Epies, EpiTot,nrow=1,ncol=3, align = "h", rel_widths = c(1,1.8,2.3), axis = "a")


#norm by average of references
hypo <- ggplot(GraphingFile, aes(x = Family, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("TPM normalized by reference ORF TPM") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

EpiTot <- ggplot(GraphingFile2, aes(x = Family, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion (average)") + ylab("")+ theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 3))+
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, -.5), "cm"))+
  scale_col_genes()

Epies <- ggplot(GraphingFile3, aes(x = Timepoint, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4) + geom_text(label = GraphingFile3$Oxidoreductase, nudge_y = -0.07*max(GraphingFile3$NormReftpm_mean, na.rm = TRUE), size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=2) + xlab("Epilimnion (sample hour)") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, -1.25), "cm"))+
  scale_col_genes()
#Transcripts per kilobase million
#putting them together
plot_grid(hypo, Epies, EpiTot,nrow=1,ncol=3, align = "h", rel_widths = c(1,1.8,2.3), axis = "a")



######if you need to zoom in use this:
#norm by average tpm
hypo <- ggplot(GraphingFile, aes(x = Genus, y = NormAveTpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormAveTpm_mean-NormAveTpm_sd, ymax=NormAveTpm_mean+NormAveTpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("TPM normalized by total average ORF TPM") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, 10))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

EpiTot <- ggplot(GraphingFile2, aes(x = Genus, y = NormAveTpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormAveTpm_mean-NormAveTpm_sd, ymax=NormAveTpm_mean+NormAveTpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion (average)") + ylab("")+ theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 3))+
  coord_cartesian(ylim=c(0, 10))+
  theme(plot.margin = unit(c(.3, 0, 0, -.5), "cm"))+
  scale_col_genes()

Epies <- ggplot(GraphingFile3, aes(x = Timepoint, y = NormAveTpm_mean,col = Oxidoreductase)) + geom_point(size=4) + geom_text(label = GraphingFile3$Oxidoreductase, nudge_y = -0.07*max(GraphingFile3$NormAveTpm_mean, na.rm = TRUE), size = 2)+
  geom_errorbar(aes(ymin=NormAveTpm_mean-NormAveTpm_sd, ymax=NormAveTpm_mean+NormAveTpm_sd), width=2) + xlab("Epilimnion (sample hour)") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, 10))+
  theme(plot.margin = unit(c(.3, 0, 0, -1.25), "cm"))+
  scale_col_genes()

#Transcripts per kilobase million
#putting them together
plot_grid(hypo, Epies, EpiTot,nrow=1,ncol=3, align = "h", rel_widths = c(1,1.8,2.3), axis = "a")


#norm by average of references
hypo <- ggplot(GraphingFile, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("TPM normalized by reference ORF TPM") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, 10))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

EpiTot <- ggplot(GraphingFile2, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = GraphingFile2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion (average)") + ylab("")+ theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 3))+
  coord_cartesian(ylim=c(0, 10))+
  theme(plot.margin = unit(c(.3, 0, 0, -.5), "cm"))+
  scale_col_genes()

Epies <- ggplot(GraphingFile3, aes(x = Timepoint, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4) + geom_text(label = GraphingFile3$Oxidoreductase, nudge_y = -0.07*max(GraphingFile3$NormReftpm_mean, na.rm = TRUE), size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=2) + xlab("Epilimnion (sample hour)") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, 10))+
  theme(plot.margin = unit(c(.3, 0, 0, -1.25), "cm"))+
  scale_col_genes()
#Transcripts per kilobase million
#putting them together
plot_grid(hypo, Epies, EpiTot,nrow=1,ncol=3, align = "h", rel_widths = c(1,1.8,2.3), axis = "a")

write.csv(TB_Summary,"/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/MetatranscriptomicsSummary.csv")


### Taking out Timeseries, just hypo and epi averages and one 
#Chlorobi, Geothrix, Polynucleobacter, Methylococcales, Methylophilaceae, Ferrovaceae, Rhodoferax
#TPM normalized by references:

#Chlorobi GSB-A deep one
#Chlorobi_A1 <- Hypolimn %>% filter(bin == "3300020734-bin_13")
#Chlorobi_A2 <- Epi_total %>% filter(bin == "3300020734-bin_13")
#HypoChlorobi_A <- ggplot(Chlorobi_A1, aes(x = Class, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Chlorobi_A1$Oxidoreductase, nudge_x = 0, size = 2)+
#  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("TPM normalized by reference ORF TPM") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
#  coord_cartesian(ylim=c(0, NA))+
#  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
#  scale_col_genes()
#EpiChlorobi_A <- ggplot(Chlorobi_A2, aes(x = Class, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Chlorobi_A2$Oxidoreductase, nudge_x = 0.1, size = 2)+
#  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.position="none") +
#  coord_cartesian(ylim=c(0, NA))+
#  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
#  scale_col_genes()
#Chlorogi GSB-B Boom-bust
Chlorobi_B1 <- Hypolimn %>% filter(bin == "3300021132-bin_18")
Chlorobi_B2 <- Epi_total %>% filter(bin == "3300021132-bin_18")
HypoChlorobi_B <- ggplot(Chlorobi_B1, aes(x = Class, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Chlorobi_B1$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("TPM normalized by reference ORF TPM") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()
EpiChlorobi_B <- ggplot(Chlorobi_B2, aes(x = Class, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Chlorobi_B2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

#Geothrix 3300020713-bin_7
Geothrix1 <- Hypolimn %>% filter(bin == "3300020713-bin_7")
Geothrix2 <- Epi_total %>% filter(bin == "3300020713-bin_7")
HypoGeothrix <- ggplot(Geothrix1, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Geothrix1$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()
EpiGeothrix <- ggplot(Geothrix2, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Geothrix2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

#Polynucleobacter 3300020700-bin_2
Pnec1 <- Hypolimn %>% filter(bin == "3300020700-bin_2")
Pnec2 <- Epi_total %>% filter(bin == "3300020700-bin_2")
HypoPnec <- ggplot(Pnec1, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Pnec1$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()
EpiPnec <- ggplot(Pnec2, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Pnec2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

#Methylococcales 3300020682-bin_7
Methan1 <- Hypolimn %>% filter(bin == "3300020682-bin_7")
Methan2 <- Epi_total %>% filter(bin == "3300020682-bin_7")
HypoMethan <- ggplot(Methan1, aes(x = Order, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Methan1$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()
EpiMethan <- ggplot(Methan2, aes(x = Order, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Methan2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

# Methylophilaceae 3300020713-bin_6
Methyl1 <- Hypolimn %>% filter(bin == "3300020713-bin_6")
Methyl2 <- Epi_total %>% filter(bin == "3300020713-bin_6")
HypoMethyl <- ggplot(Methyl1, aes(x = Family, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Methyl1$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()
EpiMethyl <- ggplot(Methyl2, aes(x = Family, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Methyl2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

# Ferrovaceae 3300020726-bin_3
Ferrovaceae1 <- Hypolimn %>% filter(bin == "3300020726-bin_3")
Ferrovaceae2 <- Epi_total %>% filter(bin == "3300020726-bin_3")
HypoFerrovaceae <- ggplot(Ferrovaceae1, aes(x = Family, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Ferrovaceae1$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()
EpiFerrovaceae <- ggplot(Ferrovaceae2, aes(x = Family, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Ferrovaceae2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.position="none") +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

#Rhodoferax 3300020723-bin_4
Rhodoferax1 <- Hypolimn %>% filter(bin == "3300020723-bin_4")
Rhodoferax2 <- Epi_total %>% filter(bin == "3300020723-bin_4")
HypoRhodoferax <- ggplot(Rhodoferax1, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Rhodoferax1$Oxidoreductase, nudge_x = 0, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Hypolimnion") + ylab("") + theme(legend.position="none", axis.title.y=element_text(size=14)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()
EpiRhodoferax <- ggplot(Rhodoferax2, aes(x = Genus, y = NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(x = -.4)) + geom_text(label = Rhodoferax2$Oxidoreductase, nudge_x = 0.1, size = 2)+
  geom_errorbar(aes(ymin=NormReftpm_mean-NormReftpm_sd, ymax=NormReftpm_mean+NormReftpm_sd), width=.15, position = position_nudge(x = -.4)) + xlab("Epilimnion") + ylab("") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 3)) +
  coord_cartesian(ylim=c(0, NA))+
  theme(plot.margin = unit(c(.3, 0, 0, .1), "cm"))+
  scale_col_genes()

plot_grid(HypoChlorobi_B, EpiChlorobi_B, HypoGeothrix, EpiGeothrix, HypoPnec, EpiPnec, HypoMethan, EpiMethan, HypoMethyl, EpiMethyl, HypoFerrovaceae, EpiFerrovaceae, HypoRhodoferax, EpiRhodoferax, nrow=1,ncol=14, align = "h", rel_widths = c(1,1,1,1,1,1,1,1,1,1,1,1,1,2.3), axis = "a")

### Now for the horizontal versions cuz it'll look better:

HypoChlorobi_B <- ggplot(Chlorobi_B1, aes(y = Class, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Chlorobi_B1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2)) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiChlorobi_B <- ggplot(Chlorobi_B2, aes(y = Class, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Chlorobi_B2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoGeothrix <- ggplot(Geothrix1, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Geothrix1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 2), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2)) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiGeothrix <- ggplot(Geothrix2, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Geothrix2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoPnec <- ggplot(Pnec1, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Pnec1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2)) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiPnec <- ggplot(Pnec2, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Pnec2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoMethan <- ggplot(Methan1, aes(y = Order, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methan1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2)) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiMethan <- ggplot(Methan2, aes(y = Order, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methan2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoMethyl <- ggplot(Methyl1, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methyl1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2)) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiMethyl <- ggplot(Methyl2, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methyl2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoFerrovaceae <- ggplot(Ferrovaceae1, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Ferrovaceae1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2)) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiFerrovaceae <- ggplot(Ferrovaceae2, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Ferrovaceae2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoRhodoferax <- ggplot(Rhodoferax1, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Rhodoferax1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("Hypolimnion") + theme(legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2)) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiRhodoferax <- ggplot(Rhodoferax2, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Rhodoferax2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("Epilimnion") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))+ guides(col = guide_legend(ncol = 2), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()

combined <- HypoChlorobi_B + EpiChlorobi_B + HypoGeothrix + EpiGeothrix + HypoPnec + EpiPnec + HypoMethan + EpiMethan + HypoMethyl + EpiMethyl + HypoFerrovaceae + EpiFerrovaceae + HypoRhodoferax + EpiRhodoferax & theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))
combined + plot_layout(nrow=7,ncol=2,guides = "collect") + plot_annotation(title = "TPM normalized by reference ORF TPM")
#same thing:
#combined <- (HypoChlorobi_B | EpiChlorobi_B) / (HypoGeothrix | EpiGeothrix) / (HypoPnec | EpiPnec) / (HypoMethan | EpiMethan) / (HypoMethyl | EpiMethyl) / (HypoFerrovaceae | EpiFerrovaceae) / (HypoRhodoferax | EpiRhodoferax) & theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))
#combined + plot_layout(guides = "collect") + plot_annotation(title = "TPM normalized by reference ORF TPM")
#doesn't work:
#combined <- (HypoChlorobi_B | plot_spacer() | EpiChlorobi_B) / plot_spacer() / (HypoGeothrix | plot_spacer() | EpiGeothrix) / plot_spacer() / (HypoPnec | plot_spacer() | EpiPnec) / plot_spacer() / (HypoMethan | plot_spacer() | EpiMethan) / plot_spacer() / (HypoMethyl | plot_spacer() | EpiMethyl) / plot_spacer() / (HypoFerrovaceae | plot_spacer() | EpiFerrovaceae) / plot_spacer() / (HypoRhodoferax | plot_spacer() | EpiRhodoferax) & theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=8), legend.key.height = unit(0,"cm"))
#combined + plot_layout(widths = c(1,-.5,1,2,1,-.5,1,2,1,-.5,1,2,1,-.5,1,2,1,-.5,1,2,1,-.5,1,2,1,-.5,1), heights = c(1,1,1,-.2,1,1,1,-.2,1,1,1,-.2,1,1,1,-.2,1,1,1,-.2,1,1,1,-.2,1,1,1), guides = "collect") + plot_annotation(title = "TPM normalized by reference ORF TPM")





HypoChlorobi_B <- ggplot(Chlorobi_B1, aes(y = Class, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Chlorobi_B1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiChlorobi_B <- ggplot(Chlorobi_B2, aes(y = Class, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Chlorobi_B2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoGeothrix <- ggplot(Geothrix1, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Geothrix1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 2), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiGeothrix <- ggplot(Geothrix2, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Geothrix2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoPnec <- ggplot(Pnec1, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Pnec1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiPnec <- ggplot(Pnec2, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Pnec2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoMethan <- ggplot(Methan1, aes(y = Order, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methan1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiMethan <- ggplot(Methan2, aes(y = Order, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methan2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoMethyl <- ggplot(Methyl1, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methyl1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiMethyl <- ggplot(Methyl2, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Methyl2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoFerrovaceae <- ggplot(Ferrovaceae1, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Ferrovaceae1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiFerrovaceae <- ggplot(Ferrovaceae2, aes(y = Family, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Ferrovaceae2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
HypoRhodoferax <- ggplot(Rhodoferax1, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Rhodoferax1$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("Hypolimnion") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()
EpiRhodoferax <- ggplot(Rhodoferax2, aes(y = Genus, x =  NormReftpm_mean,col = Oxidoreductase)) + geom_point(size=4, position = position_nudge(y = -.4)) + geom_text(label = Rhodoferax2$Oxidoreductase, position = position_jitter(width = 0, height = .2, seed = 1), size = 2)+
  geom_errorbar(aes(xmin=NormReftpm_mean-NormReftpm_sd, xmax=NormReftpm_mean+NormReftpm_sd), width=.3, position = position_nudge(y = -.4)) + ylab("") + xlab("Epilimnion") + theme(legend.position="none", axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_cartesian(xlim=c(0, NA))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  scale_col_genes()

plot_grid(HypoChlorobi_B, EpiChlorobi_B, HypoGeothrix, EpiGeothrix, HypoPnec, EpiPnec, HypoMethan, EpiMethan, HypoMethyl, EpiMethyl, HypoFerrovaceae, EpiFerrovaceae, HypoRhodoferax, EpiRhodoferax, nrow=7,ncol=2, align = "vh", rel_widths = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1), axis = "b")

# how many of the top 10 TB mOTUs were EET++ ?

path.to.files <- "/Users/cnolmsted/Documents/MchMahon_Lab/GEODES_EET_stuff/BorealLakesEETpaperAnalysis/SummaryInfo/"
FilesToLoad <- list.files(path = path.to.files)
SumInfo_GEODES <- read.csv(file=paste0(path.to.files,FilesToLoad[1]), stringsAsFactors=FALSE)
SumInfo_GEODES$Sample <- str_replace(FilesToLoad[1],".csv","") %>% str_replace("EpiReferenceNumbers","GEODES") %>% str_replace("ReferenceNumbers","GEODES")

for (file in 2:length(FilesToLoad)){
  my_file <- read.csv(file=paste0(path.to.files,FilesToLoad[file]), stringsAsFactors=FALSE)
  my_file$Sample <- str_replace(FilesToLoad[file],".csv","") %>% str_replace("EpiReferenceNumbers","GEODES") %>% str_replace("ReferenceNumbers","GEODES")
  SumInfo_GEODES <- rbind(SumInfo_GEODES, my_file)
  print(file)
}

SumInfo_GEODES <- merge(SumInfo_GEODES,GEODESdata, by="Sample", all=TRUE)
colnames(SumInfo_GEODES)
SumInfo_GEODES <- SumInfo_GEODES[c(9,2:8)]
SumInfo_GEODES_Summary <- SumInfo_GEODES %>% group_by(Layer,bin) %>% summarise_all(funs(mean(.,na.rm = TRUE),sd(.,na.rm = TRUE)))

#EETexp >1?
colnames(TB_FEET_GEODES)
TBEET <- TB_FEET_GEODES[c(1,2,7,8,9)]
TBEET <- TBEET %>% group_by(bin,orf,Layer) %>% summarise_all(funs(mean(.,na.rm = TRUE)))
TBEET[is.na(TBEET)] <- 0
TBEET <- TBEET %>% mutate(EET=ifelse(NormAveTpm > 1 | NormReftpm > 1 , 1, 0))
TBEET <- TBEET[c(1,3,6)]
TBEET <- TBEET %>% group_by(bin,Layer) %>% summarise_all(funs(sum))

SumInfo_GEODES_Summary <- merge(SumInfo_GEODES_Summary,TBEET, by=c("bin","Layer"), all=TRUE)

SumInfo_sum_narm <- filter(SumInfo_GEODES_Summary, is.na(EET) == FALSE)

#how many EET++ with mean_ec greater than 1? 
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion") #48 
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & EET >= 1) #30
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & EET == 0) #18 
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 1) #34
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 1 & EET == 0) #11
filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 1 & EET >= 1) %>% summarise(sum(EET)) #59 total EET genes that were expressed over 1 
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 3) #22
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 3 & EET == 0) #6
filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 3 & EET >= 1) %>% summarise(sum(EET)) #48
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 10) #10
temp <- filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 10 & EET == 0) #0
filter(SumInfo_sum_narm, Layer == "Hypolimnion" & mean_ec_mean > 10 & EET >= 1) %>% summarise(sum(EET)) #31 

temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion") #48
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & EET >= 1) #6
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & EET == 0) #42
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 1) #20
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 1 & EET == 0) #17
filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 1 & EET >= 1) %>% summarise(sum(EET)) #7
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 3) #9
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 3 & EET == 0) #8
filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 3 & EET >= 1) %>% summarise(sum(EET)) #1
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 10) #6
temp <- filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 10 & EET == 0) #5
filter(SumInfo_sum_narm, Layer == "Epilimnion" & mean_ec_mean > 10 & EET >= 1) %>% summarise(sum(EET)) #1

+ annotate() # can specify positions, sizes