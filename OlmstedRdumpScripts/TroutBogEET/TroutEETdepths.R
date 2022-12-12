#Need to make taxa proportions for each depth

rm(list = ls())
library(ggplot2)
library(dplyr)
library(cowplot)
library(reshape2)
library(stringr)
theme_set(theme_cowplot())

#SamBinSummary <- read.csv("/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/Summaries/EETDiscreteSum/AllSampBinsSummary.csv", header = TRUE, stringsAsFactors=FALSE)

#SamBinSummary <- read.csv("/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/Summaries/EETDiscreteSum/SeparatedAllSampBinsEuksToo.csv", header = TRUE, stringsAsFactors=FALSE)
#Correcting:
#temp <- filter(SamBinSummary, EETcount == 0 & PutEET > 0) #oops
#temp <- filter(SamBinSummary, EETcount != PutEET + RedEET + OxiEET + MtrB) #oops
#SamBinSummary2 <- SamBinSummary %>% mutate(PutEET = ifelse(EETcount == 0 & PutEET > 0, 0, PutEET))
#write.csv(SamBinSummary2,"/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/Summaries/EETDiscreteSum/SeparatedAllSampBinsEuksTooCorrected.csv")
SamBinSummary <- read.csv("/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/Summaries/EETDiscreteSum/SeparatedAllSampBinsEuksTooCorrected.csv", header = TRUE, stringsAsFactors=FALSE)



MetaRelSummary <- read.csv("/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/DerepTaxAbund/SeparatedEukNamedAllSampBinsTaxCorrected.csv", header = TRUE, stringsAsFactors=FALSE)
#EukVsPro <- read.csv("/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/Summaries/EukVsPro/AllEukVsPro.csv", header = TRUE, stringsAsFactors=FALSE)

METEBOLIC_PA <- read.csv("/Users/cnolmsted/Documents/MchMahon_Lab/METABOLIC/TroutBogOnly/TroutBogMetabins/METABOLICwk1TransposedAbrevPA.csv",header = TRUE, stringsAsFactors=FALSE)
METEBOLIC_Num <- read.csv("/Users/cnolmsted/Documents/MchMahon_Lab/METABOLIC/TroutBogOnly/TroutBogMetabins/METABOLICwk1TransposedAbrevNums.csv",header = TRUE, stringsAsFactors=FALSE)

# s1 <- unique(SamBinSummary$Bin) #1301 bins
# s1 <- unique(SamBinSummary$Metabin) #702 before double corrections 698 after entries (681 before adding D2m2019 to the clustering) 
# write.csv(s1,"/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/Summaries/EETDiscreteSum/UniqueMetabins.csv")

# first making doing the taxonomic proportions for each sample, making a graph, and then also one where it's just the 2021 vertical profile
# then later I'll make the EET genes, oxi/red/put/total

#colnames(EukVsPro)[1] <- "Bin"

#SamBinEukVsPro <- merge(SamBinSummary,EukVsPro, by=c("Bin"), all=TRUE)
#SamBinEukVsPro[SamBinEukVsPro==""] <- NA
#SamBinEukVsPro <- SamBinEukVsPro %>% mutate(Eukaryotic = coalesce(Eukaryotic, 0))
#SamBinEukVsPro <- SamBinEukVsPro %>% mutate(Prokaryotic = coalesce(Prokaryotic, 0))
#SamBinEukVsPro <- SamBinEukVsPro %>% mutate(GTDB.tk=ifelse(is.na(GTDB.tk) & Eukaryotic > Prokaryotic, "d__Eukarya;p__;c__;o__;f__;g__;s__", GTDB.tk))
#SamBinEukVsPro <- SamBinEukVsPro %>% mutate(GTDB.tk=ifelse(is.na(GTDB.tk), "d__;p__;c__;o__;f__;g__;s__", GTDB.tk))
#write.csv(SamBinEukVsPro,"/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/Summaries/EETDiscreteSum/AllSampBinsEuksToo.csv")
#str(SamBinEukVsPro)
#sub(pattern, replacement, x)
#gsub(pattern, replacement, x)
#BinTax <- SamBinEukVsPro[c(1,4)]
#colnames(BinTax)[2] <- "GTDB.tk.Eukrep"
#MetaRelSummary_2 <- left_join(MetaRelSummary,BinTax, by=c("Metabin"="Bin"))
#MetaRelSummary_3 <- MetaRelSummary_2[c(1,2,7,4,5,6)]
#write.csv(MetaRelSummary_3,"/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/DepthCorrectAnalysis/DerepTaxAbund/EukNamedAllSampBinsTax.csv")

SamBinSummary[SamBinSummary==""] <- NA
MetaRelSummary[MetaRelSummary==""] <- NA
#if no domain (is.na), replace with unknown:
SamBinSummary <- SamBinSummary %>% mutate(Domain = coalesce(Domain, "Unknown"))


#removing weird taxa from metarelsummary t() = transpose which takes the list and command 'c' which means combine
#UniqPhyla <- data.frame(t(sapply(unique(MetaRelSummary$Phylum),c)))

UniqPhylaClass <- as.data.frame(do.call(cbind, list(unique(paste(MetaRelSummary$Phylum, MetaRelSummary$Class, sep = "_")))))
UniqPhyla <- as.data.frame(do.call(cbind, list(unique(MetaRelSummary$Phylum))))
#has: Firmicutes_C, Desulfobacterota_A, Chloroflexota_A 
UniqClass <- as.data.frame(do.call(cbind, list(unique(MetaRelSummary$Class))))
UniqOrder <- as.data.frame(do.call(cbind, list(unique(MetaRelSummary$Order))))
#has: Rhodospirillales_B
UniqFamily <- as.data.frame(do.call(cbind, list(unique(MetaRelSummary$Family))))
UniqGenus <- as.data.frame(do.call(cbind, list(unique(MetaRelSummary$Genus)))) 
#has: Methylopumilus_A, Magnetospirillum_A, Vitreoscilla_A, Geobacter_A, Pelobacter_C, Desulfovibrio_N, Taibaiella_B
UniqSpecies <- as.data.frame(do.call(cbind, list(unique(MetaRelSummary$Species))))

MetaRelSummary <- MetaRelSummary %>% mutate(Phylum = ifelse(Phylum == "Firmicutes_C", "Firmicutes", Phylum))
MetaRelSummary <- MetaRelSummary %>% mutate(Phylum = ifelse(Phylum == "Desulfobacterota_A", "Desulfobacterota", Phylum))
MetaRelSummary <- MetaRelSummary %>% mutate(Phylum = ifelse(Phylum == "Chloroflexota_A", "Chloroflexota", Phylum))
MetaRelSummary <- MetaRelSummary %>% mutate(Order = ifelse(Order == "Rhodospirillales_B", "Rhodospirillales", Order))
MetaRelSummary <- MetaRelSummary %>% mutate(Genus = ifelse(Genus == "Methylopumilus_A", "Methylopumilus", Genus))
MetaRelSummary <- MetaRelSummary %>% mutate(Genus = ifelse(Genus == "Magnetospirillum_A", "Magnetospirillum", Genus))
MetaRelSummary <- MetaRelSummary %>% mutate(Genus = ifelse(Genus == "Vitreoscilla_A", "Vitreoscilla", Genus))
MetaRelSummary <- MetaRelSummary %>% mutate(Genus = ifelse(Genus == "Geobacter_A", "Geobacter", Genus))
MetaRelSummary <- MetaRelSummary %>% mutate(Genus = ifelse(Genus == "Pelobacter_C", "Pelobacter", Genus))
MetaRelSummary <- MetaRelSummary %>% mutate(Genus = ifelse(Genus == "Desulfovibrio_N", "Desulfovibrio", Genus))
MetaRelSummary <- MetaRelSummary %>% mutate(Genus = ifelse(Genus == "Taibaiella_B", "Taibaiella", Genus))


MetaRelSummaryCustom <- MetaRelSummary


#Make custom taxa to highlight: Eukarya, Archaea, Unknown, Other, Chlorobia, Geothrix, order Geobacterales, family Ferrovaceae (or other order Burkholderiales?), Phyla Myxococcota, phyla Patescibacteria, phyla Verrucomicrobiota, phyla Planctomycetota, phyla Eremiobacterota, phyla Eisenbacteria, phyla Chloroflexota, phyla Armatimonadota
#The following misbehaves if you go from high to low taxa corrections, only works in reverse, but then you don't get the lower taxa if corrected by a higher so ...
#MetaRelSummaryCustom$Custom <- MetaRelSummaryCustom$Class
#MetaRelSummaryCustom <- MetaRelSummaryCustom %>% mutate(Custom = ifelse(Domain == "Eukarya" | Domain == "Archaea" | Domain == "Unknown", Domain, Custom))
#MetaRelSummaryCustom <- MetaRelSummaryCustom %>% mutate(Custom = ifelse(Phylum == "Myxococcota" | Phylum == "Patescibacteria" | Phylum == "Verrucomicrobiota" | Phylum == "Planctomycetota" | Phylum == "Eremiobacterota" | Phylum == "Eisenbacteria" | Phylum == "Chloroflexota" | Phylum == "Armatimonadota", Phylum, Custom))
#MetaRelSummaryCustom <- MetaRelSummaryCustom %>% mutate(Custom = ifelse(Order == "Geobacterales" | Order == "Burkholderiales", Order, Custom))
#MetaRelSummaryCustom <- MetaRelSummaryCustom %>% mutate(Custom = ifelse(Family == "Ferrovaceae", Family, Custom))
#MetaRelSummaryCustom <- MetaRelSummaryCustom %>% mutate(Custom = ifelse(Genus == "Polynucleobacter" | Genus == "Geothrix", Genus, Custom))

#these phyla are not included if done that way:
#Alphaproteobacteria, Gammaproteobacteria, Firmicutes, Campylobacterota, Bdellovibrionota, Bacteroidota, Actinobacteriota, Acidobacteriota 
#I think I need to include every class that is in the NA values, and Ferrovaceae is burkholderia
#NotIncluded2 <- filter(MetaRelSummaryCustom,is.na(MetaRelSummaryCustom$Custom))
#forgottenclasses2 <- as.data.frame(do.call(cbind, list(unique(NotIncluded2$Class))))

#Doesn't work quite right either, and still doesn't grab ferrovaceae and others
#MetaRelSummaryCustom$Custom <- MetaRelSummaryCustom$Class
#MetaRelSummaryCustom <- MetaRelSummaryCustom %>% mutate(Custom = ifelse(Domain == "Eukarya" | Domain == "Archaea" | Domain == "Unknown", Domain, 
#                                                                        ifelse(Phylum == "Myxococcota" | Phylum == "Patescibacteria" | Phylum == "Verrucomicrobiota" | Phylum == "Planctomycetota" | Phylum == "Eremiobacterota" | Phylum == "Eisenbacteria" | Phylum == "Chloroflexota" | Phylum == "Armatimonadota", Phylum,
#                                                                               ifelse(Class == "Gammaproteobacteria" | Class == "Alphaproteobacteria" | Class == "Negativicutes" | Class == "Campylobacteria" | Class == "Bacteriovoracia" | Class == "Bacteroidia" | Class == "Acidimicrobiia" | Class == "Actinobacteria" | Class == "Coriobacteriia" | Class == "Acidobacteriae" & Order != "Burkholderiales" & Genus != "Polynucleobacter", Class,
#                                                                                      ifelse(Order == "Geobacterales" | Order == "Burkholderiales" & Family != "Ferrovaceae", Order,
#                                                                                             ifelse(Family == "Ferrovaceae", Family, 
#                                                                                                    ifelse(Genus == "Polynucleobacter" | Genus == "Geothrix", Genus, Custom)))))))

#fuck it. I don't need to look at ferrovaceae in the charts, or Polynucleobacter or burkholderiales , so fuck, it I hate R, this doesn't make sense.
MetaRelSummaryCustom$Custom <- MetaRelSummaryCustom$Class
MetaRelSummaryCustom <- MetaRelSummaryCustom %>% mutate(Custom = ifelse(Domain == "Eukarya" | Domain == "Archaea" | Domain == "Unknown", Domain, 
                                                                        ifelse(Phylum == "Myxococcota" | Phylum == "Patescibacteria" | Phylum == "Verrucomicrobiota" | Phylum == "Planctomycetota" | Phylum == "Eremiobacterota" | Phylum == "Eisenbacteria" | Phylum == "Chloroflexota" | Phylum == "Armatimonadota", Phylum,
                                                                               ifelse(Class == "Gammaproteobacteria" | Class == "Alphaproteobacteria" | Class == "Negativicutes" | Class == "Campylobacteria" | Class == "Bacteriovoracia" | Class == "Bacteroidia" | Class == "Acidimicrobiia" | Class == "Actinobacteria" | Class == "Coriobacteriia" | Class == "Acidobacteriae", Class,
                                                                                      ifelse(Order == "Geobacterales" | Order == "Burkholderiales", Order,
                                                                                             ifelse(Family == "Ferrovaceae", Family, 
                                                                                                    ifelse(Genus == "Polynucleobacter" | Genus == "Geothrix", Genus, Custom)))))))



colnames(MetaRelSummaryCustom)
RelCustom <- MetaRelSummaryCustom[c(1,13,12)]

Depth2021MetaRel <- filter(RelCustom, Sample == "0m" | Sample == "1m" | Sample == "2m" | Sample == "3m" | Sample == "4m" | Sample == "5m" | Sample == "6m") 
Depth2021MetaRel <- Depth2021MetaRel  %>% group_by(Sample,Custom) %>% summarise_all(funs(sum))
Depth2021MetaRel$Sample <- factor(Depth2021MetaRel$Sample, levels = c("0m","1m","2m","3m","4m","5m","6m"))
#Depth2021MetaRel$Custom <- factor(Depth2021MetaRel$Custom, levels = c("Acidimicrobiia","Acidobacteriae","Actinobacteria","Alphaproteobacteria","Archaea","Armatimonadota","Bacteriovoracia","Bacteroidia","Campylobacteria","Chlorobia","Chloroflexota","Coriobacteriia","Desulfovibrionia","Eisenbacteria","Elusimicrobia","Eremiobacterota","Eukarya","Gammaproteobacteria","Geobacterales","Geothrix","Holophagae","Myxococcota","Negativicutes","Patescibacteria","Planctomycetota","Spirochaetia","Thermoleophilia","Unknown","Verrucomicrobiota"))

TB2020MetaRel <- filter(RelCustom, Sample == "TBE" | Sample == "TBH") 
TB2020MetaRel <- TB2020MetaRel  %>% group_by(Sample,Custom) %>% summarise_all(funs(sum))

ElectrodesRel <- filter(RelCustom, Sample == "Ch6-C" | Sample == "Ch4-A" | Sample == "Ch4-C" | Sample == "Ch12-A")
ElectrodesRel <- ElectrodesRel %>% group_by(Sample,Custom) %>% summarise_all(funs(sum))



#TBH TBE 2020-08-19

#Trout_hypo epi, from timeseries

#MetaRel0m <- filter(Depth2021MetaRel, Sample == "0m")
#sum(MetaRel0m$RelAbund) #.99999999

cbPalette[1]

UniqCustom <- as.data.frame(do.call(cbind, list(unique(RelCustom$Custom))))
#CustomList <- c("Verrucomicrobiota","Archaea","Spirochaetia","Gammaproteobacteria","Alphaproteobacteria","Planctomycetota","Patescibacteria","Myxococcota","Negativicutes","Eremiobacterota","Elusimicrobia","Eisenbacteria","Geobacterales","Desulfovibrionia","Chloroflexota","Campylobacteria","Bacteriovoracia","Bacteroidia","Chlorobia","Armatimonadota","Acidimicrobiia","Actinobacteria","Thermoleophilia","Coriobacteriia","Geothrix","Acidobacteriae","Holophagae","Unknown","Eukarya")
#cbPalette <- c("saddlebrown","grey50","firebrick","skyblue","royalblue","aquamarine","red","goldenrod","blue","deepskyblue","plum","steelblue","yellow","purple","green4","lightpink3","mediumvioletred","darkseagreen3","green","tan","maroon1","darkorange1","orangered","darkorange2","hotpink3","hotpink4","hotpink1","black","turquoise1")
#alphabetized:
CustomList <- c("Acidimicrobiia","Acidobacteriae","Actinobacteria","Alphaproteobacteria","Archaea","Armatimonadota","Bacteriovoracia","Bacteroidia","Campylobacteria","Chlorobia","Chloroflexota","Coriobacteriia","Desulfovibrionia","Eisenbacteria","Elusimicrobia","Eremiobacterota","Eukarya","Gammaproteobacteria","Geobacterales","Geothrix","Holophagae","Myxococcota","Negativicutes","Patescibacteria","Planctomycetota","Spirochaetia","Thermoleophilia","Unknown","Verrucomicrobiota")
cbPalette <- c("maroon1","hotpink4","darkorange1","royalblue","grey50","tan","mediumvioletred","darkseagreen3","lightpink3","green","green4","darkorange2","purple","steelblue","plum","deepskyblue","turquoise1","skyblue","yellow","hotpink3","hotpink1","goldenrod","blue","red","aquamarine","firebrick","orangered","black","saddlebrown")

#coloring function
scale_fill_man <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(cbPalette , CustomList), 
    ...
  )
}

#2021 depth discrete:
ggplot(Depth2021MetaRel, aes(x = RelAbund, y = Sample, fill=Custom)) + geom_bar(stat='identity', color="black", size=.2) + geom_text(aes(label = ifelse(RelAbund > .01, Custom, "")), angle = 90, position = position_stack(vjust = .5),size = 2.5) + ylab("Trout Bog Lake depth (m)") + xlab("Relative abundance on 08/17/2021") + scale_y_discrete(limits=rev) + theme(legend.title = element_blank(), legend.position = "left") + guides(fill = guide_legend(ncol = 1)) + scale_fill_man()

#2020 integrated epi and hypo
ggplot(TB2020MetaRel, aes(x = RelAbund, y = Sample, fill=Custom)) + geom_bar(stat='identity', color="black", size=.2) + geom_text(aes(label = ifelse(RelAbund > .01, Custom, "")), angle = 90, position = position_stack(vjust = .5),size = 2.5) + ylab("Trout Bog Lake integrated samples") + xlab("Relative abundance on 2020/08/19") + scale_y_discrete(limits=rev) + theme(legend.title = element_blank(), legend.position = "left") + guides(fill = guide_legend(ncol = 1)) + scale_fill_man()

#Electrodes:
ggplot(ElectrodesRel, aes(x = RelAbund, y = Sample, fill=Custom)) + geom_bar(stat='identity', color="black", size=.2) + geom_text(aes(label = ifelse(RelAbund > .01, Custom, "")), angle = 90, position = position_stack(vjust = .5),size = 2.5) + ylab("Trout Bog Lake electrodes") + xlab("Relative abundance in biofilms") + scale_y_discrete(labels=c("Ch12-A\n(10cm-G5.5m)","Ch4-A\n(10cm-5.5m)","Ch4-C\n(10cm-5.5m)","Ch6-C\n(2.25m-5.25m)")) + theme(legend.title = element_blank(), legend.position = "left") + guides(fill = guide_legend(ncol = 1)) + scale_fill_man()


UniqPhylaClass <- as.data.frame(do.call(cbind, list(unique(paste(SamBinSummary$Phylum, SamBinSummary$Class, sep = "_")))))
UniqPhyla <- as.data.frame(do.call(cbind, list(unique(SamBinSummary$Phylum))))
#has: Firmicutes_C, Desulfobacterota_A, Chloroflexota_A 
UniqClass <- as.data.frame(do.call(cbind, list(unique(SamBinSummary$Class))))
UniqOrder <- as.data.frame(do.call(cbind, list(unique(SamBinSummary$Order))))
#has: Rhodospirillales_B
UniqFamily <- as.data.frame(do.call(cbind, list(unique(SamBinSummary$Family))))
UniqGenus <- as.data.frame(do.call(cbind, list(unique(SamBinSummary$Genus)))) 
#has: Methylopumilus_A, Magnetospirillum_A, Vitreoscilla_A, Geobacter_A, Pelobacter_C, Desulfovibrio_N, Taibaiella_B
UniqSpecies <- as.data.frame(do.call(cbind, list(unique(SamBinSummary$Species))))

SamBinSummary <- SamBinSummary %>% mutate(Phylum = ifelse(Phylum == "Firmicutes_C", "Firmicutes", Phylum))
SamBinSummary <- SamBinSummary %>% mutate(Phylum = ifelse(Phylum == "Desulfobacterota_A", "Desulfobacterota", Phylum))
SamBinSummary <- SamBinSummary %>% mutate(Phylum = ifelse(Phylum == "Chloroflexota_A", "Chloroflexota", Phylum))
SamBinSummary <- SamBinSummary %>% mutate(Order = ifelse(Order == "Rhodospirillales_B", "Rhodospirillales", Order))
SamBinSummary <- SamBinSummary %>% mutate(Genus = ifelse(Genus == "Methylopumilus_A", "Methylopumilus", Genus))
SamBinSummary <- SamBinSummary %>% mutate(Genus = ifelse(Genus == "Magnetospirillum_A", "Magnetospirillum", Genus))
SamBinSummary <- SamBinSummary %>% mutate(Genus = ifelse(Genus == "Vitreoscilla_A", "Vitreoscilla", Genus))
SamBinSummary <- SamBinSummary %>% mutate(Genus = ifelse(Genus == "Geobacter_A", "Geobacter", Genus))
SamBinSummary <- SamBinSummary %>% mutate(Genus = ifelse(Genus == "Pelobacter_C", "Pelobacter", Genus))
SamBinSummary <- SamBinSummary %>% mutate(Genus = ifelse(Genus == "Desulfovibrio_N", "Desulfovibrio", Genus))
SamBinSummary <- SamBinSummary %>% mutate(Genus = ifelse(Genus == "Taibaiella_B", "Taibaiella", Genus))


SamBinSummaryCustom <- SamBinSummary
SamBinSummaryCustom$Custom <- SamBinSummaryCustom$Class
SamBinSummaryCustom <- SamBinSummaryCustom %>% mutate(Custom = ifelse(Domain == "Eukarya" | Domain == "Archaea" | Domain == "Unknown", Domain, 
                                                                        ifelse(Phylum == "Myxococcota" | Phylum == "Patescibacteria" | Phylum == "Verrucomicrobiota" | Phylum == "Planctomycetota" | Phylum == "Eremiobacterota" | Phylum == "Eisenbacteria" | Phylum == "Chloroflexota" | Phylum == "Armatimonadota", Phylum,
                                                                               ifelse(Class == "Gammaproteobacteria" | Class == "Alphaproteobacteria" | Class == "Negativicutes" | Class == "Campylobacteria" | Class == "Bacteriovoracia" | Class == "Bacteroidia" | Class == "Acidimicrobiia" | Class == "Actinobacteria" | Class == "Coriobacteriia" | Class == "Acidobacteriae", Class,
                                                                                      ifelse(Order == "Geobacterales" | Order == "Burkholderiales", Order,
                                                                                             ifelse(Family == "Ferrovaceae", Family, 
                                                                                                    ifelse(Genus == "Polynucleobacter" | Genus == "Geothrix", Genus, Custom)))))))

colnames(SamBinSummaryCustom)
RelCustom2 <- SamBinSummaryCustom[c(2,22,12)]
RelCustom2$Custom <- RelCustom2$Custom %>% replace(is.na(.),"Unknown")
Depth2021MetaRel2 <- filter(RelCustom2, Sample == "0m" | Sample == "1m" | Sample == "2m" | Sample == "3m" | Sample == "4m" | Sample == "5m" | Sample == "6m") 
Depth2021MetaRel2 <- Depth2021MetaRel2  %>% group_by(Sample,Custom) %>% summarise_all(funs(sum))
Depth2021MetaRel2$Sample <- factor(Depth2021MetaRel2$Sample, levels = c("0m","1m","2m","3m","4m","5m","6m"))

UniqCustom2 <- as.data.frame(do.call(cbind, list(unique(RelCustom2$Custom))))
CustomList <- c("Verrucomicrobiota","Archaea","Spirochaetia","Gammaproteobacteria","Alphaproteobacteria","Planctomycetota","Patescibacteria","Myxococcota","Negativicutes","Eremiobacterota","Elusimicrobia","Eisenbacteria","Geobacterales","Desulfovibrionia","Chloroflexota","Campylobacteria","Bacteriovoracia","Bacteroidia","Chlorobia","Armatimonadota","Acidimicrobiia","Actinobacteria","Thermoleophilia","Coriobacteriia","Geothrix","Acidobacteriae","Holophagae","Unknown","Eukarya")
cbPalette <- c("saddlebrown","grey50","firebrick","skyblue","royalblue","aquamarine","red","goldenrod","blue","deepskyblue","plum","steelblue","yellow","purple","green4","lightpink3","mediumvioletred","darkseagreen3","green","tan","maroon1","darkorange1","orangered","darkorange2","hotpink3","hotpink4","hotpink1","black","turquoise1")

#coloring function
scale_fill_man <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(cbPalette , CustomList), 
    ...
  )
}

ggplot(Depth2021MetaRel2, aes(x = RelAbund, y = Sample, fill=Custom)) + geom_bar(stat='identity', color="black", size=.2) + geom_text(aes(label = ifelse(RelAbund > .01, Custom, "")), angle = 90, position = position_stack(vjust = .5),size = 2.5) + ylab("Trout Bog Lake depth (m)") + xlab("Relative abundance on 08/17/2021") + scale_y_discrete(limits=rev) + theme(legend.title = element_blank(), legend.position = "left") + guides(fill = guide_legend(ncol = 1)) + scale_fill_man()

#pretty much the same. so, just continue with the metabins for taxonomy

#2019-07-29 D2m2019
D2m2019MetaRel <- filter(RelCustom2, Sample == "D2m2019") 
D2m2019MetaRel <- D2m2019MetaRel  %>% group_by(Sample,Custom) %>% summarise_all(funs(sum))
1 - sum(D2m2019MetaRel$RelAbund) # = 0.3269982
test <- data.frame(Sample=character(), Custom=character(), RelAbund=numeric(),stringsAsFactors=FALSE) 
test[nrow(test) + 1,] = c("D2m2019","Unknown",as.numeric("0.3269982"))
D2m2019MetaRel <- bind_rows(D2m2019MetaRel, test %>% type.convert(as.is=TRUE))
str(D2m2019MetaRel)
glimpse(D2m2019MetaRel)
ggplot(D2m2019MetaRel, aes(x = RelAbund, y = Sample, fill=Custom)) + geom_bar(stat='identity', color="black", size=.2) + geom_text(aes(label = ifelse(RelAbund > .01, Custom, "")), angle = 90, position = position_stack(vjust = .5),size = 2.5) + ylab("Trout Bog Lake 2m deep") + xlab("Relative abundance on 2019/07/29") + scale_y_discrete(labels=c("")) + theme(legend.title = element_blank(), legend.position = "left") + guides(fill = guide_legend(ncol = 1)) + scale_fill_man()

TB2019to2020 <- filter(RelCustom2, Sample == "D2m2019" | Sample == "TBE" | Sample == "TBH") 
TB2019to2020 <- TB2019to2020  %>% group_by(Sample,Custom) %>% summarise_all(funs(sum))

test <- data.frame(Sample=character(), Custom=character(), RelAbund=numeric(),stringsAsFactors=FALSE) 
test[nrow(test) + 1,] = c("D2m2019","Unknown",as.numeric("0.3269982"))
TB2019to2020 <- bind_rows(TB2019to2020, test %>% type.convert(as.is=TRUE))
TB2019to2020$Sample <- factor(TB2019to2020$Sample, levels = c("TBH","D2m2019","TBE"))

ggplot(TB2019to2020, aes(x = RelAbund, y = Sample, fill=Custom)) + geom_bar(stat='identity', color="black", size=.2) + geom_text(aes(label = ifelse(RelAbund > .01, Custom, "")), angle = 90, position = position_stack(vjust = .5),size = 2.5) + ylab("Trout Bog Lake samples") + xlab("Relative abundance on 2019/07/29 or 2020/08/19") + scale_y_discrete(labels=c("TBH 2020","2m 2019","TBE 2020")) + theme(legend.title = element_blank(), legend.position = "left") + guides(fill = guide_legend(ncol = 1)) + scale_fill_man()


# now time for EETRatio and EET/OTU using SamBinSummary
# First make it for each sample,then give each sample depths, and graph as dots on depth plot

colnames(SamBinSummary)

EETSamBinSummary <- SamBinSummary[c(1,2,12:19)]
EETSamBinSummaryBest <- filter(EETSamBinSummary, Completeness > 50 & Contamination < 10)

colnames(EETSamBinSummaryBest)[6] <- "TotEET"

EETSamBinSummaryBest$TotEET_PA <- ifelse(EETSamBinSummaryBest$TotEET > 0, 1, 0) # If bin contains EET value = 1, otherwise 0
EETSamBinSummaryBest$RedEET_PA <- ifelse(EETSamBinSummaryBest$RedEET > 0, 1, 0)
EETSamBinSummaryBest$OxiEET_PA <- ifelse(EETSamBinSummaryBest$OxiEET > 0, 1, 0)
EETSamBinSummaryBest$PutEET_PA <- ifelse(EETSamBinSummaryBest$PutEET > 0, 1, 0)
EETSamBinSummaryBest$MtrB_PA <- ifelse(EETSamBinSummaryBest$MtrB > 0, 1, 0)

BestSummaryNums <- EETSamBinSummaryBest[c(2,6:15)]
BestSummaryNums$N <- as.numeric("1")

BestSummary <- BestSummaryNums %>% group_by(Sample) %>% summarise_all(list(sum))

BestSummary$TotEETRatio <- BestSummary$TotEET_PA / BestSummary$N
BestSummary$RedEETRatio <- BestSummary$RedEET_PA / BestSummary$N
BestSummary$OxiEETRatio <- BestSummary$OxiEET_PA / BestSummary$N
BestSummary$PutEETRatio <- BestSummary$PutEET_PA / BestSummary$N
BestSummary$MtrBRatio <- BestSummary$MtrB_PA / BestSummary$N
BestSummary$TotEET_OTU <- BestSummary$TotEET / BestSummary$N
BestSummary$RedEET_OTU <- BestSummary$RedEET / BestSummary$N
BestSummary$OxiEET_OTU <- BestSummary$OxiEET / BestSummary$N
BestSummary$PutEET_OTU <- BestSummary$PutEET / BestSummary$N
BestSummary$MtrB_OTU <- BestSummary$MtrB / BestSummary$N

colnames(BestSummary)
BestSummaryMeltable <- BestSummary[c(1,13:22)]
melted.BestSummary <- melt(BestSummaryMeltable, id.vars = "Sample")
BestSummaryRatio <- melted.BestSummary %>% filter(str_detect(variable, 'Ratio'))
BestSummaryOTU <- melted.BestSummary %>% filter(str_detect(variable, '_OTU'))

BestSummary2021Ratio <- filter(BestSummaryRatio, Sample == "0m" | Sample == "1m" | Sample == "2m" | Sample == "3m" | Sample == "4m" | Sample == "5m" | Sample == "6m") 
BestSummary2021Ratio$Sample <- factor(BestSummary2021Ratio$Sample, levels = c("0m","1m","2m","3m","4m","5m","6m"))
BestSummary2021Ratio$Depth <- as.numeric(sub("m", "", BestSummary2021Ratio$Sample))

BestSummary2021OTU <- filter(BestSummaryOTU, Sample == "0m" | Sample == "1m" | Sample == "2m" | Sample == "3m" | Sample == "4m" | Sample == "5m" | Sample == "6m") 
BestSummary2021OTU$Sample <- factor(BestSummary2021OTU$Sample, levels = c("0m","1m","2m","3m","4m","5m","6m"))
BestSummary2021OTU$Depth <- as.numeric(sub("m", "", BestSummary2021OTU$Sample))


ggplot(BestSummary2021Ratio, aes(x = value, y = Depth, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,.25,.5,.75,1), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("Ratio of Trout Bog MAGs with EET genes") + scale_y_reverse(breaks=seq(0,6,1)) + scale_x_continuous(limits=c(0, 1), breaks=seq(0,1,.25)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB"))
ggplot(BestSummary2021OTU, aes(x = value, y = Depth, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,1,2,3,4,5,6), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("Number of EET genes per MAG") + scale_y_reverse(breaks=seq(0,6,1)) + scale_x_continuous(limits=c(0, 6), breaks=seq(0,6,1)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB"))


Bins0m <- filter(SamBinSummary, Sample == "0m")
Bins3m <- filter(SamBinSummary, Sample == "3m")
Bins4m <- filter(SamBinSummary, Sample == "4m")
Bins5m <- filter(SamBinSummary, Sample == "5m")

#double checking my 1000x evidence that I switched the depths correctly:
BestSummary2021OTU$Depth2 <- ifelse(BestSummary2021OTU$Depth == 2, 3, ifelse(BestSummary2021OTU$Depth == 3, 4, ifelse(BestSummary2021OTU$Depth == 4, 2, BestSummary2021OTU$Depth)))
BestSummary2021Ratio$Depth2 <- ifelse(BestSummary2021Ratio$Depth == 2, 3, ifelse(BestSummary2021Ratio$Depth == 3, 4, ifelse(BestSummary2021Ratio$Depth == 4, 2, BestSummary2021Ratio$Depth)))
ggplot(BestSummary2021Ratio, aes(x = value, y = Depth2, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,.25,.5,.75,1), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("Ratio of Trout Bog MAGs with EET genes") + scale_y_reverse(breaks=seq(0,6,1)) + scale_x_continuous(limits=c(0, 1), breaks=seq(0,1,.25)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB"))
ggplot(BestSummary2021OTU, aes(x = value, y = Depth2, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,1,2,3,4,5,6), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("Number of EET genes per MAG") + scale_y_reverse(breaks=seq(0,6,1)) + scale_x_continuous(limits=c(0, 6), breaks=seq(0,6,1)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB"))
#yeah still makes sense I think


BestSummaryElectrodeRatio <- filter(BestSummaryRatio, Sample == "Ch12-A" | Sample == "Ch4-A" | Sample == "Ch4-C" | Sample =="Ch6-C") 
BestSummaryElectrodeOTU <- filter(BestSummaryOTU, Sample == "Ch12-A" | Sample == "Ch4-A" | Sample == "Ch4-C" | Sample =="Ch6-C") 

ggplot(BestSummaryElectrodeRatio, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,.25,.5,.75,1), linetype="dotted") + ylab("Trout Bog Lake electrodes") + xlab("Ratio of MAGs with EET genes") + scale_x_continuous(limits=c(0, 1), breaks=seq(0,1,.25)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB")) + scale_y_discrete(labels=c("Ch12-A\n(10cm-G5.5m)","Ch4-A\n(10cm-5.5m)","Ch4-C\n(10cm-5.5m)","Ch6-C\n(2.25m-5.25m)"))
ggplot(BestSummaryElectrodeOTU, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,1,2,3,4,5,6,7), linetype="dotted") + ylab("Trout Bog Lake electrodes") + xlab("Number of EET genes per MAG") + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB")) + scale_x_continuous(limits=c(0, 7), breaks=seq(0,7,1)) + scale_y_discrete(labels=c("Ch12-A\n(10cm-G5.5m)","Ch4-A\n(10cm-5.5m)","Ch4-C\n(10cm-5.5m)","Ch6-C\n(2.25m-5.25m)"))


BestSummary2019to2020Ratio <- filter(BestSummaryRatio, Sample == "D2m2019" | Sample == "TBE" | Sample == "TBH")
BestSummary2019to2020OTU <- filter(BestSummaryOTU, Sample == "D2m2019" | Sample == "TBE" | Sample == "TBH")
BestSummary2019to2020OTU$Sample <- factor(BestSummary2019to2020OTU$Sample, levels = c("TBH","D2m2019","TBE"))
BestSummary2019to2020Ratio$Sample <- factor(BestSummary2019to2020Ratio$Sample, levels = c("TBH","D2m2019","TBE"))


ggplot(BestSummary2019to2020Ratio, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,.25,.5,.75,1), linetype="dotted") + ylab("Trout Bog Lake samples") + xlab("Ratio of MAGs with EET genes") + scale_x_continuous(limits=c(0, 1), breaks=seq(0,1,.25)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB")) + scale_y_discrete(labels=c("TBH 2020","2m 2019","TBE 2020"))
ggplot(BestSummary2019to2020OTU, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,1,2,3,4,5), linetype="dotted") +  ylab("Trout Bog Lake samples") + xlab("Number of EET genes per MAG") + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB")) + scale_x_continuous(limits=c(0, 5), breaks=seq(0,5,1)) + scale_y_discrete(labels=c("TBH 2020","2m 2019","TBE 2020")) 


BestSummaryTimeseriesRatio <- filter(BestSummaryRatio, Sample == "TroutBog_epilimnion_OSF" | Sample == "TroutBog_hypolimnion_OSF")
BestSummaryTimeseriesOTU <- filter(BestSummaryOTU, Sample == "TroutBog_epilimnion_OSF" | Sample == "TroutBog_hypolimnion_OSF")
BestSummaryTimeseriesOTU$Sample <- factor(BestSummaryTimeseriesOTU$Sample, levels = c("TroutBog_hypolimnion_OSF","TroutBog_epilimnion_OSF"))
BestSummaryTimeseriesRatio$Sample <- factor(BestSummaryTimeseriesRatio$Sample, levels = c("TroutBog_hypolimnion_OSF","TroutBog_epilimnion_OSF"))


ggplot(BestSummaryTimeseriesRatio, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,.25,.5,.75,1), linetype="dotted") + ylab("Trout Bog Lake Timeseries") + xlab("Ratio of MAGs with EET genes") + scale_x_continuous(limits=c(0, 1), breaks=seq(0,1,.25)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB")) + scale_y_discrete(labels=c("Hypolimnion","Epilimnion"))
ggplot(BestSummaryTimeseriesOTU, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,1,2,3), linetype="dotted") +  ylab("Trout Bog Lake Timeseries") + xlab("Number of EET genes per MAG") + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB")) + scale_x_continuous(limits=c(0, 3), breaks=seq(0,3,1)) + scale_y_discrete(labels=c("Hypolimnion","Epilimnion")) 

#now I want to make EET graphs where the number of EET genes for each bacteria takes into acount the relative abundance...
#But how...? #EET genes/MAG  could be EET gene values = sum of #EET genes/MAG

RelEETSamBinSummaryBest <- EETSamBinSummaryBest

RelEETSamBinSummaryBest$RelTotEET <- RelEETSamBinSummaryBest$TotEET * RelEETSamBinSummaryBest$RelAbund
RelEETSamBinSummaryBest$RelRedEET <- RelEETSamBinSummaryBest$RedEET * RelEETSamBinSummaryBest$RelAbund
RelEETSamBinSummaryBest$RelOxiEET <- RelEETSamBinSummaryBest$OxiEET * RelEETSamBinSummaryBest$RelAbund
RelEETSamBinSummaryBest$RelPutEET <- RelEETSamBinSummaryBest$PutEET * RelEETSamBinSummaryBest$RelAbund
RelEETSamBinSummaryBest$RelMtrB <- RelEETSamBinSummaryBest$MtrB * RelEETSamBinSummaryBest$RelAbund

colnames(RelEETSamBinSummaryBest)
RelBestSummaryNums <- RelEETSamBinSummaryBest[c(2,16:20)]
RelBestSummaryNums$N <- as.numeric("1")

RelBestSummary <- RelBestSummaryNums %>% group_by(Sample) %>% summarise_all(list(sum))


colnames(RelBestSummary)
RelBestSummaryMeltable <- RelBestSummary[c(1:6)]
melted.RelBestSummary <- melt(RelBestSummaryMeltable, id.vars = "Sample")
RelBestSummary2021 <- filter(melted.RelBestSummary, Sample == "0m" | Sample == "1m" | Sample == "2m" | Sample == "3m" | Sample == "4m" | Sample == "5m" | Sample == "6m") 
RelBestSummary2021$Sample <- factor(RelBestSummary2021$Sample, levels = c("0m","1m","2m","3m","4m","5m","6m"))
RelBestSummary2021$Depth <- as.numeric(sub("m", "", RelBestSummary2021$Sample))

RelBestSummaryElectrode <- filter(melted.RelBestSummary, Sample == "Ch12-A" | Sample == "Ch4-A" | Sample == "Ch4-C" | Sample =="Ch6-C") 


ggplot(RelBestSummary2021, aes(x = value, y = Depth, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,1,2,3,4,5,6,7), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("EET genes normalized to MAG abundance") + scale_y_reverse(breaks=seq(0,7,1)) + scale_x_continuous(limits=c(0, 7), breaks=seq(0,7,1)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB"))
ggplot(RelBestSummaryElectrode, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,1,2,3,4,5,6,7), linetype="dotted") + ylab("Trout Bog Lake electrodes") + xlab("Number of EET genes per MAG") + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("Total", "Reductive", "Oxidative", "Putative","MtrB")) + scale_x_continuous(limits=c(0, 7), breaks=seq(0,7,1)) + scale_y_discrete(labels=c("Ch12-A\n(10cm-G5.5m)","Ch4-A\n(10cm-5.5m)","Ch4-C\n(10cm-5.5m)","Ch6-C\n(2.25m-5.25m)"))

##########
#Adding Metabolic stuff together
##########

TaxMetEetCombined <- METEBOLIC_Num

TaxMetEetCombined <- mutate(TaxMetEetCombined, C1Oxid = mxaF + mdh + pmoA + pmoB + pmoC + mmoB + mmoD)
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitrogenResp = napA + napB + narG + narH + nrfH + nrfA + nrfD + nirB + nirD + nirK + nirS + octR + norB + norC + nosD + nosZ + hzoA + hzsA)
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitrateResp = napA + napB + narG + narH)
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitriteResp = nrfH + nrfA + nrfD + nirB + nirD + nirK + nirS + octR)
TaxMetEetCombined <- mutate(TaxMetEetCombined, SulfurAnyResp = dsrD + asrA + asrB + asrC + aprA + sat + sor + sreA + sreB + sreC)
TaxMetEetCombined <- mutate(TaxMetEetCombined, SulfateResp = aprA + sat)
TaxMetEetCombined <- mutate(TaxMetEetCombined, SulfurAnyOxid = fccB + sqr + sor + dsrA + dsrB + sdo + soxB + soxY + soxC)
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitrogenOxid = amoA + amoB + amoC + nxrA + nxrB)
TaxMetEetCombined <- mutate(TaxMetEetCombined, MethaneProduction = mcrA	+ mcrB + mcrC)
TaxMetEetCombined <- mutate(TaxMetEetCombined, H2uptake = fefe.group.a2 + fefe.group.a4 + nife.group.1 + nife.group.2ade + nife.group.2bc + nife.group.4hi)
TaxMetEetCombined <- mutate(TaxMetEetCombined, H2evolution = fefe.group.a13 + fefe.group.b + nife.group.4a.g)
TaxMetEetCombined <- mutate(TaxMetEetCombined, Hydrogenases = fefe.group.a13 + fefe.group.a2 + fefe.group.a4 + fefe.group.b + fefe.group.c1 + fefe.group.c2 + fefe.group.c3 + fe + nife.group.1 + nife.group.2ade + nife.group.2bc + nife.group.3abd + nife.group.4a.g + nife.group.4hi)


TaxMetEetCombined <- mutate(TaxMetEetCombined, C1Oxid_PA = ifelse((mxaF + mdh + pmoA + pmoB + pmoC + mmoB + mmoD) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitrogenResp_PA = ifelse((napA + napB + narG + narH + nrfH + nrfA + nrfD + nirB + nirD + nirK + nirS + octR + norB + norC + nosD + nosZ + hzoA + hzsA) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitrateResp_PA = ifelse((napA + napB + narG + narH) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitriteResp_PA = ifelse((nrfH + nrfA + nrfD + nirB + nirD + nirK + nirS + octR) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, SulfurAnyResp_PA = ifelse((dsrD + asrA + asrB + asrC + aprA + sat + sor + sreA + sreB + sreC) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, SulfateResp_PA = ifelse((aprA + sat) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, SulfurAnyOxid_PA = ifelse((fccB + sqr + sor + dsrA + dsrB + sdo + soxB + soxY + soxC) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, NitrogenOxid_PA = ifelse((amoA + amoB + amoC + nxrA + nxrB) > 0, 1, 0))
TaxMetEetCombined <- mutate(TaxMetEetCombined, MethaneProduction_PA = ifelse((mcrA	+ mcrB + mcrC) > 0, 1, 0))



#note these HMM hits include more EET genes than would be called by the EET gene pipeline

#MetaRelSummary_2 <- left_join(MetaRelSummary,BinTax, by=c("Metabin"="Bin"))
AllBinsMetSummary <- left_join(SamBinSummary,TaxMetEetCombined, by=c("Metabin"="Gene_abbreviation"))

#AllBinsMetSummary <- filter(AllBinsMetSummary, EETcount > 0) #699/1301=53%
#AllBinsMetSummary <- filter(AllBinsMetSummary, EETcount == 0) #699/1301=53%


#write.csv(AllBinsMetSummary,"/Users/cnolmsted/Documents/MchMahon_Lab/Manuscripts/BogLakeOxyclineEET/Tables/AllBinsSummary.csv")
colnames(EETSamBinSummaryBest)
colnames(AllBinsMetSummary)


AllBinsMetSum <- AllBinsMetSummary[c(1,3,4:10,337:354)]


EETSamBinSummaryBestMeta <- left_join(EETSamBinSummaryBest,AllBinsMetSum, by=c("Bin"="Bin"))


colnames(EETSamBinSummaryBestMeta)



RelSamBinMeta <- EETSamBinSummaryBestMeta

RelSamBinMeta$RelTotEET <- RelSamBinMeta$TotEET * RelSamBinMeta$RelAbund
RelSamBinMeta$RelRedEET <- RelSamBinMeta$RedEET * RelSamBinMeta$RelAbund
RelSamBinMeta$RelOxiEET <- RelSamBinMeta$OxiEET * RelSamBinMeta$RelAbund
RelSamBinMeta$RelPutEET <- RelSamBinMeta$PutEET * RelSamBinMeta$RelAbund
RelSamBinMeta$RelMtrB <- RelSamBinMeta$MtrB * RelSamBinMeta$RelAbund

RelSamBinMeta$RelNitrogenResp <- RelSamBinMeta$NitrogenResp * RelSamBinMeta$RelAbund
RelSamBinMeta$RelNitrateResp <- RelSamBinMeta$NitrateResp * RelSamBinMeta$RelAbund
RelSamBinMeta$RelNitriteResp <- RelSamBinMeta$NitriteResp * RelSamBinMeta$RelAbund
RelSamBinMeta$RelNitrogenOxid <- RelSamBinMeta$NitrogenOxid * RelSamBinMeta$RelAbund
RelSamBinMeta$RelSulfurAnyResp <- RelSamBinMeta$SulfurAnyResp * RelSamBinMeta$RelAbund
RelSamBinMeta$RelSulfateResp <- RelSamBinMeta$SulfateResp * RelSamBinMeta$RelAbund
RelSamBinMeta$RelSulfurAnyOxid <- RelSamBinMeta$SulfurAnyOxid * RelSamBinMeta$RelAbund
RelSamBinMeta$RelC1Oxid <- RelSamBinMeta$C1Oxid * RelSamBinMeta$RelAbund
RelSamBinMeta$RelH2uptake <- RelSamBinMeta$H2uptake * RelSamBinMeta$RelAbund
RelSamBinMeta$RelH2evolution <- RelSamBinMeta$H2evolution * RelSamBinMeta$RelAbund
RelSamBinMeta$RelHydrogenases <- RelSamBinMeta$Hydrogenases * RelSamBinMeta$RelAbund


colnames(RelSamBinMeta)
RelSumNums <- RelSamBinMeta[c(2,42:57)]
RelSumNums$N <- as.numeric("1")

RelSummary <- RelSumNums %>% group_by(Sample) %>% summarise_all(list(sum),na.rm=TRUE)

colnames(RelSummary)

#RelMeltable <- RelSummary[c(1:14)]
RelMeltable <- RelSummary[c(1,7,10,11,13,14,17)]

melted.RelMeltable <- melt(RelMeltable, id.vars = "Sample")
RelSummary2021 <- filter(melted.RelMeltable, Sample == "0m" | Sample == "1m" | Sample == "2m" | Sample == "3m" | Sample == "4m" | Sample == "5m" | Sample == "6m") 
RelSummary2021$Sample <- factor(RelSummary2021$Sample, levels = c("0m","1m","2m","3m","4m","5m","6m"))
RelSummary2021$Depth <- as.numeric(sub("m", "", RelSummary2021$Sample))

RelElectrode <- filter(melted.RelMeltable, Sample == "Ch12-A" | Sample == "Ch4-A" | Sample == "Ch4-C" | Sample =="Ch6-C") 

#ggplot(RelSummary2021, aes(x = value, y = Depth, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,.05,.1,.15,.2), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("Oxidoreductase genes normalized to MAG abundance") + scale_y_reverse(breaks=seq(0,7,1)) + scale_x_continuous(limits=c(0, .2), breaks=seq(0,.2,.05)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("N Reduction", "N Oxidation", "S Reduction", "S Oxidation","Methylotrophy","Hydrogenases"))

ggplot(RelSummary2021, aes(x = value, y = Depth, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,.25,.5,.75,1), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("Oxidoreductase genes normalized to MAG abundance") + scale_y_reverse(breaks=seq(0,7,1)) + scale_x_continuous(limits=c(0, 1), breaks=seq(0,1,.25)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("N Reduction", "N Oxidation", "S Reduction", "S Oxidation","Methylotrophy","Hydrogenases"))
ggplot(RelElectrode, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,.25,.5,.75,1), linetype="dotted") + ylab("Trout Bog Lake electrodes") + xlab("Oxidoreductase genes normalized to MAG abundance") + theme(legend.title = element_blank()) + scale_x_continuous(limits=c(0, 1), breaks=seq(0,1,.25)) + scale_y_discrete(labels=c("Ch12-A\n(10cm-G5.5m)","Ch4-A\n(10cm-5.5m)","Ch4-C\n(10cm-5.5m)","Ch6-C\n(2.25m-5.25m)")) + scale_color_discrete(labels=c("N Reduction", "N Oxidation", "S Reduction", "S Oxidation","Methylotrophy","Hydrogenases"))

asdf <- RelSummary2021[c(1:3)]

colnames(RelSamBinMeta)
SumNums <- RelSamBinMeta[c(2,24,25,28,30,31,35)]
SumNums$N <- as.numeric("1")

Summary <- SumNums %>% group_by(Sample) %>% summarise_all(list(sum),na.rm=TRUE)

Summary$NitrogenResp_OTU <- Summary$NitrogenResp / Summary$N
Summary$NitrogenOxid_OTU <- Summary$NitrogenOxid / Summary$N
Summary$SulfurResp_OTU <- Summary$SulfurAnyResp / Summary$N
Summary$SulfurOxid_OTU <- Summary$SulfurAnyOxid / Summary$N
Summary$C1Oxid_OTU <- Summary$C1Oxid / Summary$N
Summary$Hydrogenases_OTU <- Summary$Hydrogenases / Summary$N

colnames(Summary)
SumMeltable <- Summary[c(1,9:14)]

melted.SumMeltable <- melt(SumMeltable, id.vars = "Sample")
SumSummary2021 <- filter(melted.SumMeltable, Sample == "0m" | Sample == "1m" | Sample == "2m" | Sample == "3m" | Sample == "4m" | Sample == "5m" | Sample == "6m") 
SumSummary2021$Sample <- factor(SumSummary2021$Sample, levels = c("0m","1m","2m","3m","4m","5m","6m"))
SumSummary2021$Depth <- as.numeric(sub("m", "", SumSummary2021$Sample))

SumElectrode <- filter(melted.SumMeltable, Sample == "Ch12-A" | Sample == "Ch4-A" | Sample == "Ch4-C" | Sample =="Ch6-C") 

ggplot(SumSummary2021, aes(x = value, y = Depth, color=variable)) + geom_point(size=4) + geom_line(orientation="y", size=2) + geom_vline(xintercept=c(0,.5,1,1.5,2), linetype="dotted") + ylab("Trout Bog Lake depth (m) on 08/17/2021") + xlab("Number of oxidoreductase genes per MAG") + scale_y_reverse(breaks=seq(0,7,1)) + scale_x_continuous(limits=c(0, 2), breaks=seq(0,2,.5)) + theme(legend.title = element_blank()) + scale_color_discrete(labels=c("N Reduction", "N Oxidation", "S Reduction", "S Oxidation","Methylotrophy","Hydrogenases"))
ggplot(SumElectrode, aes(x = value, y = Sample, color=variable)) + geom_point(size=4) + geom_vline(xintercept=c(0,.5,1,1.5,2,2.5), linetype="dotted") + ylab("Trout Bog Lake electrodes") + xlab("Number of oxidoreductase genes per MAG") + theme(legend.title = element_blank()) + scale_x_continuous(limits=c(0, 2.5), breaks=seq(0,2.5,.5)) + scale_y_discrete(labels=c("Ch12-A\n(10cm-G5.5m)","Ch4-A\n(10cm-5.5m)","Ch4-C\n(10cm-5.5m)","Ch6-C\n(2.25m-5.25m)")) + scale_color_discrete(labels=c("N Reduction", "N Oxidation", "S Reduction", "S Oxidation","Methylotrophy","Hydrogenases"))






