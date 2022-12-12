#This what I ran to make MDS plots and then run ANOSIM on the electrodes from summer of 2018.
# based off example at http://geoffreyzahn.com/nmds-example/

library(vegan)
library(ggplot2)


otus = read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/2018Electrode_count_noNumbers.csv", header = TRUE, check.names = FALSE, row.names = 1)
metadata = read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/EET_Bioinformatics/Electrode16SRealResult/2018METADATA_Electrode_count_noNumbers.csv", header = TRUE, check.names = FALSE, row.names = 1)

#transposing otus into one column
t_otus <- as.data.frame(t(otus))

#next determine the best method for calculating a distance matrix
# actually I am just going to skip to doing Bray Curtis method because I the sqrt was taking a while
sqrt_t_otus = sqrt(t_otus)
rank.totus <- rankindex(as.matrix(sqrt_t_otus), t_otus, indices = c("bray", "euclid", "manhattan", "horn"), method = "spearman")
print(paste("The highest rank was given by the", names(sort(rank.totus, decreasing = TRUE)[1]), "method."))

#making a bray curtis dissimilarity matrix
otus_dist = as.matrix((vegdist(t_otus, "bray")))
#perform NMDS
NMDS = metaMDS(otus_dist)
#build a data frame with NMDS coordinates and metadata
MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]

NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Electrode = metadata$Electrode, Connection = metadata$Connection)
#or like this
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Connection = metadata$Connection, Electrode = metadata$Electrode_combined_name)

#peak at data
head(NMDS)

#ggplotting
library(cowplot)
ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Electrode)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plot")

#try this
library(cowplot)
ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Connection)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plot")

#getting a bit more detail in the plot
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Electrode_and_Material = metadata$Electrode_Material, Connection_and_Exposure = metadata$Connection_Exposure)
ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Electrode_and_Material)) +
#ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Connection_and_Exposure)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plotting Electrode and Material vs Connection and Exposure")

#what about this
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Electrode = metadata$Electrode, Material_Connection_Exposure = metadata$Material_Connection_Exposure)

ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Electrode)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plotting Electrode vs Material, Connection, and Exposure")


#running anosim on Electrode
anosim_electrode = anosim(otus_dist, metadata$Electrode)
anosim_electrode # take a look at results
summary(anosim_electrode)
plot(anosim_electrode)
#The ANOSIM statistic compares the mean of ranked dissimilarities between groups to the mean of ranked dissimilarities within groups. An R value close to "1.0" suggests dissimilarity between groups while an R value close to "0" suggests an even distribution of high and low ranks within and between groups.
#R value close to 1 indicates high separation between levels of your factor (e.g. control vs treatment samples), while R value close to 0 indicate no separation between levels of your factor.
#running anosim if Being on the outside mattered (Exposure)
anosim_Exposure = anosim(otus_dist, metadata$Exposure)
anosim_Exposure # take a look at results
summary(anosim_Exposure)
plot(anosim_Exposure)
#running anosim if being connected mattered
anosim_Connection = anosim(otus_dist, metadata$Connection)
anosim_Connection # take a look at results
summary(anosim_Connection)
plot(anosim_Connection)
#running anosim if depth and material combination mattered
anosim_Electrode_Material = anosim(otus_dist, metadata$Electrode_Material)
anosim_Electrode_Material # take a look at results
summary(anosim_Electrode_Material)
plot(anosim_Electrode_Material)
#running anosim if depth and material combination mattered
anosim_Material_Connection_Exposure = anosim(otus_dist, metadata$Material_Connection_Exposure)
anosim_Material_Connection_Exposure # take a look at results
summary(anosim_Material_Connection_Exposure)
plot(anosim_Material_Connection_Exposure)


