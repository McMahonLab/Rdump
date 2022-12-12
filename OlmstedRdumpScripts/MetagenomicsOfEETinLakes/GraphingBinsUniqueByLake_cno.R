#################### PREFACE ####

rm(list = ls()) # Clear environment
options(scipen=999) # Disable scientific notation (optional)


# PART 0: Install these packages for later use

library(ggplot2)
library(cowplot)
library(dplyr)
theme_set(theme_cowplot())
# SAMPLE NAMES INDEX 

# Colab = Refers to collaborator's data
# Nonco = Refers to non-collaborator's data (trout, crystal, mary, mendota, etc)
# _S = Data in this dataframe/table is listed by SAMPLE (A-1, A-2, A-3, etc)
# _L = Data in this dataframe/table is by LAKE
# Totals = Data originally from EET_totals CSVs
# Context = Data originally from samples contextual CSVs

# Notes for using this script:
# Pay attention to make sure that samples are getting matched with the correct lakes
# If the alphabetical order of samples is off, they may get mismatched




#################### Load CSVs ####

#colabTotals <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/All_Colab_EET_totals.csv", header = FALSE)

#noncoTotals <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/Non_Colab_EET_totals.csv", header = FALSE, stringsAsFactors=FALSE)
noncoTotals <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/FeGenie/EET-finder-Pipeline_plusD-Bond/Edited_Pipeline_Files/NewNon_Colab_EET_totals.csv", header = FALSE, stringsAsFactors=FALSE)

#ByLake <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/mOTUandBinEETByLake.csv", header = TRUE, stringsAsFactors=FALSE)
ByLake <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/FeGenie/EET-finder-Pipeline_plusD-Bond/Edited_Pipeline_Files/mOTUandBinEETByLake.csv", header = TRUE, stringsAsFactors=FALSE)

colabContext <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/samples_contextual_All_Colab.csv", header = TRUE)
noncoContext <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/samples_contextual_Non_Colab.csv", header = TRUE, stringsAsFactors=FALSE)

#Kept <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/Colab_EET_totals_70-10kept.csv", header = FALSE)
Kept <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/FeGenie/EET-finder-Pipeline_plusD-Bond/Edited_Pipeline_Files/NewColab_EET_totals_70-10kept.csv", header = FALSE)

#Removed <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/Colab_EET_totals_Removed.csv", header = FALSE, stringsAsFactors = FALSE)
Removed <- read.csv(file = "/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/FeGenie/EET-finder-Pipeline_plusD-Bond/Edited_Pipeline_Files/NewColab_EET_totals_Removed.csv", header = FALSE, stringsAsFactors = FALSE)


#################### Collaborator data by SAMPLE ####

#Read in the file and get it ready
colabTotals_S <- Kept
colabTotals_S <- aggregate(colabTotals_S$V3, by=list(Sample=colabTotals_S$V1), FUN=sum) #Aggregate (sum) values in column V3 (EET COUNT) by unique values in column V1 (SAMPLE NAME).
names(colabTotals_S)[names(colabTotals_S) == "x"] <- "EETCount"

colabContext <- arrange(colabContext,Sample)

# Making the KEPT columns
temp <- Kept
temp$V2 <- (1) # Bincount
temp$V3 <- ifelse(temp$V3 > 0, 1, 0) # If bin contains EET value = 1, otherwise 0
temp <- unique(within(temp, { 
  BinCount <- ave(V2, V1, FUN = sum, na.rm=T)
  HasEET <- ave(V3, V1, FUN = sum, na.rm=T)
  rm(V2, V3)
}))
temp <- temp %>% group_by(V1) %>% summarise_all(funs(sum)) # Group by sample-name and finish summing columns
colabTotals_S[, "EETRatio"] <- temp[, "HasEET"] / temp[, "BinCount"] #Find EET Ratio
colabTotals_S[, "EETOTU"] <- colabTotals_S[, "EETCount"] / temp[, "BinCount"] # Finding EETCount/BinCount (based on EETCount Kept)
colabTotals_S[, "BinCount"] <- temp[, "BinCount"]

# Making the REMOVED columns 
temp <- Removed
temp$V2 <- (1) # Bincount
temp$V3 <- ifelse(temp$V3 > 0, 1, 0) # If bin contains EET value = 1, otherwise 0
temp[nrow(temp) + 1,] = list("F-3",0,0)
temp <- unique(within(temp, { 
  BinCount <- ave(V2, V1, FUN = sum, na.rm=T)
  HasEET <- ave(V3, V1, FUN = sum, na.rm=T)
  rm(V2, V3)
}))
temp <- temp %>% group_by(V1) %>% summarise_all(funs(sum)) # Group by sample-name and finish summing columns
colabTotals_S[, "EETRatioREMOVED"] <- temp[, "HasEET"] / temp[, "BinCount"] #Find EET Ratio

# Finding the EET_Ratio means of KEPT vs REMOVED 
colMeans(colabTotals_S[ , "EETRatio", drop = FALSE], na.rm=T)
colMeans(colabTotals_S[ , "EETRatioREMOVED", drop = FALSE], na.rm=T)

# Deleting the REMOVED column (we don't need it anymore)
colabTotals_S <- colabTotals_S[ c(1:5) ]

colabTotals_S$DOC=colabContext[,"DOC..mg.L.1."] 
colabTotals_S$TOC=colabContext[,"TOC..mg.L.1."] 
colabTotals_S$SO4=colabContext[,"SO4...mg.L.1."]
colabTotals_S$FeTot=colabContext$FeII..uM. + colabContext$FeIII..uM.
colabTotals_S$ODO=colabContext[,"O2..mg.l.1."]
colabTotals_S$CH4=colabContext[,"CH4..uM."]
colabTotals_S$pH=colabContext[,"pH"]
colabTotals_S$DOCorTOC <- rowMeans(colabTotals_S[,c('DOC', 'TOC')], na.rm=TRUE)

colabTotals_S$Temperature=colabContext[,"Temp....C."]
colabTotals_S$NH4=colabContext[,"NH4..ug.L.1."]
colabTotals_S$NO2=colabContext[,"NO2..ug.L.1."]
colabTotals_S$NO3=colabContext[,"NO3..ug.L.1."]
colabTotals_S$NOX <- rowMeans(colabTotals_S[,c('NO2', 'NO3')], na.rm=TRUE)
colabTotals_S$PO4=colabContext[,"PO4...ug.L.1."]
colabTotals_S$Depth=colabContext[,"depth..m."]
colabTotals_S$CO2=colabContext[,"CO2..uM."]

#################### Noncollaborator data by SAMPLE ####

noncoTotals_S <- noncoTotals
noncoTotals_S <- aggregate(noncoTotals_S$V3, by=list(Sample=noncoTotals_S$V1), FUN=sum)
names(noncoTotals_S)[names(noncoTotals_S) == "x"] <- "EETCount"

temp <- noncoTotals
temp$V2 <- (1) # Bincount
temp$V3 <- ifelse(temp$V3 > 0, 1, 0) # If bin contains EET value = 1, otherwise 0
temp <- unique(within(temp, { 
  BinCount <- ave(V2, V1, FUN = sum, na.rm=T)
  HasEET <- ave(V3, V1, FUN = sum, na.rm=T)
  rm(V2, V3)
}))
temp <- temp %>% group_by(V1) %>% summarise_all(funs(sum)) # Group by sample-name and finish summing columns

noncoTotals_S[, "EETRatio"] <- temp[, "HasEET"] / temp[, "BinCount"] #Find EET Ratio
noncoTotals_S[, "EETOTU"] <- noncoTotals_S[, "EETCount"] / temp[, "BinCount"] # Finding EETCount/BinCount (based on EETCount Kept)
noncoTotals_S[, "BinCount"] <- temp[, "BinCount"]

#Grab wanted data

temp <- noncoContext[ , c(1, 3, 4, 5, 6, 7, 8) ] #Remove Source.File and Sample Name column
temp <- aggregate(.~Sample, temp, FUN=mean, na.rm=TRUE, na.action="na.pass")

noncoTotals_S$DOC=temp[,"DOC..mg.L.1."] 
noncoTotals_S$TOC <- (NA)
noncoTotals_S$SO4=temp[,"so4...mg.L."]
noncoTotals_S$FeTot=temp[,"FeTot..uM."]
noncoTotals_S$ODO=temp[,"ODO..mg.L."]
noncoTotals_S$CH4 <- (NA) #There is no methane data, but we need to keep the dataframes the same so that we can combine them
noncoTotals_S$pH=temp[,"pH"]
noncoTotals_S$DOCorTOC=temp[,"DOC..mg.L.1."]

noncoTotals_S$Temperature <- (NA)
noncoTotals_S$NH4 <- (NA)
noncoTotals_S$NO2 <- (NA)
noncoTotals_S$NO3 <- (NA)
noncoTotals_S$NOX <- (NA)
noncoTotals_S$PO4 <- (NA)
noncoTotals_S$Depth <- (NA)
noncoTotals_S$CO2 <- (NA)

#################### All by LAKE ####
#################################Charles's method#######################################################################################
##################################Where each bin is in a lake only once##################################################################
colabTotals_L2 <- ByLake

colabTotals_L2 <- aggregate(colabTotals_L2$EETgenes, by=list(lake=colabTotals_L2$lake), FUN=sum) #Aggregate (sum) values in column V4 (EET COUNT) by unique values in column V1 (lake NAME).
names(colabTotals_L2)[names(colabTotals_L2) == "x"] <- "EETCount"

colabContextLake <- arrange(colabContext,Lake)

temp <- ByLake
temp$bin <- (1) # Bincount
temp$EETgenes <- ifelse(temp$EETgenes > 0, 1, 0) # If bin contains EET value = 1, otherwise 0
temp <- unique(within(temp, { 
  BinCount <- ave(bin, lake, FUN = sum, na.rm=T)
  HasEET <- ave(EETgenes, lake, FUN = sum, na.rm=T)
  rm(bin,sample,EETgenes,mOTU)
}))
temp <- temp %>% group_by(lake) %>% summarise_all(funs(sum)) # Group by lake and finish summing columns
colabTotals_L2[, "EETRatio"] <- temp[, "HasEET"] / temp[, "BinCount"] #Find EET Ratio
colabTotals_L2[, "EETOTU"] <- colabTotals_L2[, "EETCount"] / temp[, "BinCount"] # Finding EETCount/BinCount (based on EETCount Kept)
colabTotals_L2[, "BinCount"] <- temp[, "BinCount"]




# Assign lake IDs to colabTotals_S
colabTotals_S$Lake=colabContext$Lake
colabTotals_L <- colabTotals_S
colabTotals_L <- colabTotals_L[ c(2:22) ] # Remove Sample column, keep Lake column
colabTotals_L <- colabTotals_L %>% group_by(Lake) %>% summarise_all(mean, na.rm = TRUE) # Average by Lake (since theres multiple samples per lake)
colabTotals_L2 <- colabTotals_L2 %>% rename(Lake = "lake")

colabTotals_L3 <- cbind(colabTotals_L2,colabTotals_L[ c(6:21)])
colabTotals_L <- colabTotals_L3

# Assign lake IDs to noncoTotals_S
temp <- noncoContext
temp <- unique( temp[ , 1:2 ] )
temp <- arrange(temp,Sample_Name)
noncoTotals_S$Lake=temp$Sample 
noncoTotals_L <- noncoTotals_S[ c(2:22) ] # Remove Sample column, keep Lake column

#################### StdDev + StdErr ####

# Create dataframes for holding the StdDev and StdErrors 
colabStd <- as.data.frame(colabTotals_L$Lake)  # colab dataframe base
noncoStd <- as.data.frame(noncoTotals_L$Lake)  # nonco dataframe base

# COLAB TALLY
temp <- colabTotals_S %>% group_by(Lake) %>% tally() #Count how many samples there are for each lake in Lake column
colabStd$SampleCount=temp$n

# NONCO TALLY 
noncoDataTally <- noncoContext
noncoDataTally <- noncoDataTally[ , c(3:8) ] #Remove Source.File and Sample and Lake columns
noncoDataTally[is.na(noncoDataTally)] <- 0 # Turn all NA values to 0
noncoDataTally[noncoDataTally > 0] <- 1 # Turn all values bigger than 0 to 1
noncoDataTally$Sample <- noncoContext$Sample #Add Samples column back
noncoDataTally <- aggregate(.~Sample, noncoDataTally, FUN=sum, na.rm=TRUE, na.action="na.pass") #Add em up!

# COLAB
# EET RATIO
temp <- aggregate(colabTotals_S$EETRatio, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) # Aggregate by Lake while calculating standard deviation by lake
colabStd$EET_StdDev <- temp[c(2)] 
colabTotals_L$EET_StdErr=unlist(colabStd$EET_StdDev / sqrt(colabStd$SampleCount)) #Calculate the standard error
# EETOTU
temp <- aggregate(colabTotals_S$EETOTU, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$EETOTU_StdDev <- temp[c(2)] 
colabTotals_L$EETOTU_StdErr=unlist(colabStd$EETOTU_StdDev / sqrt(colabStd$SampleCount)) 
# SO4
temp <- aggregate(colabTotals_S$SO4, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$SO4_StdDev <- temp[c(2)] 
colabTotals_L$SO4_StdErr=unlist(colabStd$SO4_StdDev / sqrt(colabStd$SampleCount))
# FeTot
temp <- aggregate(colabTotals_S$FeTot, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$FeTot_StdDev <- temp[c(2)] 
colabTotals_L$FeTot_StdErr=unlist(colabStd$FeTot_StdDev / sqrt(colabStd$FeTot))
# DOC
temp <- aggregate(colabTotals_S$DOC, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$DOC_StdDev <- temp[c(2)] 
colabTotals_L$DOC_StdErr=unlist(colabStd$DOC_StdDev / sqrt(colabStd$SampleCount))
# ODO
temp <- aggregate(colabTotals_S$ODO, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$ODO_StdDev <- temp[c(2)] 
colabTotals_L$ODO_StdErr=unlist(colabStd$ODO_StdDev / sqrt(colabStd$SampleCount))
#pH
temp <- aggregate(colabTotals_S$pH, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$pH_StdDev <- temp[c(2)] 
colabTotals_L$pH_StdErr=unlist(colabStd$pH_StdDev / sqrt(colabStd$SampleCount))
#CH4
temp <- aggregate(colabTotals_S$CH4, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$CH4_StdDev <- temp[c(2)] 
colabTotals_L$CH4_StdErr=unlist(colabStd$CH4_StdDev / sqrt(colabStd$SampleCount))
#DOCorTOC
temp <- aggregate(colabTotals_S$DOCorTOC, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$DOCorTOC_StdDev <- temp[c(2)] 
colabTotals_L$DOCorTOC_StdErr=unlist(colabStd$DOCorTOC_StdDev / sqrt(colabStd$SampleCount))

#Temperature
temp <- aggregate(colabTotals_S$Temperature, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$Temperature_StdDev <- temp[c(2)] 
colabTotals_L$Temperature_StdErr=unlist(colabStd$Temperature_StdDev / sqrt(colabStd$SampleCount))
#NH4
temp <- aggregate(colabTotals_S$NH4, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$NH4_StdDev <- temp[c(2)] 
colabTotals_L$NH4_StdErr=unlist(colabStd$NH4_StdDev / sqrt(colabStd$SampleCount))
#NO3
temp <- aggregate(colabTotals_S$NO3, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$NO3_StdDev <- temp[c(2)] 
colabTotals_L$NO3_StdErr=unlist(colabStd$NO3_StdDev / sqrt(colabStd$SampleCount))
#NO2
temp <- aggregate(colabTotals_S$NO2, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$NO2_StdDev <- temp[c(2)] 
colabTotals_L$NO2_StdErr=unlist(colabStd$NO2_StdDev / sqrt(colabStd$SampleCount))
#NOX
temp <- aggregate(colabTotals_S$NOX, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$NOX_StdDev <- temp[c(2)] 
colabTotals_L$NOX_StdErr=unlist(colabStd$NOX_StdDev / sqrt(colabStd$SampleCount))
#PO4
temp <- aggregate(colabTotals_S$PO4, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$PO4_StdDev <- temp[c(2)] 
colabTotals_L$PO4_StdErr=unlist(colabStd$PO4_StdDev / sqrt(colabStd$SampleCount))
#Depth
temp <- aggregate(colabTotals_S$Depth, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$Depth_StdDev <- temp[c(2)] 
colabTotals_L$Depth_StdErr=unlist(colabStd$Depth_StdDev / sqrt(colabStd$SampleCount))
#CO2
temp <- aggregate(colabTotals_S$CO2, by=list(colabTotals_S$Lake), FUN=sd, na.rm=TRUE) 
colabStd$CO2_StdDev <- temp[c(2)] 
colabTotals_L$CO2_StdErr=unlist(colabStd$Depth_StdDev / sqrt(colabStd$SampleCount))


#NONCO
# Standard deviation and standard error for EET RATIO are not possible to do by lake for these nonco samples, since there is only one sample per lake
# But we need a placeholder for StdErr full of NA's for graphing
noncoTotals_L$EET_StdErr <- (NA)
# EETOTU
# We need a placeholder for StdErr full of NA's for graphing again
noncoTotals_L$EETOTU_StdErr <- (NA)
# SO4
temp <- aggregate(noncoContext$so4...mg.L., by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$SO4_StdDev <- temp[c(2)] 
noncoTotals_L$SO4_StdErr=unlist(noncoStd$SO4_StdDev / sqrt(noncoDataTally[["so4...mg.L."]])) 
# FeTot
temp <- aggregate(noncoContext$FeTot..uM., by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$FeTot_StdDev <- temp[c(2)] 
noncoTotals_L$FeTot_StdErr=unlist(noncoStd$FeTot_StdDev / sqrt(noncoDataTally[["FeTot..uM."]])) 
# DOC
temp <- aggregate(noncoContext$DOC..mg.L.1., by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$DOC_StdDev <- temp[c(2)] 
noncoTotals_L$DOC_StdErr=unlist(noncoStd$DOC_StdDev / sqrt(noncoDataTally[["DOC..mg.L.1."]]))
# ODO
temp <- aggregate(noncoContext$ODO..mg.L., by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$ODO_StdDev <- temp[c(2)] 
noncoTotals_L$ODO_StdErr=unlist(noncoStd$ODO_StdDev / sqrt(noncoDataTally[["ODO..mg.L."]]))
#pH
temp <- aggregate(noncoContext$pH, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$pH_StdDev <- temp[c(2)] 
noncoTotals_L$pH_StdErr=unlist(noncoStd$pH_StdDev / sqrt(noncoDataTally[["pH"]]))
#CH4
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$CH4_StdDev <- temp[c(2)] 
noncoTotals_L$CH4_StdErr=unlist(noncoStd$CH4_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#DOCorTOC
temp <- aggregate(noncoContext$DOC, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$DOCorTOC_StdDev <- temp[c(2)] 
noncoTotals_L$DOCorTOC_StdErr=unlist(noncoStd$DOCorTOC_StdDev / sqrt(noncoDataTally[["DOC..mg.L.1."]]))

#Temperature
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$Temperature_StdDev <- temp[c(2)] 
noncoTotals_L$Temperature_StdErr=unlist(noncoStd$Temperature_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#NH4
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$NH4_StdDev <- temp[c(2)] 
noncoTotals_L$NH4_StdErr=unlist(noncoStd$NH4_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#NO3
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$NO3_StdDev <- temp[c(2)] 
noncoTotals_L$NO3_StdErr=unlist(noncoStd$NO3_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#NO2
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$NO2_StdDev <- temp[c(2)] 
noncoTotals_L$NO2_StdErr=unlist(noncoStd$NO2_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#NOX
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$NOX_StdDev <- temp[c(2)] 
noncoTotals_L$NOX_StdErr=unlist(noncoStd$NOX_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#PO4
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$PO4_StdDev <- temp[c(2)] 
noncoTotals_L$PO4_StdErr=unlist(noncoStd$PO4_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#Depth
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$Depth_StdDev <- temp[c(2)] 
noncoTotals_L$Depth_StdErr=unlist(noncoStd$Depth_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))
#CO2
temp <- aggregate(noncoContext$CH4, by=list(noncoContext$Sample), FUN=sd, na.rm=TRUE) 
noncoStd$CO2_StdDev <- temp[c(2)] 
noncoTotals_L$CO2_StdErr=unlist(noncoStd$CO2_StdDev / sqrt(noncoDataTally[["CH4..uM."]]))

#################### Merging + R values ####

noncoTotals_L$Label <- ("blue") #Label the data so that we can differentiate label color in the graph later
colabTotals_L$Label <- ("red")
Totals_L <- rbind(noncoTotals_L,colabTotals_L)

#################### All by SAMPLE ####

noncoTotals_S$Label <- ("blue") #Label the data so that we can differentiate label color in the graph later
colabTotals_S$Label <- ("red")
Totals_S <- rbind(noncoTotals_S,colabTotals_S)


Rval <- data.frame(This=character(), withThis=character(), By=character(), R=numeric(), p=numeric(), stringsAsFactors=FALSE) # Blank DF
# Find R (correlation) values between EETRatio + Lake
Rval[nrow(Rval) + 1,] = c("EETRatio","DOC","Lake",cor(Totals_L$EETRatio, Totals_L$DOC, use = "complete.obs"),cor.test(Totals_L$EETRatio, Totals_L$DOC, use = "complete.obs")$p.value) 
Rval[nrow(Rval) + 1,] = c("EETRatio","SO4","Lake",cor(Totals_L$EETRatio, Totals_L$SO4, use = "complete.obs"),cor.test(Totals_L$EETRatio, Totals_L$SO4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","FeTot","Lake",cor(Totals_L$EETRatio, Totals_L$FeTot, use = "complete.obs"),cor.test(Totals_L$EETRatio, Totals_L$FeTot, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","ODO","Lake",cor(Totals_L$EETRatio, Totals_L$ODO, use = "complete.obs"),cor.test(Totals_L$EETRatio, Totals_L$ODO, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","pH","Lake",cor(Totals_L$EETRatio, Totals_L$pH, use = "complete.obs"),cor.test(Totals_L$EETRatio, Totals_L$pH, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","CH4","Lake",cor(Totals_L$EETRatio, Totals_L$CH4, use = "complete.obs"),cor.test(Totals_L$EETRatio, Totals_L$CH4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","DOCorTOC","Lake",cor(Totals_L$EETRatio, Totals_L$DOCorTOC, use = "complete.obs"),cor.test(Totals_L$EETRatio, Totals_L$DOCorTOC, use = "complete.obs")$p.value)
# Find R (correlation) values between EETOTU + Lake
Rval[nrow(Rval) + 1,] = c("EETOTU","DOC","Lake",cor(Totals_L$EETOTU, Totals_L$DOC, use = "complete.obs"),cor.test(Totals_L$EETOTU, Totals_L$DOC, use = "complete.obs")$p.value) 
Rval[nrow(Rval) + 1,] = c("EETOTU","SO4","Lake",cor(Totals_L$EETOTU, Totals_L$SO4, use = "complete.obs"),cor.test(Totals_L$EETOTU, Totals_L$SO4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","FeTot","Lake",cor(Totals_L$EETOTU, Totals_L$FeTot, use = "complete.obs"),cor.test(Totals_L$EETOTU, Totals_L$FeTot, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","ODO","Lake",cor(Totals_L$EETOTU, Totals_L$ODO, use = "complete.obs"),cor.test(Totals_L$EETOTU, Totals_L$ODO, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","pH","Lake",cor(Totals_L$EETOTU, Totals_L$pH, use = "complete.obs"),cor.test(Totals_L$EETOTU, Totals_L$pH, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","CH4","Lake",cor(Totals_L$EETOTU, Totals_L$CH4, use = "complete.obs"),cor.test(Totals_L$EETOTU, Totals_L$CH4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","DOCorTOC","Lake",cor(Totals_L$EETOTU, Totals_L$DOCorTOC, use = "complete.obs"),cor.test(Totals_L$EETOTU, Totals_L$DOCorTOC, use = "complete.obs")$p.value)
# Find R (correlation) values between EETRatio + Sample
Rval[nrow(Rval) + 1,] = c("EETRatio","DOC","Sample",cor(Totals_S$EETRatio, Totals_S$DOC, use = "complete.obs"),cor.test(Totals_S$EETRatio, Totals_S$DOC, use = "complete.obs")$p.value) 
Rval[nrow(Rval) + 1,] = c("EETRatio","SO4","Sample",cor(Totals_S$EETRatio, Totals_S$SO4, use = "complete.obs"),cor.test(Totals_S$EETRatio, Totals_S$SO4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","FeTot","Sample",cor(Totals_S$EETRatio, Totals_S$FeTot, use = "complete.obs"),cor.test(Totals_S$EETRatio, Totals_S$FeTot, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","ODO","Sample",cor(Totals_S$EETRatio, Totals_S$ODO, use = "complete.obs"),cor.test(Totals_S$EETRatio, Totals_S$ODO, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","pH","Sample",cor(Totals_S$EETRatio, Totals_S$pH, use = "complete.obs"),cor.test(Totals_S$EETRatio, Totals_S$pH, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","CH4","Sample",cor(Totals_S$EETRatio, Totals_S$CH4, use = "complete.obs"),cor.test(Totals_S$EETRatio, Totals_S$CH4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETRatio","DOCorTOC","Sample",cor(Totals_S$EETRatio, Totals_S$DOCorTOC, use = "complete.obs"),cor.test(Totals_S$EETRatio, Totals_S$DOCorTOC, use = "complete.obs")$p.value)
# Find R (correlation) values between EETRatio + Sample
Rval[nrow(Rval) + 1,] = c("EETOTU","DOC","Sample",cor(Totals_S$EETOTU, Totals_S$DOC, use = "complete.obs"),cor.test(Totals_S$EETOTU, Totals_S$DOC, use = "complete.obs")$p.value) 
Rval[nrow(Rval) + 1,] = c("EETOTU","SO4","Sample",cor(Totals_S$EETOTU, Totals_S$SO4, use = "complete.obs"),cor.test(Totals_S$EETOTU, Totals_S$SO4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","FeTot","Sample",cor(Totals_S$EETOTU, Totals_S$FeTot, use = "complete.obs"),cor.test(Totals_S$EETOTU, Totals_S$FeTot, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","ODO","Sample",cor(Totals_S$EETOTU, Totals_S$ODO, use = "complete.obs"),cor.test(Totals_S$EETOTU, Totals_S$ODO, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","pH","Sample",cor(Totals_S$EETOTU, Totals_S$pH, use = "complete.obs"),cor.test(Totals_S$EETOTU, Totals_S$pH, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","CH4","Sample",cor(Totals_S$EETOTU, Totals_S$CH4, use = "complete.obs"),cor.test(Totals_S$EETOTU, Totals_S$CH4, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("EETOTU","DOCorTOC","Sample",cor(Totals_S$EETOTU, Totals_S$DOCorTOC, use = "complete.obs"),cor.test(Totals_S$EETOTU, Totals_S$DOCorTOC, use = "complete.obs")$p.value)
#Adding correlations between variables themselves Default is pearson corelation method
Rval[nrow(Rval) + 1,] = c("FeTot","DOC","Sample",cor(Totals_S$FeTot, Totals_S$DOC, use = "complete.obs"),cor.test(Totals_S$FeTot, Totals_S$DOC, use = "complete.obs")$p.value)
Rval[nrow(Rval) + 1,] = c("FeTot","CH4","Sample",cor(Totals_S$FeTot, Totals_S$CH4, use = "complete.obs"),cor.test(Totals_S$FeTot, Totals_S$CH4, use = "complete.obs")$p.value)


Rval$LessThan0.05 = Rval$p
Rval$LessThan0.05 <- ifelse(Rval$LessThan0.05 < 0.05, 1, 0) 


#################### Graphing EET RATIO by LAKE ####

# SO4
ggplot(Totals_L , aes(x=SO4, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin = EETRatio-EET_StdErr, ymax = EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin = SO4-SO4_StdErr, xmax = SO4+SO4_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Sulfate vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="SO4, Sulfate (mg/L)", 
       caption="")

# FeTot
ggplot(Totals_L , aes(x=FeTot, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETRatio-EET_StdErr, ymax=EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=FeTot-FeTot_StdErr, xmax=FeTot+FeTot_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Total Iron vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="Total Iron (uM)", 
       caption="")

# DOC
ggplot(Totals_L , aes(x=DOC, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETRatio-EET_StdErr, ymax=EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=DOC-DOC_StdErr, xmax=DOC+DOC_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="DOC vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="DOC (mg/L)", 
       caption="")

# ODO
ggplot(Totals_L , aes(x=ODO, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETRatio-EET_StdErr, ymax=EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=ODO-ODO_StdErr, xmax=ODO+ODO_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Dissolved Oxygen vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="ODO (mg/L)", 
       caption="")

# PH
ggplot(Totals_L , aes(x=pH, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETRatio-EET_StdErr, ymax=EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=pH-pH_StdErr, xmax=pH+pH_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="pH vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="pH", 
       caption="")

# CH4
ggplot(Totals_L , aes(x=CH4, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETRatio-EET_StdErr, ymax=EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=CH4-CH4_StdErr, xmax=CH4+CH4_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="CH4 vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="Methane (uM)", 
       caption="")

# TOC
ggplot(Totals_L , aes(x=TOC, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETRatio-EET_StdErr, ymax=EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=TOC, xmax=TOC), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="TOC vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="TOC (mg/L)", 
       caption="")


# DOCorTOC
ggplot(Totals_L , aes(x=DOCorTOC, y=EETRatio)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETRatio-EET_StdErr, ymax=EETRatio+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=DOCorTOC-DOCorTOC_StdErr, xmax=DOCorTOC+DOCorTOC_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="DOCorTOC vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="Average of DOC and TOC (mg/L)", 
       caption="")

#################### Graphing EET/OTU by LAKE ####

# SO4
ggplot(Totals_L , aes(x=SO4, y=EETOTU)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETOTU-EET_StdErr, ymax=EETOTU+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=SO4-SO4_StdErr, xmax=SO4+SO4_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Sulfate vs EET OTU", 
       subtitle="EET OTU: EET genes found / Bacteria OTU #", 
       y="EET OTU", 
       x="SO4, Sulfate (mg/L)", 
       caption="")

# FeTot
ggplot(Totals_L , aes(x=FeTot, y=EETOTU)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETOTU-EET_StdErr, ymax=EETOTU+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=FeTot-FeTot_StdErr, xmax=FeTot+FeTot_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Total Iron vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="Total Iron (uM)", 
       caption="")

# DOC
ggplot(Totals_L , aes(x=DOC, y=EETOTU)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETOTU-EET_StdErr, ymax=EETOTU+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=DOC-DOC_StdErr, xmax=DOC+DOC_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  ylim(0,7) +
  labs(title="DOC vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="DOC (mg/L)", 
       caption="")

# ODO
ggplot(Totals_L , aes(x=ODO, y=EETOTU)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETOTU-EET_StdErr, ymax=EETOTU+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=ODO-ODO_StdErr, xmax=ODO+ODO_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Dissolved Oxygen vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="ODO (mg/L)", 
       caption="")

# PH
ggplot(Totals_L , aes(x=pH, y=EETOTU)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETOTU-EET_StdErr, ymax=EETOTU+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=pH-pH_StdErr, xmax=pH+pH_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="pH vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="pH", 
       caption="")

# CH4
ggplot(Totals_L , aes(x=CH4, y=EETOTU)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETOTU-EET_StdErr, ymax=EETOTU+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=CH4-CH4_StdErr, xmax=CH4+CH4_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="CH4 vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="Methane (uM)", 
       caption="")

# DOCorTOC
ggplot(Totals_L , aes(x=DOCorTOC, y=EETOTU)) + 
  geom_point(colour = Totals_L$Label) +
  geom_errorbar(aes(ymin=EETOTU-EET_StdErr, ymax=EETOTU+EET_StdErr), width=0, position=position_dodge(.9)) +
  geom_errorbar(aes(xmin=DOCorTOC-DOCorTOC_StdErr, xmax=DOCorTOC+DOCorTOC_StdErr), width=0, position=position_dodge(.9)) +
  geom_smooth(method="lm") + ylim(0,8) + #removesoutlier
  theme_bw() +
  labs(title="DOCorTOC vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="Average of DOC and TOC (mg/L)", 
       caption="")

#################### Graphing EET RATIO by SAMPLE ####

# SO4
ggplot(Totals_S , aes(x=SO4, y=EETRatio)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Sulfate vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="SO4, Sulfate (mg/L)", 
       caption="")

# FeTot
ggplot(Totals_S , aes(x=FeTot, y=EETRatio)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Total Iron vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="Total Iron (uM)", 
       caption="")

# DOC
ggplot(Totals_S , aes(x=DOC, y=EETRatio)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="DOC vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="DOC (mg/L)", 
       caption="")

# ODO
ggplot(Totals_S , aes(x=ODO, y=EETRatio)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="Dissolved Oxygen vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="ODO (mg/L)", 
       caption="")

# PH
ggplot(Totals_S , aes(x=pH, y=EETRatio)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="pH vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="pH", 
       caption="")

# CH4
ggplot(Totals_S , aes(x=CH4, y=EETRatio)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="CH4 vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="Methane (uM)", 
       caption="")

# DOCorTOC
ggplot(Totals_S , aes(x=DOCorTOC, y=EETRatio)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  labs(title="DOCorTOC vs EET Ratio", 
       subtitle="EET Ratio: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET Ratio", 
       x="Average of DOC and TOC (mg/L)", 
       caption="")

#################### Graphing EET/OTU by SAMPLE ####

# SO4
ggplot(Totals_S , aes(x=SO4, y=EETOTU)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  ylim(0,5) + # Removes outlier
  theme_bw() +
  labs(title="Sulfate vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="SO4, Sulfate (mg/L)", 
       caption="")

# FeTot
ggplot(Totals_S , aes(x=FeTot, y=EETOTU)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  ylim(0,11) + # Removes outlier
  labs(title="Total Iron vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="Total Iron (uM)", 
       caption="")

# DOC
ggplot(Totals_S , aes(x=DOC, y=EETOTU)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  ylim(0, 10) + # Removes outlier
  labs(title="DOC vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="DOC (mg/L)", 
       caption="")

# ODO
ggplot(Totals_S , aes(x=ODO, y=EETOTU)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  ylim(0,11) +
  labs(title="Dissolved Oxygen vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="ODO (mg/L)", 
       caption="")

# PH
ggplot(Totals_S , aes(x=pH, y=EETOTU)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  ylim(0,11) +
  labs(title="pH vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="pH", 
       caption="")

# CH4
ggplot(Totals_S , aes(x=CH4, y=EETOTU)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  ylim(0,12) +
  labs(title="CH4 vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="Methane (uM)", 
       caption="")

# DOCorTOC
ggplot(Totals_S , aes(x=DOCorTOC, y=EETOTU)) + 
  geom_point(colour = Totals_S$Label) +
  geom_smooth(method="lm") +
  theme_bw() +
  ylim(0,12) +
  labs(title="DOCorTOC vs EET OTU", 
       subtitle="EET OTU: # of bacteria found to perform EET / Total # of bacteria in sample", 
       y="EET OTU", 
       x="Average of DOC and TOC (mg/L)", 
       caption="")

write.csv(Totals_S, "/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/Outputs/Totals_S.csv", row.names = FALSE)


#open up Rdata from 2021 and then play the following:
# DOC
ggplot(LakeMETEetRatios_CarbonQuality , aes(x=DOC_Ave_peura, y=OxidEETRatio)) + 
  geom_point() +
  geom_smooth(method="lm", se=F) +
  theme_bw() +
  labs(title="", 
       subtitle="", 
       y="oxiEETRatio", 
       x="DOC (mg/L)", 
       caption="")
#export 8x5

# DOC
ggplot(LakeMETEetRatios_CarbonQuality , aes(x=DOC_Ave_peura, y=RespEETRatio)) + 
  geom_point() +
  geom_smooth(method="lm", se=F) +
  theme_bw() +
  labs(title="", 
       subtitle="", 
       y="redEETRatio", 
       x="DOC (mg/L)", 
       caption="")


