#Clean data list slate:
rm(list = ls())
library("dplyr") 


#reading in trout bog lv1 csvs
TB_2018_06_21 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-06-21_Trout_Bog_Lv1.csv", header = T)
TB_2018_06_25 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-06-25_Trout_Bog_Lv1.csv", header = T)
TB_2018_07_02 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-07-02_Trout_Bog_Lv1.csv", header = T)
TB_2018_07_09 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-07-09_Trout_Bog_Lv1.csv", header = T)
TB_2018_07_16 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-07-16_Trout_Bog_Lv1.csv", header = T)
TB_2018_07_23 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-07-23_Trout_Bog_Lv1.csv", header = T)
TB_2018_07_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-07-30_Trout_Bog_Lv1.csv", header = T)
TB_2018_08_07 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-08-07_Trout_Bog_Lv1.csv", header = T)
TB_2018_08_13 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-08-13_Trout_Bog_Lv1.csv", header = T)
TB_2018_08_28 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-08-28_Trout_Bog_Lv1.csv", header = T)
TB_2018_09_11 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-09-11_Trout_Bog_Lv1.csv", header = T)
TB_2018_09_27 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-09-27_Trout_Bog_Lv1.csv", header = T)
TB_2018_10_13 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-10-13_Trout_Bog_Lv1.csv", header = T)
TB_2018_11_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-11-30_Trout_Bog_Lv1.csv", header = T)
TB_2018_12_17 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2018-12-17_Trout_Bog_Lv1.csv", header = T)
TB_2019_02_23 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-02-23_Trout_Bog_Lv1.csv", header = T)
TB_2019_05_27 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-05-27_Trout_Bog_Lv1.csv", header = T)
TB_2019_06_03 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-06-03_Trout_Bog_Lv1.csv", header = T)
TB_2019_06_10 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-06-10_Trout_Bog_Lv1.csv", header = T)
TB_2019_06_17 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-06-17_Trout_Bog_Lv1.csv", header = T)
TB_2019_06_24 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-06-24_Trout_Bog_Lv1.csv", header = T)
TB_2019_06_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-06-30_Trout_Bog_Lv1.csv", header = T)
# not trout bog??? TB_2019_07_08 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-07-08_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_15 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-07-15_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_21 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-07-21_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_22 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-07-22_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_23 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-07-23_Trout_Bog_Lv1.csv", header = T)
TB_2019_07_29 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-07-29_Trout_Bog_Lv1.csv", header = T)
TB_2019_08_05 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-08-05_Trout_Bog_Lv1.csv", header = T)
TB_2019_08_13 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-08-13_Trout_Bog_Lv1.csv", header = T)
TB_2019_08_19 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Trout_Bog/2019-08-19_Trout_Bog_Lv1.csv", header = T)
#South Sparkling 
SS_2018_06_28 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-06-28_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_07_03 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-07-03_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_07_14 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-07-14_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_07_18 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-07-18_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_07_31 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-07-31_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_08_08 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-08-08_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_10_13 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-10-13_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_11_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-11-30_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2018_12_17 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2018-12-17_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_05_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-05-30_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_06_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-06-06_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_06_12 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-06-12_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_06_20 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-06-20_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_06_27 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-06-27_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_07_03 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-07-03_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_07_11 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-07-11_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_07_19 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-07-19_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_07_24 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-07-24_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_08_08 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-08-08_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_08_15 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-08-15_South_Sparkling_Bog_Lv1.csv", header = T)
SS_2019_08_20 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/South_Sparkling_Bog/2019-08-20_South_Sparkling_Bog_Lv1.csv", header = T)
#north sparkling
NS_2018_07_03 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2018-07-03_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2018_07_14 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2018-07-14_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2018_07_18 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2018-07-18_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2018_07_31 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2018-07-31_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2018_08_08 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2018-08-08_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_05_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-05-30_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_06_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-06-06_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_06_12 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-06-12_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_06_20 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-06-20_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_06_27 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-06-27_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_07_03 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-07-03_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_07_11 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-07-11_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_07_19 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-07-19_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_07_24 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-07-24_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_08_08 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-08-08_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_08_15 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-08-15_North_Sparkling_Bog_Lv1.csv", header = T)
NS_2019_08_20 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/North_Sparkling_Bog/2019-08-20_North_Sparkling_Bog_Lv1.csv", header = T)
#mary lake
ML_2018_07_03 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2018-07-03_Mary_Lake_Lv1.csv", header = T)
ML_2018_07_12 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2018-07-12_Mary_Lake_Lv1.csv", header = T)
ML_2018_07_19 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2018-07-19_Mary_Lake_Lv1.csv", header = T)
ML_2018_08_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2018-08-06_Mary_Lake_Lv1.csv", header = T)
ML_2018_08_10 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2018-08-10_Mary_Lake_Lv1.csv", header = T)
ML_2019_05_29 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-05-29_Mary_Lake_Lv1.csv", header = T)
ML_2019_06_05 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-06-05_Mary_Lake_Lv1.csv", header = T)
ML_2019_06_13 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-06-13_Mary_Lake_Lv1.csv", header = T)
ML_2019_06_19 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-06-19_Mary_Lake_Lv1.csv", header = T)
ML_2019_06_26 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-06-26_Mary_Lake_Lv1.csv", header = T)
ML_2019_07_02 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-07-02_Mary_Lake_Lv1.csv", header = T)
ML_2019_07_10 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-07-10_Mary_Lake_Lv1.csv", header = T)
ML_2019_07_17 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-07-17_Mary_Lake_Lv1.csv", header = T)
ML_2019_07_23 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-07-23_Mary_Lake_Lv1.csv", header = T)
ML_2019_07_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-07-30_Mary_Lake_Lv1.csv", header = T)
ML_2019_08_07 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-08-07_Mary_Lake_Lv1.csv", header = T)
ML_2019_08_14 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-08-14_Mary_Lake_Lv1.csv", header = T)
ML_2019_08_20 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Mary_Lake/2019-08-20_Mary_Lake_Lv1.csv", header = T)
#Crystal Bog
CB_2019_07_12 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2018-07-12_Crystal_Bog_Lv1.csv", header = T)
CB_2018_07_30 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2018-07-30_Crystal_Bog_Lv1.csv", header = T)
CB_2018_08_07 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2018-08-07_Crystal_Bog_Lv1.csv", header = T)
CB_2018_08_14 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2018-08-14_Crystal_Bog_Lv1.csv", header = T)
CB_2018_09_11 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2018-09-11_Crystal_Bog_Lv1.csv", header = T)
CB_2019_05_27 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-05-27_Crystal_Bog_Lv1.csv", header = T)
CB_2019_06_04 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-06-04_Crystal_Bog_Lv1.csv", header = T)
CB_2019_06_11 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-06-11_Crystal_Bog_Lv1.csv", header = T)
CB_2019_06_18 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-06-18_Crystal_Bog_Lv1.csv", header = T)
CB_2019_06_26 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-06-26_Crystal_Bog_Lv1.csv", header = T)
CB_2019_07_01 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-07-01_Crystal_Bog_Lv1.csv", header = T)
CB_2019_07_09 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-07-09_Crystal_Bog_Lv1.csv", header = T)
CB_2019_07_16 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-07-16_Crystal_Bog_Lv1.csv", header = T)
CB_2019_07_22 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-07-22_Crystal_Bog_Lv1.csv", header = T)
CB_2019_08_06 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-08-06_Crystal_Bog_Lv1.csv", header = T)
CB_2019_08_12 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-08-12_Crystal_Bog_Lv1.csv", header = T)
CB_2019_08_19 <- read.csv(file="/Users/cnolmsted/Documents/MchMahon_Lab/Time_Series/Bogdata_ProDSS/Crystal_Bog/2019-08-19_Crystal_Bog_Lv1.csv", header = T)


#plotting trout bog 2018
TB_all_2018 <- rbind(TB_2018_06_21, TB_2018_06_25, TB_2018_07_02, TB_2018_07_09, TB_2018_07_16, TB_2018_07_23, TB_2018_07_30, TB_2018_08_07, TB_2018_08_13, TB_2018_08_28, TB_2018_09_11, TB_2018_09_27, TB_2018_10_13, TB_2018_11_30, TB_2018_12_17)
#getting rid of turbidities over 6
TB_all_2018 <- TB_all_2018[which(TB_all_2018$Turbidity < 6), ]
TB_all_2018 <- TB_all_2018[which(TB_all_2018$pH < 5.2), ]

#export 5x7.5 pdf
ggplot(data=TB_all_2018, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.4))
ggplot(data=TB_all_2018, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=TB_all_2018, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=TB_all_2018, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=TB_all_2018, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.4))

#plotting only before mix
TB_all_2018 <- rbind(TB_2018_06_21, TB_2018_06_25, TB_2018_07_02, TB_2018_07_09, TB_2018_07_16, TB_2018_07_23, TB_2018_07_30, TB_2018_08_07, TB_2018_08_13, TB_2018_08_28)
#getting rid of turbidities over 6
TB_all_2018 <- TB_all_2018[which(TB_all_2018$Turbidity < 6), ]
TB_all_2018 <- TB_all_2018[which(TB_all_2018$pH < 5.2), ]
#export 5x7.5 pdf
ggplot(data=TB_all_2018, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.4))
ggplot(data=TB_all_2018, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=TB_all_2018, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=TB_all_2018, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=TB_all_2018, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.4))

#plotting trout bog 2019

TB_all_2019 <- rbind(TB_2019_05_27, TB_2019_06_03, TB_2019_06_10, TB_2019_06_17, TB_2019_06_24, TB_2019_06_30, TB_2019_07_15, TB_2019_07_21, TB_2019_07_22, TB_2019_07_23, TB_2019_07_29, TB_2019_08_05, TB_2019_08_13, TB_2019_08_19)
TB_all_2019 <- TB_all_2019[which(TB_all_2019$Turbidity < 5), ]
#export 5x7.5 pdf
ggplot(data=TB_all_2019, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.4))
ggplot(data=TB_all_2019, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=TB_all_2019, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=TB_all_2019, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=TB_all_2019, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Trout Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.75,.4))

#Plotting South Sparkling
SS_all_2018 <- rbind(SS_2018_06_28, SS_2018_07_03, SS_2018_07_14, SS_2018_07_18, SS_2018_07_31, SS_2018_08_08, SS_2018_10_13, SS_2018_11_30, SS_2018_12_17)
#before mix
SS_all_2018 <- rbind(SS_2018_06_28, SS_2018_07_03, SS_2018_07_14, SS_2018_07_18, SS_2018_07_31, SS_2018_08_08)
SS_all_2018 <- SS_all_2018[which(SS_all_2018$Turbidity < 15), ]

SS_all_2019 <- rbind(SS_2019_05_30, SS_2019_06_06, SS_2019_06_12, SS_2019_06_20, SS_2019_06_27, SS_2019_07_03, SS_2019_07_11, SS_2019_07_19, SS_2019_07_24, SS_2019_08_08, SS_2019_08_15, SS_2019_08_20)
SS_all_2019 <- SS_all_2019[which(SS_all_2019$Turbidity < 12), ]
#export 5x7.5 pdf
ggplot(data=SS_all_2018, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.4))
ggplot(data=SS_all_2018, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=SS_all_2018, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=SS_all_2018, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=SS_all_2018, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))

ggplot(data=SS_all_2019, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.4))
ggplot(data=SS_all_2019, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=SS_all_2019, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=SS_all_2019, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=SS_all_2019, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("South Sparkling Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.75,.4))

#Plotting North Sparkling
NS_all_2018 <- rbind(NS_2018_07_03, NS_2018_07_14, NS_2018_07_18, NS_2018_07_31, NS_2018_08_08)
NS_all_2019 <- rbind(NS_2019_05_30, NS_2019_06_06, NS_2019_06_12, NS_2019_06_20, NS_2019_06_27, NS_2019_07_03, NS_2019_07_11, NS_2019_07_19, NS_2019_07_24, NS_2019_08_08, NS_2019_08_15, NS_2019_08_20)
NS_all_2019 <- NS_all_2019[which(NS_all_2019$Turbidity < 15), ]
#export 5x7.5 pdf
ggplot(data=NS_all_2018, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.8))
ggplot(data=NS_all_2018, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=NS_all_2018, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=NS_all_2018, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=NS_all_2018, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))

ggplot(data=NS_all_2019, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.76))
ggplot(data=NS_all_2019, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.3,.7))
ggplot(data=NS_all_2019, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=NS_all_2019, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=NS_all_2019, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("North Sparkling Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.75,.78))

#Plotting Mary Lake
ML_all_2018 <- rbind(ML_2018_07_03, ML_2018_07_12, ML_2018_07_19, ML_2018_08_06, ML_2018_08_10)
ML_all_2019 <- rbind(ML_2019_05_29, ML_2019_06_05, ML_2019_06_13, ML_2019_06_19, ML_2019_06_26, ML_2019_07_02, ML_2019_07_10, ML_2019_07_17, ML_2019_07_23, ML_2019_07_30, ML_2019_08_07, ML_2019_08_14, ML_2019_08_20)
ML_all_2019 <- ML_all_2019[which(ML_all_2019$Turbidity < 8), ]
ML_all_2019 <- ML_all_2019[which(ML_all_2019$Depth < 10), ]
#export 5x7.5 pdf
ggplot(data=ML_all_2018, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.8))
ggplot(data=ML_all_2018, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=ML_all_2018, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=ML_all_2018, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=ML_all_2018, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
#Adjust averages of Turb 10 to 11 deep to be 2.05 fnu
ggplot(data=ML_all_2019, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.6))
ggplot(data=ML_all_2019, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.5,.3))
ggplot(data=ML_all_2019, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.4,.4))
ggplot(data=ML_all_2019, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.4,.4))
ggplot(data=ML_all_2019, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Mary Lake pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))

#Plotting Crystal Bog
CB_all_2018 <- rbind(CB_2019_07_12, CB_2018_08_07, CB_2018_08_14, CB_2018_09_11)
CB_all_2019 <- rbind(CB_2019_05_27, CB_2019_06_04, CB_2019_06_11, CB_2019_06_18, CB_2019_06_26, CB_2019_07_01, CB_2019_07_09, CB_2019_07_16, CB_2019_07_22, CB_2019_08_06, CB_2019_08_12, CB_2019_08_19)
CB_all_2018 <- CB_all_2018[which(CB_all_2018$Turbidity < 10), ]
CB_all_2019 <- CB_all_2019[which(CB_all_2019$Turbidity < 40), ]
#export 5x7.5 pdf
ggplot(data=CB_all_2018, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.8))
ggplot(data=CB_all_2018, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.8,.3))
ggplot(data=CB_all_2018, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=CB_all_2018, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
ggplot(data=CB_all_2018, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.4))
#2019
ggplot(data=CB_all_2019, aes(x=Turbidity,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog Turbidity Profiles") + labs(x="Turbidity(FNU)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.5))
ggplot(data=CB_all_2019, aes(x=ORP,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog ORP Profiles") + labs(x="ORP(mV)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.1,.5))
ggplot(data=CB_all_2019, aes(x=Temp,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog Temperature Profiles") + labs(x="Temperature(F)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.7,.2))
ggplot(data=CB_all_2019, aes(x=ODO.1,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog ODO Profiles") + labs(x="ODO(mg/L)",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.6,.3))
ggplot(data=CB_all_2019, aes(x=pH,y=Depth, group = DATE, color=DATE)) + geom_point() + scale_y_reverse() + geom_path() + ggtitle("Crystal Bog pH Profiles") + labs(x="pH",y='Depth(m)') + scale_color_hue(h = c(60,360) + 15, c = 100, l = 50, h.start = 60, direction = 1, na.value = "grey50",aesthetics = "colour") + theme(legend.position=c(.5,.4))

#Grabbing all the date-times for each of the bogs. 
#want to average the first and last time found in each.
#master list:
Litterally_all <- rbind(TB_2018_06_21, TB_2018_06_25, TB_2018_07_02, TB_2018_07_09, TB_2018_07_16, TB_2018_07_23, TB_2018_07_30, TB_2018_08_07, TB_2018_08_13, TB_2018_08_28, TB_2018_09_11, TB_2018_09_27, TB_2018_10_13, TB_2018_11_30, TB_2018_12_17, TB_2019_05_27, TB_2019_06_03, TB_2019_06_10, TB_2019_06_17, TB_2019_06_24, TB_2019_06_30, TB_2019_07_15, TB_2019_07_21, TB_2019_07_22, TB_2019_07_23, TB_2019_07_29, TB_2019_08_05, TB_2019_08_13, TB_2019_08_19, SS_2018_06_28, SS_2018_07_03, SS_2018_07_14, SS_2018_07_18, SS_2018_07_31, SS_2018_08_08, SS_2018_10_13, SS_2018_11_30, SS_2018_12_17, SS_2019_05_30, SS_2019_06_06, SS_2019_06_12, SS_2019_06_20, SS_2019_06_27, SS_2019_07_03, SS_2019_07_11, SS_2019_07_19, SS_2019_07_24, SS_2019_08_08, SS_2019_08_15, SS_2019_08_20, NS_2018_07_03, NS_2018_07_14, NS_2018_07_18, NS_2018_07_31, NS_2018_08_08, NS_2019_05_30, NS_2019_06_06, NS_2019_06_12, NS_2019_06_20, NS_2019_06_27, NS_2019_07_03, NS_2019_07_11, NS_2019_07_19, NS_2019_07_24, NS_2019_08_08, NS_2019_08_15, NS_2019_08_20, ML_2019_07_03, ML_2018_07_12, ML_2018_07_19, ML_2018_08_06, ML_2018_08_10, ML_2019_05_29, ML_2019_06_05, ML_2019_06_13, ML_2019_06_19, ML_2019_06_26, ML_2019_07_02, ML_2019_07_10, ML_2019_07_17, ML_2019_07_23, ML_2019_07_30, ML_2019_08_07, ML_2019_08_14, ML_2019_08_20, CB_2019_07_12, CB_2018_08_07, CB_2018_08_14, CB_2018_09_11, CB_2019_05_27, CB_2019_06_04, CB_2019_06_11, CB_2019_06_18, CB_2019_06_26, CB_2019_07_01, CB_2019_07_09, CB_2019_07_16, CB_2019_07_22, CB_2019_08_06, CB_2019_08_12, CB_2019_08_19)
First3col <- c(Litterally_all[1:3])
First3colUniq <- Litterally_all %>% group_by(DATE,SITE) %>% summarize(First_time = first(TIME))

write.csv(First3colUniq,"/Users/cnolmsted/Documents/R_scripts/ProDSS_plotting/Date_Times_Bog_all.csv", row.names = FALSE)
