#Step 1: run ManualFrequencyDecomp2020.R
#Also, be sure to choose how to break up the timeseries
#Then run this.
library(patchwork)

#x = Sys.time()
#library(lubridate)
#x + hours(3)

#or

#x <- Sys.time()
#x
#[1] "2012-08-12 13:33:13 BST"
#x + 3*60*60 # add 3 hours
#[1] "2012-08-12 16:33:13 BST"

#best positive shift means when correlate#2's time is given additional X hours, it is most positively correlated to correlate1 at X shifted hours.

#AmpLuxDOTemp60 <- mutate(AmpLuxDOTemp, date_time_lubridate = date_time_lubridate + hours(1))
AmpLuxDOTempSequence <- mutate(AmpLuxDOTemp, date_time_lubridate = (1:nrow(AmpLuxDOTemp))*20)
colnames(AmpLuxDOTempSequence)
#colnames(df)[col_indx] <- “new_col_name_at_col_indx”
colnames(AmpLuxDOTempSequence)[1] <- "minutes"

AmpLuxDOTemp[c(1),c(1)]
AmpLuxDOTemp[c(1252),c(1)] - AmpLuxDOTemp[c(1),c(1)] # =17.6625 days wheras in AmpLuxDOTempSequence the end is 17.3889 days so by the end its about 6 hours off by the end.

#AmpLuxDOTempSequence20 <- mutate(AmpLuxDOTempSequence, minutes = (1252*20,(2:1251)*20))
#AmpLuxDOTempSequenceP20 <- AmpLuxDOTempSequence
#AmpLuxDOTempSequenceP20$minutes2 <- AmpLuxDOTempSequenceP20[c(1252,1:1251),1]
#AmpLuxDOTempSequenceP20 <- AmpLuxDOTempSequenceP20[,c(26,2:25)]
#colnames(AmpLuxDOTempSequenceP20)[1] <- "minutes"

#AmpLuxDOTempSequenceM20 <- AmpLuxDOTempSequence
#AmpLuxDOTempSequenceM20$minutes2 <- AmpLuxDOTempSequenceM20[c(2:1252,1),1]
#AmpLuxDOTempSequenceM20 <- AmpLuxDOTempSequenceM20[,c(26,2:25)]
#colnames(AmpLuxDOTempSequenceM20)[1] <- "minutes"

#AmpLuxDOTempSequenceP40 <- AmpLuxDOTempSequence
#AmpLuxDOTempSequenceP40$minutes2 <- AmpLuxDOTempSequenceP40[c(1251:1252,1:1250),1]
#AmpLuxDOTempSequenceP40 <- AmpLuxDOTempSequenceP40[,c(26,2:25)]
#colnames(AmpLuxDOTempSequenceP40)[1] <- "minutes"

#AmpLuxDOTempSequenceM40 <- AmpLuxDOTempSequence
#AmpLuxDOTempSequenceM40$minutes2 <- AmpLuxDOTempSequenceM40[c(3:1252,1:2),1]
#AmpLuxDOTempSequenceM40 <- AmpLuxDOTempSequenceM40[,c(26,2:25)]
#colnames(AmpLuxDOTempSequenceM40)[1] <- "minutes"

#AmpLuxDOTempSequenceP60 <- AmpLuxDOTempSequence
#AmpLuxDOTempSequenceP60$minutes2 <- AmpLuxDOTempSequenceP60[c(1250:1252,1:1249),1]
#AmpLuxDOTempSequenceP60 <- AmpLuxDOTempSequenceP60[,c(26,2:25)]
#colnames(AmpLuxDOTempSequenceP60)[1] <- "minutes"
#not going to be fast enough

#attempting from: https://technofob.com/2016/02/14/how-to-find-the-lag-that-results-in-maximum-cross-correlation-r/
x <- seq(0,2*pi,pi/100)
length(x)
# [1] 201

y1 <- sin(x)
plot(x,y1,type="l", col = "green")

y2 <- sin(x+pi/2)
lines(x,y2,type="l",col="red")

cv <- ccf(x = y1, y = y2, lag.max = 100, type = c("correlation"),plot = TRUE)

cor = cv$acf[,,1]
lag = cv$lag[,,1]
res = data.frame(cor,lag)
res_max = res[which.max(res$cor),]$lag
res_max
# [1] 44

#me trying: there are 24 60min in a day and  72 20minutes in a day

y1 <- FirstThird$ch1
y2 <- FirstThird$Intensity_lux
cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = TRUE)

y1 <- MiddleThird$ch2
y2 <- MiddleThird$Intensity_lux
cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = TRUE)

y1 <- LastThird$ch2
y2 <- LastThird$Intensity_lux
cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = TRUE)

y1 <- AmpLuxDOTempSequence$ch2
y2 <- AmpLuxDOTempSequence$Intensity_lux
cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = TRUE)

cor = cv$acf[,,1]
lag = cv$lag[,,1]
res = data.frame(cor,lag)
res_max = res[which.max(res$cor),]$lag
res_min = res[which.min(res$cor),]$lag
res_max #Ch1 = 16 ... 16*20/60 = 5.333 hours
res_min #Ch1 = -10 ... -10*20/60 = -3.333 hours
res %>% filter(lag == res_max)
res %>% filter(lag == res_min)
#ccf() uses pearson correlation by default
?ccf()
?which()


#now doing it a bunch of times:

CorLags <- data.frame(Correlate1=character(), Correlate2=character(), Interval=character(), BestPositiveCorrelation=numeric(), BestPositiveLag=numeric(), BestNegativeCorrelation=numeric(), BestNegativeLag=numeric(), stringsAsFactors=FALSE) # Blank DF

FirstDay <- AmpLuxDOTemp[c(1:83),]
channellist <- c("ch1","ch2","ch3","ch4","ch5","ch6","ch7","ch8","ch9","ch10","ch11","ch12","ch13","ch14","ch15","ch16")
correllist <- c("Intensity_lux","Temp_Surface","SunSimulated","Shadow","DO_2m","Temp_2m","DO_20cm","Temp_20cm")
#channellist <- c("Intensity_lux","Temp_Surface","SunSimulated","Shadow","DO_2m","Temp_2m","DO_20cm","Temp_20cm") #for envVSenv

for (correlate in correllist){
  for (channel in channellist){
    y1 <- AmpLuxDOTempSequence[[as.character(channel)]]
    y2 <- AmpLuxDOTempSequence[[as.character(correlate)]]
    cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = FALSE)
    cor = cv$acf[,,1]
    lag = cv$lag[,,1]
    res = data.frame(cor,lag)
    res_max = res[which.max(res$cor),]$lag
    res_min = res[which.min(res$cor),]$lag
    CorLags[nrow(CorLags) + 1,] = c(as.character(channel),as.character(correlate),"total",res[1] %>% filter(lag == res_max),res[2] %>% filter(lag == res_max),res[1] %>% filter(lag == res_min),res[2] %>% filter(lag == res_min))
  }
  for (channel in channellist){
    y1 <- FirstDay[[as.character(channel)]]
    y2 <- FirstDay[[as.character(correlate)]]
    cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = FALSE)
    cor = cv$acf[,,1]
    lag = cv$lag[,,1]
    res = data.frame(cor,lag)
    res_max = res[which.max(res$cor),]$lag
    res_min = res[which.min(res$cor),]$lag
    CorLags[nrow(CorLags) + 1,] = c(as.character(channel),as.character(correlate),"DayOne",res[1] %>% filter(lag == res_max),res[2] %>% filter(lag == res_max),res[1] %>% filter(lag == res_min),res[2] %>% filter(lag == res_min))
  }
  for (channel in channellist){
    y1 <- FirstThird[[as.character(channel)]]
    y2 <- FirstThird[[as.character(correlate)]]
    cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = FALSE)
    cor = cv$acf[,,1]
    lag = cv$lag[,,1]
    res = data.frame(cor,lag)
    res_max = res[which.max(res$cor),]$lag
    res_min = res[which.min(res$cor),]$lag
    CorLags[nrow(CorLags) + 1,] = c(as.character(channel),as.character(correlate),"FirstThird",res[1] %>% filter(lag == res_max),res[2] %>% filter(lag == res_max),res[1] %>% filter(lag == res_min),res[2] %>% filter(lag == res_min))
  }
  for (channel in channellist){
    y1 <- MiddleThird[[as.character(channel)]]
    y2 <- MiddleThird[[as.character(correlate)]]
    cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = FALSE)
    cor = cv$acf[,,1]
    lag = cv$lag[,,1]
    res = data.frame(cor,lag)
    res_max = res[which.max(res$cor),]$lag
    res_min = res[which.min(res$cor),]$lag
    CorLags[nrow(CorLags) + 1,] = c(as.character(channel),as.character(correlate),"MiddleThird",res[1] %>% filter(lag == res_max),res[2] %>% filter(lag == res_max),res[1] %>% filter(lag == res_min),res[2] %>% filter(lag == res_min))
  }
  for (channel in channellist){
    y1 <- LastThird[[as.character(channel)]]
    y2 <- LastThird[[as.character(correlate)]]
    cv <- ccf(x = y1, y = y2, lag.max = 72, type = c("correlation"),plot = FALSE)
    cor = cv$acf[,,1]
    lag = cv$lag[,,1]
    res = data.frame(cor,lag)
    res_max = res[which.max(res$cor),]$lag
    res_min = res[which.min(res$cor),]$lag
    CorLags[nrow(CorLags) + 1,] = c(as.character(channel),as.character(correlate),"LastThird",res[1] %>% filter(lag == res_max),res[2] %>% filter(lag == res_max),res[1] %>% filter(lag == res_min),res[2] %>% filter(lag == res_min))
  }
}


#exporting table
#write.csv(CorLags,"/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers/FrequencyDecompositionAndSynchrony/PearsonCorrelations/BestCorLagsDayOneAndThirds.csv")
#each "1" in lag is equal to 20 min
#write.csv(CorLags,"/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers/FrequencyDecompositionAndSynchrony/PearsonCorrelations/EnvironmentalBestCorLagsDayOneAndThirds.csv") #for environmnetal vs environmental


#channel groups: (C = higher A = lower in water (even for 2.25 - 5.25 even though current flow reversed)) G = alginate-covered otherwise free , D = dark cathode
# Chlorobi depth to surface group: Ch1(10cm-2m), Ch2(G10cm=2.5m), Ch8(D10cm-2m), Ch9(10cm-G2m), Ch11(10cm-2.5m), Ch14(10cm-G2.5m)
# Reverse Current flow Chlorobi depth group: Ch6(2.25m-5.25m), Ch7(2.25m-5.25m), Ch10(G2.25m-5.25m)
# Oxygen/Cyano/Purples/unknown depth group: Ch5(70cm-5m), Ch13(G70cm-G5m)
# Deep group: Ch3(10cm-G4m), Ch4(10cm-5.5m), Ch12(10cm-G5.5m), Ch15(10cm-4m)

#generating graphs for each channel where each hour of day is an average of several days (decomposed though)
# I need to group by hours of a day, so 

as.character(AmpLuxDOTemp[c(1),c(1)])
((as.character(AmpLuxDOTemp[c(1),c(1)]) %>% strsplit(" "))[[1]][2] %>% strsplit(":"))[[1]][1]
str_extract(as.character(AmpLuxDOTemp[c(1),c(1)]), "[^ ]+$")
str_extract(str_extract(as.character(AmpLuxDOTemp[c(1),c(1)]), "[^ ]+$"), "^[^\\:]+")

AmpLuxDOTempHours <- mutate(AmpLuxDOTemp, date_time_lubridate = as.numeric(str_extract(str_extract(as.character(date_time_lubridate), "[^ ]+$"), "^[^\\:]+")))
colnames(AmpLuxDOTempHours)[1] <- "DayHour"

#now split into sections you want graphs for, and then summarize and then graph
FirstThird <- AmpLuxDOTempHours[c(1:417),]
MiddleThird <- AmpLuxDOTempHours[c(418:835),]
LastThird <- AmpLuxDOTempHours[c(836:1252),]
FirstDay <- AmpLuxDOTempHours[c(1:83),]
FirstHalf <- AmpLuxDOTempHours[c(84:585),]
LastHalf <- AmpLuxDOTempHours[c(586:1252),]

AmpLuxDOTemp[c(1),c(1)]
AmpLuxDOTemp[c(417),c(1)]
AmpLuxDOTemp[c(835),c(1)]
AmpLuxDOTemp[c(1252),c(1)]

CorLagPvalues <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers/FrequencyDecompositionAndSynchrony/PearsonCorrelations/BestCorLagsDayOneAndThirdsHoursPvalCalc.csv", header=T)
#CorLagPvalues <- read.csv(file="/Users/cnolmsted/Documents/R_scripts/Microgeist_Loggers/FrequencyDecompositionAndSynchrony/PearsonCorrelations/EnvironmentalBestCorLagsEdit.csv", header=T) #environmental

FirstThirdSummed <- FirstThird  %>% group_by(DayHour) %>% summarise_all(funs(mean,sd))
MiddleThirdSummed <- MiddleThird  %>% group_by(DayHour) %>% summarise_all(funs(mean,sd))
LastThirdSummed <- LastThird  %>% group_by(DayHour) %>% summarise_all(funs(mean,sd))
FirstDaySummed <- FirstDay  %>% group_by(DayHour) %>% summarise_all(funs(mean,sd))
FirstHalfSummed <- FirstHalf  %>% group_by(DayHour) %>% summarise_all(funs(mean,sd))
LastHalfSummed <- LastHalf  %>% group_by(DayHour) %>% summarise_all(funs(mean,sd))

channel <- "ch1"
channel <- "ch11"
channel <- "Intensity_lux"

#get(paste(as.character(channel),"_mean",sep=""))
#get(paste(as.character(channel),"_sd",sep=""))



ggplot(FirstDaySummed, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")))) + geom_point(size=4)+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  xlab("Hour of day, Year: 2020")+
  ylab(expression(paste("Electrical current, anode to cathode (", mu, "A)")))
ggplot(FirstThirdSummed, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")))) + geom_point(size=4)+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  xlab("Hour of day, Year: 2020")+
  ylab(expression(paste("Electrical current, anode to cathode (", mu, "A)")))
ggplot(MiddleThirdSummed, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")))) + geom_point(size=4)+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  xlab("Hour of day, Year: 2020")+
  ylab(expression(paste("Electrical current, anode to cathode (", mu, "A)")))
ggplot(LastThirdSummed, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")))) + geom_point(size=4)+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  xlab("Hour of day, Year: 2020")+
  ylab(expression(paste("Electrical current, anode to cathode (", mu, "A)")))



#Combining first day first third middle third and last third all into one graph:
FirstDaySummed <- mutate(FirstDaySummed, Interval = "DayOne")
FirstThirdSummed <- mutate(FirstThirdSummed, Interval = "FirstThird")
MiddleThirdSummed <- mutate(MiddleThirdSummed, Interval = "MiddleThird")
LastThirdSummed <- mutate(LastThirdSummed, Interval = "LastThird")

SummedSeriesIntervals <- rbind(FirstDaySummed,FirstThirdSummed,MiddleThirdSummed,LastThirdSummed)
SummedSeriesIntervals$Interval <- factor(SummedSeriesIntervals$Interval, levels = c("DayOne","FirstThird","MiddleThird","LastThird"))

channel <- "ch11"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab(expression(paste("Electrical current, anode to cathode (", mu, "A)")))

#then do for all channels
#Also for the other types of data:
#Intensity_lux:
channel <- "Intensity_lux"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab("Surface light intensity (lux)")
channel <- "Shadow"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab("Surface light intensity (lux)")


channel <- "Temp_Surface"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab("Temperature (°C)")
channel <- "Temp_2m"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab("Temperature (°C)")
channel <- "Temp_20cm"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab("Temperature (°C)")

channel <- "DO_20cm"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab("Dissolved Oxygen (mg/L)")
channel <- "DO_2m"
ggplot(SummedSeriesIntervals, aes(x = DayHour, y = get(paste(as.character(channel),"_mean",sep="")), color = Interval)) + geom_point(size=4)+ geom_line()+
  geom_errorbar(aes(ymin=get(paste(as.character(channel),"_mean",sep=""))-get(paste(as.character(channel),"_sd",sep="")), ymax=get(paste(as.character(channel),"_mean",sep=""))+get(paste(as.character(channel),"_sd",sep="")))) +
  theme(legend.title=element_blank()) + scale_color_discrete(labels=c('8-16', '8-16 to 8-22', '8-23 to 8-28','8-29 to 9-03')) +
  xlab("Hour of day, Year: 2020") + ylab("Dissolved Oxygen (mg/L)")



#remake heatmap
Pvals <- data.frame(Correlate1=character(), Correlate2=character(), Interval=character(),N=numeric(), r=numeric(), p=numeric(), stringsAsFactors=FALSE) # Blank DF

for (correlate in correllist){
  for (channel in channellist){
    Pvals[nrow(Pvals) + 1,] = c(as.character(channel),as.character(correlate),"total",sum(complete.cases(AmpLuxDOTemp[[as.character(channel)]], AmpLuxDOTemp[[as.character(correlate)]]), na.rm = TRUE), cor(AmpLuxDOTemp[[as.character(channel)]], AmpLuxDOTemp[[as.character(correlate)]], use = "complete.obs"), cor.test(AmpLuxDOTemp[[as.character(channel)]], AmpLuxDOTemp[[as.character(correlate)]], use = "complete.obs")$p.value)
    Pvals[nrow(Pvals) + 1,] = c(as.character(channel),as.character(correlate),"DayOne",sum(complete.cases(FirstDay[[as.character(channel)]], FirstDay[[as.character(correlate)]]), na.rm = TRUE), cor(FirstDay[[as.character(channel)]], FirstDay[[as.character(correlate)]], use = "complete.obs"), cor.test(FirstDay[[as.character(channel)]], FirstDay[[as.character(correlate)]], use = "complete.obs")$p.value)
    Pvals[nrow(Pvals) + 1,] = c(as.character(channel),as.character(correlate),"FirstThird",sum(complete.cases(FirstThird[[as.character(channel)]], FirstThird[[as.character(correlate)]]), na.rm = TRUE), cor(FirstThird[[as.character(channel)]], FirstThird[[as.character(correlate)]], use = "complete.obs"), cor.test(FirstThird[[as.character(channel)]], FirstThird[[as.character(correlate)]], use = "complete.obs")$p.value)
    Pvals[nrow(Pvals) + 1,] = c(as.character(channel),as.character(correlate),"MiddleThird",sum(complete.cases(MiddleThird[[as.character(channel)]], MiddleThird[[as.character(correlate)]]), na.rm = TRUE), cor(MiddleThird[[as.character(channel)]], MiddleThird[[as.character(correlate)]], use = "complete.obs"), cor.test(MiddleThird[[as.character(channel)]], MiddleThird[[as.character(correlate)]], use = "complete.obs")$p.value)
    Pvals[nrow(Pvals) + 1,] = c(as.character(channel),as.character(correlate),"LastThird",sum(complete.cases(LastThird[[as.character(channel)]], LastThird[[as.character(correlate)]]), na.rm = TRUE), cor(LastThird[[as.character(channel)]], LastThird[[as.character(correlate)]], use = "complete.obs"), cor.test(LastThird[[as.character(channel)]], LastThird[[as.character(correlate)]], use = "complete.obs")$p.value)
  }
}

PvalAll <- merge(Pvals,CorLagPvalues, by=c("Correlate1","Correlate2","Interval","N"), all=TRUE)
#reshaping
colnames(PvalAll)
P1 <- PvalAll[,c(1:6)]
P1$Lag <- "0"
P2 <- PvalAll[,c(1:4,7,13,8)]
P3 <- PvalAll[,c(1:4,9,14,10)]
colnames(P1)
colnames(P2)[1:7] <- c("Correlate1","Correlate2","Interval","N","r","p","Lag" )
colnames(P3)[1:7] <- c("Correlate1","Correlate2","Interval","N","r","p","Lag" )
P1$LagType <- "None"
P2$LagType <- "Positive"
P3$LagType <- "Negative"
PvalMelt <- rbind(P1,P2,P3)
str(PvalMelt)
colnames(PvalMelt)
PvalMelt$N = as.numeric(as.character(PvalMelt$N))
PvalMelt$p = as.numeric(as.character(PvalMelt$p))
PvalMelt$r = as.numeric(as.character(PvalMelt$r))
PvalMelt$Lag = as.numeric(as.character(PvalMelt$Lag))
PvalMelt$Interval <- factor(PvalMelt$Interval, levels = c("total","LastThird","MiddleThird","FirstThird","DayOne"))

#Change channel and run below:
#correllist <- c("Intensity_lux","Temp_Surface","SunSimulated","Shadow","DO_2m","Temp_2m","DO_20cm","Temp_20cm")
#channel <- "Temp_20cm"
channel <- "ch4"

PvalSubset <- PvalMelt %>% filter(Correlate1 == as.character(channel) & Interval != "total")
PvalSubsetDO_20cm <- PvalSubset %>% filter(Correlate2 == "DO_20cm")
PvalSubsetDO_2m <- PvalSubset %>% filter(Correlate2 == "DO_2m")
PvalSubsetIntensity_lux <- PvalSubset %>% filter(Correlate2 == "Intensity_lux")
PvalSubsetTemp_2m <- PvalSubset %>% filter(Correlate2 == "Temp_2m")
PvalSubsetTemp_20cm <- PvalSubset %>% filter(Correlate2 == "Temp_20cm")
PvalSubsetShadow <- PvalSubset %>% filter(Correlate2 == "Shadow")
b <- c(-1, 0, 1)
BH1 <- PvalSubsetDO_20cm$p %>% p.adjust(method = "BH")
BH2 <- PvalSubsetDO_2m$p %>% p.adjust(method = "BH")
BH3 <- PvalSubsetTemp_20cm$p %>% p.adjust(method = "BH")
BH4 <- PvalSubsetTemp_2m$p %>% p.adjust(method = "BH")
BH5 <- PvalSubsetShadow$p %>% p.adjust(method = "BH")
BH6 <- PvalSubsetIntensity_lux$p %>% p.adjust(method = "BH")


asdf1 <- ggplot(PvalSubsetDO_20cm, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH1 <= 0.05,'black','grey50'),size = 2) +
  scale_color_gradient2(name = paste(as.character(channel),"r", sep="   "),limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
  ylab("DO 20cm (mg/L)")+xlab("")
asdf2 <-ggplot(PvalSubsetDO_2m, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH2 <= 0.05,'black','grey50'),size = 2) +
  scale_color_gradient2(name = paste(as.character(channel),"r", sep="   "),limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
  ylab("DO 2m (mg/L)")+xlab("")
asdf3 <-ggplot(PvalSubsetTemp_20cm, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH3 <= 0.05,'black','grey50'),size = 2) +
  scale_color_gradient2(name = paste(as.character(channel),"r", sep="   "),limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
  ylab("Temp 20cm (°C)")+xlab("")
asdf4 <-ggplot(PvalSubsetTemp_2m, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH4 <= 0.05,'black','grey50'),size = 2) +
  scale_color_gradient2(name = paste(as.character(channel),"r", sep="   "),limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
  ylab("Temp 2m (°C)")+xlab("")
#asdf5 <-ggplot(PvalSubsetShadow, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=11)+ geom_point(size=10)+
#  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH5 <= 0.05,'black','grey50'),size = 3) +
#  scale_color_gradient2(name = paste(as.character(channel),"r", sep="   "),limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
#  theme(axis.title.y = element_text(size = 9),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
#  ylab("Shadow (lux)")+xlab("")
asdf5 <-ggplot(PvalSubsetIntensity_lux, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH6 <= 0.05,'black','grey50'),size = 2) +
  scale_color_gradient2(name = paste(as.character(channel),"r", sep="   "), limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(-.5, 0, 0, .3), "cm"))+
  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
  ylab("Intensity (lux)")+xlab("Time Lag (Hours)")



#for Env. vs Env:
#asdf1 <- ggplot(PvalSubsetDO_20cm, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
#  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH1 <= 0.05,'black','grey50'),size = 2) +
#  scale_color_gradient2(limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
#  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
#  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
#  ylab("DO 20cm (mg/L)")+xlab("")
#asdf2 <-ggplot(PvalSubsetDO_2m, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
#  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH2 <= 0.05,'black','grey50'),size = 2) +
#  scale_color_gradient2(limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
#  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
#  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
#  ylab("DO 2m (mg/L)")+xlab("")
#asdf3 <-ggplot(PvalSubsetTemp_20cm, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
#  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH3 <= 0.05,'black','grey50'),size = 2) +
#  scale_color_gradient2(limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
#  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
#  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
#  ylab("Temp 20cm (°C)")+xlab("")
#asdf4 <-ggplot(PvalSubsetTemp_2m, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
#  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH4 <= 0.05,'black','grey50'),size = 2) +
#  scale_color_gradient2(limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
#  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(0, 0, 0, .3), "cm"))+
#  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
#  ylab("Temp 2m (°C)")+xlab("")
#asdf5 <-ggplot(PvalSubsetIntensity_lux, aes(x=Lag, y=Interval, color=r))+ geom_line(color="black")+ geom_point(color="black", size=7)+ geom_point(size=6)+
#  geom_text(aes(label = paste(str_remove(as.character(sprintf("%.3f",p)), "^0+"))), fontface = "bold", color = ifelse(BH6 <= 0.05,'black','grey50'),size = 2) +
#  scale_color_gradient2(limits = c(-1,1),low="red", high="dodgerblue", mid="white", breaks=b, labels=format(b)) + 
#  theme(axis.title.y = element_text(size = 9,margin = margin(t = 0, r = 6, b = 0, l = 0)),axis.text.y = element_text(size=11),plot.margin = unit(c(-.5, 0, 0, .3), "cm"))+
#  scale_y_discrete(labels=c('8-29 to 9-03','8-23 to 8-28','8-16 to 8-22','8-16'))+
#  ylab("Intensity (lux)")+xlab("Time Lag (Hours)")
#then add title in illustrator

#plot_grid(asdf1, asdf2, asdf3 , asdf4, asdf5, asdf6, ncol=1, labels = c('','','','','',''), align = "v")

#combined <- asdf1 + asdf2 + asdf3 + asdf4 + asdf5 & theme(legend.position="top",legend.key.width = unit(1.22, 'cm'),legend.title = element_text(size=24))
#combined + plot_layout(nrow=5,ncol=1,guides = "collect") # + plot_annotation(title = as.character(channel)) 

combined <- asdf1 + plot_spacer() + asdf2 + plot_spacer() + asdf3 + plot_spacer() + asdf4 + plot_spacer() + asdf5 & theme(legend.position="top",legend.key.width = unit(1.5, 'cm'),legend.title = element_text(size=24))
combined + plot_layout(nrow=9,ncol=1,guides = "collect",heights=c(1,-.6,1,-.6,1,-.6,1,-.6,1)) # + plot_annotation(title = as.character(channel)) 

