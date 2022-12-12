

#First run these two R scripts except all the graphing parts with ggplot
#/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/GraphingBinsUniqueByLake_cno.R
#/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/R/Outliers_UniqueByLakecno.R
theme_set(theme_cowplot())
#First I want to see which samples are in the epi, hypo, or oxycline, and to do so I'll graph each lake's samples by oxygen level and by depth
#There might be samples from various times and therefore the oxygen content might differ at the same depth; lets see how this goes.

# checking 754378-169136 lake first, don't have much in the oxicline, depth jumps from 1 to 3.9m and  8mg/L to 0.3mg/L
# checking next: Alinen Mustaj_rvi,
Totals_AlinenMustaj <- Totals_S[which(Totals_S$Lake == "Alinen Mustaj_rvi"), ]
ggplot(Totals_AlinenMustaj , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_AlinenMustaj$Label) +
  labs(title="Alinen Mustaj_rvi Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_AlinenMustaj[ c(1:6,20:22) ]
#calculating EET values: EET ratio:(ratio*bincount1 + 2 + 3) / (Bincount1 + 2 + 3)
#calculating EET count: (EET count1 + 2 + 3) / (bincount1 + 2 + 3)
#EET ratio
#epilimnion: 0.571428571 = (14 + 0 + 18) / (22 + 1 + 33)
#Oxycline: 0.536585366 = (14 + 0 + 1 + 23 + 28) / (24 + 3 + 4 + 43 + 49) 
#hypolimnion: 0.658743633 = (17 + ... so on ) / (25 + ... so on  )
#EET count
#epilimnion: 2.107142857 = (53 + 0 + 65) / (22 + 1 + 33) 
#Oxicline: 1.764227642 = (55 + 0 + 2 + 78 + 82) / (24 + 3 + 4 + 43 + 49) 
#hypolimnion: 2.674023769 = (71 + ... so on ) / (25 + ... so on )

# checking next: B1_2, which has no anoxic part, just a less oxic part
Totals_B1_2 <- Totals_S[which(Totals_S$Lake == "B1_2"), ]
ggplot(Totals_B1_2 , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_B1_2$Label) +
  labs(title="Alinen Mustaj_rvi Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_B1_2[ c(1:6,10,20:22) ]
#EET ratio
#epilimnion: 0.5
#oxycline: 0.8
#EET count
#epilimnion: 2.125
#oxycline: 2.66667

# checking next: B4,
Totals_B4 <- Totals_S[which(Totals_S$Lake == "B4"), ]
ggplot(Totals_B4 , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_B4$Label) +
  labs(title="Alinen Mustaj_rvi Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_B4[ c(1:6,10,20:22) ]
# ... nevermind, no oxicline

# checking next: Bengtsgolen (650828-152254), ... nope, only one sample

# checking next: Bj_rntj_rnan (711620-164523), which has nothing between .25 and 6.5 meters but maybe 6.5 is at the end of the oxycline? i doubt it.
Totals_Bj711620 <- Totals_S[which(Totals_S$Lake == "Bj_rntj_rnan (711620-164523)"), ]
ggplot(Totals_Bj711620 , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_Bj711620$Label) +
  labs(title="Bj_rntj_rnan (711620-164523) Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_Bj711620[ c(1:6,10,20:22) ]
#EETratio,count
#epilimnion: 0.4117647	1.705882
#6.5m: 0.5789474, 1.929825
#hypolimnion: 0.631067961,3.048543689

# checking next: Bj_rntj_rnan (711632-164487), which has only 3 samples:
#EETratio,count
#epilimnion: 0.4166667, 1.291667
#oxycline: 0.3902439, 1.024390
#hypolimnion: 0.7631579, 3.842105

# checking next: C2_4, not too deep... but it almost gets anoxic
Totals_C2_4 <- Totals_S[which(Totals_S$Lake == "C2_4"), ]
ggplot(Totals_C2_4 , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_C2_4$Label) +
  labs(title="Bj_rntj_rnan (711620-164523) Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_C2_4[ c(1:6,10,20:22) ]

#epilimnion: 0.375	1.3125
#oxycline: 0.384615385	1.384615385
#hypolimnion: 0.416666667	1.583333333

#Checking C5, nope, all one depth


#Checking Erken, oxycline = 8 to 15m 
Totals_Erken <- Totals_S[which(Totals_S$Lake == "Erken"), ]
ggplot(Totals_Erken , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_Erken$Label) +
  labs(title="Erken Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_Erken[ c(1:6,10,20:22) ]

#epilimnion: 0.260504202, 0.74789916
#oxycline: 0.300492611, 0.655172414
#hypolimnion: 0.361904762, 0.971428571


#Checking Haukij_rvi, no oxycline at all
# I1_2, no oxycline

#Checking Ki1, epi and oxycline are one sample, hypo is 2...
Totals_Ki1 <- Totals_S[which(Totals_S$Lake == "Ki1"), ]
ggplot(Totals_Ki1 , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_Ki1$Label) +
  labs(title="Ki1 Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_Ki1[ c(1:6,10,20:22) ]

#epilimnion: 0.5000000, 1.250000
#oxycline: 0.47222221.944444
#hypolimnion: 0.913043478, 5.304347826

#Checking Lomtj_rnan (703480-142137)
Totals_Lomtj <- Totals_S[which(Totals_S$Lake == "Lomtj_rnan (703480-142137)"), ]
ggplot(Totals_Lomtj , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_Lomtj$Label) +
  labs(title="Lomtj_rnan (703480-142137) Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_Lomtj[ c(1:6,10,20:22) ]

#epilimmiomn: 0.380952381	1.023809524
#bottom: 0.670520231	3.907514451

#checking Mekkoj_rvi
Totals_Mekkoj_rvi <- Totals_S[which(Totals_S$Lake == "Mekkoj_rvi"), ]
ggplot(Totals_Mekkoj_rvi , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_Mekkoj_rvi$Label) +
  labs(title="Mekkoj_rvi Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_Mekkoj_rvi[ c(1:6,10,20:22) ]
#epilimnion: 0.423913043	1.630434783
#oxicline (kinda right after maybe?): 0.765432099	5.358024691
#hypo (maybe): 0.833333333	5.19047619

#checking: N_stj_rnen (711940-164576)
Totals_N_stj_rnen <- Totals_S[which(Totals_S$Lake == "N_stj_rnen (711940-164576)"), ]
ggplot(Totals_N_stj_rnen , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_N_stj_rnen$Label) +
  labs(title="N_stj_rnen (711940-164576) Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_N_stj_rnen[ c(1:6,10,20:22) ]
#epilimnion: 0.2903226	0.7741935
#oxicline (kinda right after maybe?): 0.4090909 1.5000000
#hypo: 0.507042254	3.014084507

#Checking SAS2A
Totals_SAS2A <- Totals_S[which(Totals_S$Lake == "SAS2A"), ]
ggplot(Totals_SAS2A , aes(x=ODO, y=Depth)) + 
  scale_y_reverse() +
  geom_point(colour = Totals_SAS2A$Label) +
  labs(title="SAS2A Dissolved Oxygen vs Depth",
       y="Depth (m)", 
       x="ODO (mg/L)", 
       caption="")
temp <- Totals_SAS2A[ c(1:6,10,20:22) ]
# not deep enough

#Checking SAS2B, also not deep enough
#Checking SAS2C, also not deep enough
#Checking SAS2D, also not deep enough
# so basically there weren't deep enough lakes, and those that were weren't depth discrete enough. In summary the only data to support the hypothesis that the zone between oxic and anoxic regions is in Trout Bog, Mary lake depth metagenome shaomei OMC counts, and I've got south sparkling bog samples to analyze too if desired.