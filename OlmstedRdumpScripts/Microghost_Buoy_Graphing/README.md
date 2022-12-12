This folder contains R scripts which can be run if the approprate files are in above folder.

The base files off of which the script ../scripts/dt_selector.sh will alter:
the beginning date from "2019-08-04" to whatever else, even a date-time
the end date from "2019-08-06" to whatever else, even a date-time
Base-files:
SpecterMicroAmpGraphMinimum_Channels_Free2mC.R #channels without membranes from specter only cathodes above oxycline between dates as above
SpecterMicroAmpGraphMinimum_Channels_all.R #all channels
SpecterMicroAmpGraphMinimum_Channels_Free.R #channels without membranes
SpecterMicroAmpGraphMinimum_Channels_2mC.R #channels with cathodes above oxycline
SpecterMicroAmpGraphMinimum_Channels_15-16.R #channels 15 and 16 only
BansheeMicroAmpGraphMinimum_Channels_Free2mC.R 
BansheeMicroAmpGraphMinimum_Channels_Free.R
BansheeMicroAmpGraphMinimum_Channels_all.R
BansheeMicroAmpGraphMinimum_Channels_15-16.R
BansheeMicroAmpGraphMinimum_Channels_2mC.R
#these scripts will make a combined Light, microamperage, and DO graph between dates as above
#commands which have been run to generate graphs: Possible dates: between 7-20-19 11:00 and 8-13-19 19:54
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-07 Ban2019-08-06_07.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_2mC.R 2019-08-06 2019-08-07 Ban2mC2019-08-06_07.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-07 BanAll2019-08-06_07.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-09 Ban2019-08-06_09.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_15-16.R 2019-08-06 2019-08-09 Ban15-16_2019-08-06_09.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-06 BanAll_2019-08-06_to_06.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_2mC.R 2019-08-06\ 00:00:00 2019-08-06\ 19:00:00 Ban2mC_2019-08-06-00-00-00_19-00-00.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-08 2019-08-09 BanAll_2019-08-08_to_09.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-08\ 00:00:00 2019-08-08\ 23:59:00 BanAll_2019-08-08_to_08.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_15-16.R 2019-08-02\ 00:00:00 2019-08-08\ 23:59:00 Ban15-16_2019-08-08_to_08.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-07-20\ 15:00:00 2019-08-13\ 09:59:00 Banall_2019-07-02_to_08-13.R

# These scripts will use the currennt-corrected untriplicated files

../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-07 BanAll2019-08-06_07_Try2.R

../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-07-20\ 15:00:00 2019-08-13\ 09:59:00 Banall_2019-07-20_to_08-13.R
../scripts/dt_selector.sh SpecterMicroAmpGraphMinimum_Channels_all.R 2019-07-20\ 15:00:00 2019-08-13\ 09:59:00 Speall_2019-07-20_to_08-13.R

#after the corrective electrode separations:
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-07-31\ 19:00:00 2019-08-13\ 09:59:00 Banall_2019-07-31_to_08-13.R
../scripts/dt_selector.sh SpecterMicroAmpGraphMinimum_Channels_all.R 2019-07-31\ 19:00:00 2019-08-13\ 09:59:00 Speall_2019-07-31_to_08-13.R

#only free epi cathodes from aug 2 at 12:00 to aug 6 at 8:30
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_Free2mC.R 2019-08-02\ 12:00:00 2019-08-06\ 08:30:00 BanFree2mC_2019-08-02noon_to_08-06morning.R
../scripts/dt_selector.sh SpecterMicroAmpGraphMinimum_Channels_Free2mC.R 2019-08-02\ 12:00:00 2019-08-06\ 08:30:00 SpeFree2mC_2019-08-02noon_to_08-06morning.R
#only ch15 and 16 on Ban from 4th thru 7th
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_15-16.R 2019-08-04 2019-08-07 Ban15_16_2019-08-04_to_08-07.R
../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_2mC.R 2019-08-02\ 12:00:00 2019-08-06\ 08:30:00 Ban2mC_2019-08-02noon_to_08-06morning.R
../scripts/dt_selector.sh SpecterMicroAmpGraphMinimum_Channels_2mC.R 2019-08-02\ 12:00:00 2019-08-06\ 08:30:00 Spe2mC_2019-08-02noon_to_08-06morning.R

#days aug 6 7 8 9  on specter channels 3 11 12 16

#final days of specter after ch2 picks up
../scripts/dt_selector.sh SpecterMicroAmpGraphMinimum_Channels_Free.R 2019-08-09 2019-08-13 Speall_2019-08-09_to_08-13.R

../scripts/dt_selector.sh BansheeMicroAmpGraphAveraged_Channels_Free.R 2019-07-20\ 15:00:00 2019-08-13\ 09:59:00 BanFree.R

