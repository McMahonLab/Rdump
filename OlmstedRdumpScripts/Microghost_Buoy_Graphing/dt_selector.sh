#this script will take arg1 as document to be adapted
#arg2 as first pattern to change "2019-08-04" throughout
#arg3 as the seccond pattern to change "2019-08-06" throughout
#and arg4 as the output filename.
#no quotes needed in arguments
#make a directory called AdaptedRscripts/
Inp=$1
P1=$2
P2=$3
Out=$4

sed "s/2019-08-04/abcdefg/g; s/2019-08-06/zyxwvut/g; s/abcdefg/$2/g; s/zyxwvut/$3/g" $1 >> AdaptedRscripts/$4
#how to run example
#be in folder with file to change i.e. Date_range_R_scripts
#run: ../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-07 Ban2019-08-06_07.R
#or ../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_2mC.R 2019-08-06 2019-08-07 Ban2mC2019-08-06_07.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-07 BanAll2019-08-06_07.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all.R 2019-08-06 2019-08-09 Ban2019-08-06_09.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_15-16.R 2019-08-06 2019-08-09 Ban15-16_2019-08-06_09.R

#or in 2020
#../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all2020.R 2020-08-16 2020-09-03 BanAll_2020-08-16_09-03.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all2020.R 2020-08-26 2020-08-28 BanAll_2020-08-26_08-28.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphMinimum_Channels_all2020.R 2020-08-27 2020-08-30 BanAll_2020-08-27_08-30.R
#../scripts/dt_selector.sh SpecterMicroAmpGraphMinimum_Channels_all2020.R 2020-08-27 2020-08-30 SpeAll_2020-08-27_08-30.R
#../scripts/dt_selector.sh SpecterMicroAmpGraphMinimum_Channels_all2020.R 2020-08-16 2020-09-03 SpeAll_2020-08-16_09-03.R

#or in 2021, well actually for 2019 
#../scripts/dt_selector.sh BansheeMicroAmpGraphAveraged_Channels_all.R 2019-07-21 2019-08-13 BanAllAve2019-07-21_to_08-13.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphAveraged_customChannels.R 2019-07-21 2019-08-13 BanCusAve2019-07-21_to_08-13.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphAveraged_customChannels.R 2019-08-06 2019-08-10 BanCusAve2019-08-06_10.R
#../scripts/dt_selector.sh SpecterMicroAmpGraphAveraged_customChannels2.R 2019-08-08 2019-08-11 SpeCusAve2019-08-08_11.R
#../scripts/dt_selector.sh BansheeMicroAmpGraphAveraged_customChannels2.R 2019-08-08 2019-08-10 BanCus2_2019-08-08_08-10.R
#../scripts/dt_selector.sh SpecterMicroAmpGraphAveraged_customChannels2.R 2019-08-08 2019-08-11 SpeCus4_2019-08-08_08-11.R

chmod +x AdaptedRscripts/$4
./AdaptedRscripts/$4