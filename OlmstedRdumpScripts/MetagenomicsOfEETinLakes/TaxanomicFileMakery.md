#I want to make a file with all the taxonomy for every organism found in the metabolic output and the EET finder output. 

#flow of this script: 
#for every bin in: 
#/Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/FeGenie/EET-finder-Pipeline/PipelineOutput/Both_EET_Redox-discrete.csv
#if it finds that bin in 
#/Users/cnolmsted/Documents/MchMahon_Lab/Mentee_Documents/2020_Things/2020_summer_project_Roger-Ort/master_table_Fix25Nup.csv
#grab taxon info and all info from Both_EET_Redox-discrete.csv and put it in the output
#if not, if it finds it in 

#Never mind, I already have two files that will do just fine

#except I would like samplename in METABOLICandEETinfo.csv for this 

cd /Users/cnolmsted/Documents/MchMahon_Lab/METABOLIC/Editing_Metabolic
Bins=$(grep ",[0-9]," All_METABOLIC_N.csv | cut -d',' -f1 | sed 's/ Hit numbers//g')
for Bin in ${Bins[*]};do
echo -n "$(grep "^$Bin Hit numbers," All_METABOLIC_N.csv | sed 's/ Hit numbers//g')," >> METABOILCandEETinfo.csv
grep ",$Bin," /Users/cnolmsted/Documents/MchMahon_Lab/EET_sequencing_stuff/FeGenie/EET-finder-Pipeline/PipelineOutput/Both_EET_Redox-discrete.csv | cut -d',' -f3-6 >> METABOILCandEETinfo.csv
done