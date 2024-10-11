# script for biomarker testing

# clear existing output_files
rm -r output_files
 # response status for stable disease: "R"=responders; "NR" otherwise
SD_status="R"
# prefiltering for testing biomarker combinations: "topsingle" or "MLbased"
Prefiltering="MLbased"
# random forest "RF" or support vector machine "SVM"; only used for MLbased prefiltering
ML_method="RF"
# "negative" for negative predictive biomarkers; "positive" otherwise
predictive="positive"

# time point (baseline or d15 or d30 or relative_d15 or relative_d30)
#timept_var=("baseline" "d15" "d30" "relative_d15" "relative_d30")
timept_var=("baseline" "d15" "d30" "relative_d15" "relative_d30")

# compartment - "all" " Blood" " Lymph node" " Tumor"
#compartment=("all" " Blood" " Lymph node" " Tumor")
compartment=("all" " Blood" " Lymph node" " Tumor")

# metric: "ris" for RIS and "response" for response probability
# not used for single biomarker testing or combo with ML based prefiltering
metric=("response" "ris")
if [[ "$Prefiltering" == "MLbased" ]]; then
    metric=("unused")
fi

IFS=""

# number of biomarkers in combo: 2 to 4
ncombo=(2 3 4)

mkdir output_files
for loop in "${timept_var[@]}";
do
    mkdir output_files/$loop
    mkdir output_files/figures
    mkdir output_files/figures/cutoffs
    mkdir output_files/figures/cutoffs/Inc
    mkdir output_files/figures/cutoffs/Dec
    cp input_files/* output_files/$loop/.;
    
    #single biomarker testing
    Rscript rank_single_biomarkers_main.R $loop $predictive $SD_status
    
    #biomarker combo testing
    for i in "${metric[@]}"; do for j in "${ncombo[@]}"; do for k in "${compartment[@]}"; do Rscript rank_bio_combo_main_parallel.R $loop $k $i $j $Prefiltering "output_${ML_method}_${loop}_sd${SD_status}_${k}.csv" $SD_status $predictive; done; done; done

    mv output_files/figures output_files/$loop/.

done



