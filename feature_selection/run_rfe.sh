# this script runs feature selection in python

# command line arguments
SD_status="R" # "R" to consider SD as responders; set "NR" otherwise
timepts=("baseline" "d15" "d30" "relative_d15" "relative_d30") # time point (baseline or d15 or d30)
filter_compartment=("all" " Blood" " Tumor" " Lymph node") # compartments
ML_method=("RF") # random forest & SVM

IFS=""

# run the python script with cmd args
for i in "${timepts[@]}"; do for j in "${filter_compartment[@]}"; do for k in "${ML_method[@]}"; do python3 feature_selection_RFE.py $SD_status $j $i $k > "score_${k}_${i}_sd${SD_status}_${j}.txt"; done; done; done




