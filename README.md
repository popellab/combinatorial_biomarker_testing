# combinatorial_biomarker_testing

This is the computational code for biomarker testing algorithm developed by the Popel Systems Biology Laboratory (https://popellab.johnshopkins.edu/) at Johns Hopkins University. 


Description of contents:

- feature_selection: contains a python script to run random forest-based feature selection and a bash script for automation. 

- input_files: input folder for cutoff-based biomarker testing algorithm; contains data on pre-treatment and on-treatment biomarker levels of virtual patients (.csv files), feature selection output files (.csv) and an annotation file (biomarker_names_units_v1.txt)

- plotting: contains plotting scripts (R scripts)

- src: R scripts used for biomarker combination analysis

- src_single: R scripts used for single biomarker analysis

- rank_bio_combo_main_parallel.R: main R script for biomarker combination analysis; runs in parallel

- rank_single_biomarkers_main.R: main R script for single biomarker analysis

- run_all.sh: bash script to run the cutoff-based biomarker testing


Steps to run the biomarker testing algorithm:

Note: Please install the following R packages before running the code: "ggplot2", "ggpattern", "writexl", "readxl", "parallel","tidyverse", "gghighlight", "ggsignif", "fabricatr".

1) To run the random forest based feature selection: (optional as output files are already provided in the folder input_files)

- copy the input files (biomarker levels in virtual patients & annotation file from folder input_files) into feature_selection folder
- execute the bash script run_rfe.sh (in folder feature_selection).
- output .csv files will be generated 

2) To run the cutoff-based biomarker testing (for both single biomarkers and combinations)

- Transfer the output files of feature selection containing selected biomarker candidates to folder input_files
- execute the bash script run_all.sh 
- a new folder output_files will be generated containing multiple subfolders (baseline, d15, d30, relative_d15, relative_d30)

3) To analyze the results and generate figures for individual timepoints

- copy the scripts from folder Plotting/individual_timept, into the output subfolders (baseline, d15, d30, relative_d15, relative_d30)
- set the variable timept in plotall.R. This should be the same as the folder name. 
- execute the R script plotall.R, which inturn runs other R scripts

4) To generate figures comparing different timepoints

- copy the scripts from folder Plotting/compare_timepts, into the output subfolders (baseline, d15, d30, relative_d15, relative_d30)
- execute the R script plotall_timepts.R

Reference: Virtual patient analysis identifies strategies to improve the performance of predictive biomarkers for PD-1 blockade
Theinmozhi Arulraj, Hanwen Wang, Atul Deshpande, Ravi Varadhan, Leisha A. Emens, Elizabeth M. Jaffee, Elana J. Fertig, Cesar A. Santa-Maria, Aleksander S. Popel
bioRxiv 2024.05.21.595235; doi: https://doi.org/10.1101/2024.05.21.595235

