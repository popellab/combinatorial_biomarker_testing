# thein: last updated 13 March 2024
# script to analyze and rank biomarker combinations

# command line arguments
args = commandArgs(trailingOnly = TRUE)

# start timer
tm_start <- proc.time()

# load required packages
packages <- c("ggplot2", "ggsignif", "writexl", "readxl", "parallel", "fabricatr", "tidyverse")
lapply(packages, library, character.only = TRUE)

# number of cores to run the analysis in parallel
ncores = detectCores() - 1 

# load required source files and functions
exec_files = c('src/filter_data.R',
                 'src/subset_vp.R', 
                 'src/display_progress.R',
                 'src/evaluate_biomarkers_combo.R',
                 'src/calc_comb_vec.R',
                 'src/calc_metrics.R',
                 'src/clean_data.R')

lapply(exec_files, source)

# time point naming
timept_var = args[1]
# compartment: "all", " Blood", " Lymph node", " Tumor"
compartment = args[2]
# metric: "score" for RIS; "response" for response probability
metric = args[3]
# number of biomarkers in combo from 2 to ..
ncombo = as.numeric(args[4])

Prefiltering=args[5] 
ML_result_file=args[6]

if(args[7] == "R"){
  consider_SD_as_responder = TRUE   # considering SD as responders
}else if(args[7] == "NR"){  
  consider_SD_as_responder = FALSE
}else{
  print("Error: unknown response status for SD")
}

predictive=args[8] # "negative" for negative predictive biomarkers
rf_sel_names = NULL

# ML based prefiltering
# ML based indices selected (after filtering unused_vars)
if(Prefiltering == "MLbased"){
  metric = "response"
  unused_vars = c(1, 2, 5:10, 21, 24, 32:34, 47, 48, 62, 83, 86, 93:96, 97, 100, 102, 103, 115)
  
  MLfile = read.csv(paste0('input_files/', ML_result_file), header = TRUE)
  rf_sel_vars = MLfile$index
  rf_sel_names = MLfile$name
  rf_sel_vars = rf_sel_vars + 1 # index starts from 0 in python
}

# no. of top single biomarkers to consider while generating combinations
ntop_biomarkers = 15 # if prefiltering is MLbased, this will not be used

# no of cutoffs/thresholds
ncutoffs = 8

# method to identify thresholds: uniform, quantiles
cutoff_method = "quantiles"

# minimum number of patients in every subgroup; subgroups with lower no of patients will be excluded
nmin_patients = 20 
nmin_patients_validation = 1

# input file names
biomarker_file_name = paste0('input_files/', timept_var, '_biomarkers_trainset.csv')
biomarker_file_name_test = paste0('input_files/', timept_var, '_biomarkers_testset.csv')
annotations_file_name = "input_files/biomarker_names_units_v1.txt"

# read files
raw_annotations <- read.delim(annotations_file_name, header = TRUE, sep = ",") # can use this file for validation as well
raw_data_train <- read.csv(biomarker_file_name)
raw_data_test <- read.csv(biomarker_file_name_test)

# file from single biomarker analysis # only for training
top_single_biomarkers_full <- read.csv(paste0('output_files/', timept_var, '/single_', metric, '_train.csv'))

if(Prefiltering == "MLbased"){
  # unused discard
  raw_data_train <- data.frame(raw_data_train[, -unused_vars])
  raw_data_test <- data.frame(raw_data_test[, -unused_vars])
  raw_annotations <- raw_annotations[-unused_vars,]

  filter_data_output <- filter_data(compartment,
                                    rf_sel_names,
                                    read_table_train = raw_data_train, 
                                    read_table_test = raw_data_test, 
                                    annotations_tmp = raw_annotations, 
                                    idx_keep = rf_sel_vars, 
                                    consider_SD_as_responder,
                                    Prefiltering)
  
  data_train = filter_data_output[[1]]
  data_test = filter_data_output[[2]]
  annotations = filter_data_output[[3]]
  
  # not used here?
  top_single_biomarkers = top_single_biomarkers_full
  
}else{
  # clean data
  cleaned_data <- clean_data(compartment,
                             rf_sel_names,
                             ntop_biomarkers, 
                             consider_SD_as_responder, 
                             data_train_tmp = raw_data_train, 
                             data_test_tmp = raw_data_test, 
                             annotations_tmp = raw_annotations,
                             top_biomarkers_tmp = top_single_biomarkers_full,
                             Prefiltering)
  
  data_train = cleaned_data[[1]]
  data_test = cleaned_data[[2]]
  annotations = cleaned_data[[3]]
  top_single_biomarkers = cleaned_data[[4]]
}
  
# train
evaluate_biomarkers_combo(metric_tmp = metric, 
                          compartment = compartment,
                          ncombo = ncombo,  
                          top_single_biomarkers = top_single_biomarkers, 
                          ntop_biomarkers = ntop_biomarkers,
                          annotations_tmp = annotations, 
                          consider_SD_as_responder = consider_SD_as_responder, 
                          data_tmp = data_train, 
                          cutoff_method = cutoff_method, 
                          ncutoffs = ncutoffs,
                          ncores = ncores,
                          train_or_validate = "train",
                          timept_var = timept_var,
                          nmin_patients_tmp = nmin_patients,
                          Prefiltering, predictive)
          

# validate
evaluate_biomarkers_combo(metric_tmp = metric, # for ML based: response
                          compartment,
                          ncombo,  
                          top_single_biomarkers = top_single_biomarkers, 
                          ntop_biomarkers,
                          annotations_tmp = annotations, 
                          consider_SD_as_responder, 
                          data_tmp = data_test, 
                          cutoff_method, 
                          ncutoffs = ncutoffs,
                          ncores,
                          train_or_validate = "validate",
                          timept_var,
                          nmin_patients_tmp = nmin_patients_validation,
                          Prefiltering, predictive)

if(Prefiltering == "MLbased"){
  # validate
  evaluate_biomarkers_combo(metric_tmp = "ris", 
                            compartment,
                            ncombo,  
                            top_single_biomarkers = top_single_biomarkers, 
                            ntop_biomarkers,
                            annotations_tmp = annotations, 
                            consider_SD_as_responder, 
                            data_tmp = data_test, 
                            cutoff_method, 
                            ncutoffs = ncutoffs,
                            ncores,
                            train_or_validate = "validate",
                            timept_var,
                            nmin_patients_tmp = nmin_patients_validation,
                            Prefiltering, predictive)

}

# stop timer
proc.time() - tm_start

