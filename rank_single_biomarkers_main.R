## thein: last updated 13 March 2024
# script for identifying single biomarkers and validation in an independent dataset

# command line arguments
args = commandArgs(trailingOnly = TRUE)

# start timer
tm_start <- proc.time()

# load required packages & functions
packages <- c("ggplot2", "ggpattern", "writexl", "readxl", "parallel","tidyverse", "gghighlight") 
lapply(packages, library, character.only = TRUE) 

fn_names <- c('src_single/filter_data.R', 'src_single/subset_vp.R', 'src_single/add_counts.R', 'src_single/evaluate_biomarkers.R',
              'src_single/plotting.R', 'src_single/calc_metrics.R', 'src_single/store_cutoffs.R', 'src_single/plot_cutoffs.R')
lapply(fn_names, source)

# Hard coded variables
# indices of unused quantities
unused_vars = c(1, 2, 5:10, 21, 24, 32:34, 47, 48, 62, 83, 86, 93:96, 97, 100, 102, 103, 115)

# number of virtual patient subsets/thresholds
VP_nsubsets = 8

# minimum number of patients per subset
min_VPs = 20 
min_VPs_validation = 1

# method to identify thresholds: uniform (uniformly spaced cutoffs), quantiles
method_threshold = "quantiles"

# naming timepoint
timept_var = args[1]
# "positive"/"negative" for positive/negative predictive biomarkers
predictive=args[2]

if(args[3] == "R"){
  consider_SD_as_responder = TRUE   # considering SD as responders
}else if(args[3] == "NR"){
  consider_SD_as_responder = FALSE
}else{
  print("Error: unknown response status for SD")
}

# input file names
biomarker_file_name = paste0('input_files/', timept_var, '_biomarkers_trainset.csv')
biomarker_file_name_test = paste0('input_files/', timept_var, '_biomarkers_testset.csv')
annotations_file_name = "input_files/biomarker_names_units_v1.txt"
  
# read input files
# annotation file with biomarker names (col 1), units (col 2) & compartment (col 3)
annotations_raw <- read.delim(annotations_file_name, header = TRUE, sep = ",")
# a file with biomarker candidate levels (cols) for virtual patients (rows)
raw_data_train <- read.csv(biomarker_file_name) %>% data.frame() # train data
raw_data_test <- read.csv(biomarker_file_name_test) %>% data.frame() # test data

# filtering train data
filter_output = filter_data(raw_data_train, raw_data_test, annotations_raw, unused_vars)
raw_data_train = filter_output[[1]]
raw_data_test = filter_output[[2]]
annotations = filter_output[[3]]

# calculating total no. of VPs, responders and non-responders
add_counts_output <- add_counts(raw_data_train, consider_SD_as_responder)
tt_VPs_train = add_counts_output[[1]]
tt_responders_train = add_counts_output[[2]]
tt_nonresponders_train = add_counts_output[[3]]

# training
evaluate_biomarkers_output <- evaluate_biomarkers(raw_data_train, 
                                                  VP_nsubsets, 
                                                  min_VPs,
                                                  tt_responders_train,
                                                  tt_nonresponders_train,
                                                  annotations,
                                                  method_threshold,
                                                  NULL,
                                                  NULL,
                                                  consider_SD_as_responder,
                                                  predictive,
                                                  "train")
biomarker_summary_train = evaluate_biomarkers_output[[1]]
plotting(biomarker_summary_train, biomarker_summary_train, "train", method_threshold, VP_nsubsets, timept_var, predictive)

# calculating total no. of VPs, responders and non-responders
add_counts_output_test <- add_counts(raw_data_test, consider_SD_as_responder)
tt_VPs_test = add_counts_output_test[[1]]
tt_responders_test = add_counts_output_test[[2]]
tt_nonresponders_test = add_counts_output_test[[3]]

# training set results as input for validation
response_train_data <- read.csv(paste0('output_files/', timept_var, '/single_response_train.csv')) %>% data.frame()
ris_train_data <- read.csv(paste0('output_files/', timept_var, '/single_ris_train.csv')) %>% data.frame()

evaluate_biomarkers_output_test <- evaluate_biomarkers(raw_data_test, 
                                                  VP_nsubsets, 
                                                  min_VPs_validation,
                                                  tt_responders_test,
                                                  tt_nonresponders_test,
                                                  annotations,
                                                  method_threshold,
                                                  response_train_data,
                                                  ris_train_data,
                                                  consider_SD_as_responder,
                                                  predictive,
                                                  "test")
response_biomarker_summary_test = evaluate_biomarkers_output_test[[1]]
ris_biomarker_summary_test = evaluate_biomarkers_output_test[[2]]
plotting(response_biomarker_summary_test, ris_biomarker_summary_test, "test", method_threshold, VP_nsubsets, timept_var, predictive)
  
# stop timer
proc.time() - tm_start
