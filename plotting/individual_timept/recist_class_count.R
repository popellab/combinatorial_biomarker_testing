# calculates no. of patients with CR/PR, SD and PD for top single biomarkers or combinations
args=commandArgs(trailingOnly = TRUE)
timept = args[1]

library(ggplot2)
library(dplyr)
source('plot_CRPR_SD_PD.R')
source('filter_patients_single.R')
source('filter_patients_combo.R')

metric <- c("response", "ris")
datasets <- c("train", "test") #thein: test/validate
datasets2 <- c("train", "validate") #thein: test/validate

datasets_name <- c("Training", "Validation")
metric_names <- c("Response probability", "RIS")
resp_status = c("CR/PR", "SD", "PD")

colors <- c("grey","#5BB300","#00C19F","#DB72FB")
colors2 <- c("#FC4E07", "#00AFBB", "#E7B800")

annotations <- read.delim("biomarker_names_units_v1.txt", header = TRUE, sep = ",") 

for(k in 1:length(datasets)){# train or test

  for(i in 1:length(metric)){ 
    
    final_data_absolute <- NULL
    final_data_fractions <- NULL

    # read the file with biomarker candidate levels for all patients
    raw_data_biomarker <- read.csv(paste0(timept, '_biomarkers_', datasets[k],'set.csv'))
    raw_data_biomarker = data.frame(raw_data_biomarker)
    
    # single biomarker analysis output file
    read_data <- read.csv(paste0('single_', metric[i], '_', datasets[k], '.csv'))
    filter_patients_single_output <- filter_patients_single(raw_data_biomarker, read_data, annotations$Name, resp_status)

    final_data_absolute = rbind(final_data_absolute, filter_patients_single_output[[1]])
    final_data_fractions = rbind(final_data_fractions, filter_patients_single_output[[2]])
    
    # biomarker combinations
    for(j in 2:4){
      
      read_data_combo <- read.csv(paste0('all', j, '_', metric[i], '_', datasets2[k], '.csv'))
      filter_patients_combo_output <- filter_patients_combo(j, raw_data_biomarker, read_data_combo, annotations$Name, resp_status)
      
      final_data_absolute = rbind(final_data_absolute, filter_patients_combo_output[[1]])
      final_data_fractions = rbind(final_data_fractions, filter_patients_combo_output[[2]])
    }

  plot_CRPR_SD_PD(final_data_absolute, final_data_fractions, metric_names[i], colors, colors2, datasets2[k])

  }
  
}
