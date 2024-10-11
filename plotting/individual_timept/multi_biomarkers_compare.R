# script to compare the predictive power of single and multi-biomarker panels
# generates upset plots

library(ggplot2)
library(tidyverse)
library(ggupset)
library(stringr)

source('generate_upset_plots.R')
source('count_matches.R')
source('iterate_count_matches.R')

colors <- c("#F0E442", "#0072B2", "#D55E00", "#CC79A7")

index_4combo = c(1:5) # row no in 4 combo file
metric = c("response", "ris") 

for(m in 1:length(metric)){
  
  filenames = c(paste0('all4_', metric[m], '_validate.csv'), #4combo 
                paste0('all3_', metric[m], '_validate.csv'), #3combo
                paste0('all2_', metric[m], '_validate.csv'), #2combo
                paste0('single_', metric[m], '_test.csv')) # single
  
  lapply(index_4combo, 
         generate_upset_plots, 
         filename_tmp = filenames, 
         metric_tmp = metric[m], 
         colors)
  
}

  
