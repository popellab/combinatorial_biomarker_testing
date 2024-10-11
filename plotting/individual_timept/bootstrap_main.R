# Bootstrapping for top biomarkers
args=commandArgs(trailingOnly = TRUE)
timept = args[1]

if(args[2] == "resp"){
    consider_SD_as_responder = TRUE
}else if(args[2] == "nonresp"){
    consider_SD_as_responder = FALSE
}

library(ggplot2)
library(dplyr)
library(ggpubr)
source('bootstrap_single.R')
source('bootstrap_combo.R')

n_bootstraps <- 1000 # Number of bootstrap samples
set.seed(100) 
metric <- c("response", "ris")
metric_name <- c("Response probability", "RIS")

# read annotations file
annotations_raw <- read.delim("biomarker_names_units_v1.txt", header = TRUE, sep = ",") 

# read biomarker candidate levels for all patients
raw_data_biomarker <- read.csv(paste0(timept, '_biomarkers_testset.csv'))
raw_data_biomarker = data.frame(raw_data_biomarker)

raw_data_biomarker = raw_data_biomarker %>%
  mutate(Response = replace(Response, Response == "CR/PR" | (Response == "SD" & consider_SD_as_responder) , "Responders")) %>%
  mutate(Response = replace(Response, Response == "PD" | (Response == "SD" & !consider_SD_as_responder) , "Non-responders")) %>% data.frame()

#total_responders = length(which(raw_data_biomarker$Response == "Responders"))
#total_nonresponders = length(which(raw_data_biomarker$Response == "Non-responders"))

for(i in 1:length(metric)){
  
  # single biomarker
  read_top_biomarkers <- read.csv(paste0('single_', metric[i], '_test.csv'))
  
  # bootstrapping
  bootstraped_vps <- NULL
  
  bootstraped_vps <- bootstrap_single(read_top_biomarkers_tmp = read_top_biomarkers, 
                               annotations_raw,
                               raw_data_biomarker,
                               bootstraped_vps_tmp = bootstraped_vps,
                               n_bootstraps,
                               #total_responders,
                               #total_nonresponders,
                               timept)
  
  # biomarker combinations
  for(bc in 2:4){
    
    read_top_combo <- read.csv(paste0('all', bc, '_', metric[i], '_validate.csv'))
    bootstraped_vps <- bootstrap_combo(ncomb = bc,
                                       read_top_biomarkers_tmp = read_top_combo, 
                                        annotations_raw,
                                        raw_data_biomarker,
                                        bootstraped_vps_tmp = bootstraped_vps,
                                        n_bootstraps,
                                       #total_responders,
                                       #total_nonresponders,
                                       timept)

  }
  
  
  # write output in csv files
  write.csv(data.frame(bootstraped_vps), paste0('bootstrap_results_', metric[i],'.csv'), row.names = F)
  
  if(metric[i] == "response"){
    value = bootstraped_vps$Response_probability
    max_y=1.3
  }else if(metric[i] == "ris"){
    value = bootstraped_vps$RIS
    max_y=1.0
  }
  
  
  plot <-  ggplot(data.frame(bootstraped_vps),
                  aes(x=as.factor(bootstraped_vps$ncombo),y=as.numeric(value),fill=factor(bootstraped_vps$ncombo)))+
    geom_point(pch = 20,position = position_jitterdodge(), size=0.15)+
    geom_boxplot(outlier.shape = NA,size=0.25,alpha=0.9)+
    xlab("No. of biomarkers")+
    ylab(metric_name[i])+ 
    theme_bw()+ 
    theme(axis.title.y = element_text(size=13),
          axis.title.x = element_text(size=13),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          legend.position="bottom"
          )+
    coord_cartesian(ylim = c(0,max_y))+
    scale_fill_manual(values=c("#CC79A7", "#D55E00", "#0072B2", "#F0E442"))
  
  png(paste0('bootstrap_plots_', metric[i], '.png'),width=4, height=4, units="in", res=300)
  print(plot)
  dev.off()
  
}
  
  

  



    

