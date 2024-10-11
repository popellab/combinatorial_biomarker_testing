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
compartments <- c("all", " Blood", " Lymph node", " Tumor")
compartments2 <- c("Multiple tissues", " Blood", " Lymph node", " Tumor")

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
  
  bootstraped_vps <- NULL
  
  for(jc in 1:length(compartments)){
    
    bootstraped_vps_temp <- NULL
    
    # single biomarker
    read_top_biomarkers <- read.csv(paste0('single_', metric[i], '_test.csv'))
    
    if(compartments[jc] != "all"){
      
      read_top_biomarkers <- read_top_biomarkers[which(read_top_biomarkers$compartment == compartments[jc]),]
      
      bootstraped_vps_temp <- bootstrap_single(read_top_biomarkers_tmp = read_top_biomarkers, 
                                          annotations_raw,
                                          raw_data_biomarker,
                                          bootstraped_vps_tmp = bootstraped_vps_temp,
                                          n_bootstraps,
                                         # total_responders,
                                        #  total_nonresponders,
                                          timept)
      
    }
    
    # biomarker combinations
    for(bc in 2:4){
      
      read_top_combo <- read.csv(paste0(compartments[jc], bc, '_', metric[i], '_validate.csv'))
      bootstraped_vps_temp <- bootstrap_combo(ncomb = bc,
                                         read_top_biomarkers_tmp = read_top_combo, 
                                         annotations_raw,
                                         raw_data_biomarker,
                                         bootstraped_vps_tmp = bootstraped_vps_temp,
                                         n_bootstraps,
                                         #total_responders,
                                         #total_nonresponders,
                                         timept)
      
    }
    
    compartment_name = rep(compartments2[jc], nrow(bootstraped_vps_temp))
    bootstraped_vps = rbind(bootstraped_vps, cbind(bootstraped_vps_temp, compartment_name))
    
    write.csv(data.frame(bootstraped_vps_temp), paste0('bootstrap_results_', metric[i],'_',compartments[jc],'.csv'), row.names = F)
  }

  if(metric[i] == "response"){
    value_metric = bootstraped_vps$Response_probability
  }else if(metric[i] == "ris"){
    value_metric = bootstraped_vps$RIS
  }
  
  bootstraped_vps = data.frame(cbind(bootstraped_vps, value_metric))

  plot <-  ggplot(data.frame(bootstraped_vps),
                  aes(x=as.factor(bootstraped_vps$ncombo),y=as.numeric(bootstraped_vps$value_metric),fill=factor(bootstraped_vps$compartment_name)))+
   geom_point(pch = 20,position = position_jitterdodge(), size=0.15)+
    geom_boxplot(outlier.shape = NA,size=0.25,alpha=0.9)+
    #facet_wrap(~factor(bootstraped_vps$ncombo),  ncol=4)+
    xlab("No. of biomarkers")+
    ylab(metric_name[i])+ 
    theme_bw()+ 
    theme(axis.title.y = element_text(size=15),
          axis.title.x = element_text(size=15),
          axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          legend.position="top"
    )+
    coord_cartesian(ylim = c(0,1))+
    scale_fill_manual(values=c("brown2", "cadetblue1", "burlywood1","grey"))+
    guides(fill = guide_legend(title = " "))
  
  png(paste0('bootstrap_plots_tissues_', metric[i], '.png'),width=5, height=5, units="in", res=300)
  print(plot)
  dev.off()
  
}









