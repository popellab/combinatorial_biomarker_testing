# script to plot bootstrapped results: compare different timepoints
library(ggplot2)
library(dplyr)
library(stringr)
library(ggpubr)

args = commandArgs(trailingOnly = TRUE)
timept <- str_split(args[1], fixed(","))[[1]]

metric <- c("response", "ris")
metric_name <- c("Response probability", "RIS")

for(i in 1:length(metric)){
    
    bootstraped_vps_combined <- NULL
    for(j in 1:length(timept)){
        read_data <- read.csv(paste0(timept[j], '/bootstrap_results_', metric[i],'.csv'))
        bootstraped_vps_combined = rbind(bootstraped_vps_combined, read_data)
    }
    
    bootstraped_vps_combined = data.frame(bootstraped_vps_combined)
  
  if(metric[i] == "response"){
    value = bootstraped_vps_combined$Response_probability
  }else if(metric[i] == "ris"){
    value = bootstraped_vps_combined$RIS
  }
  
  
  plot <-  ggplot(bootstraped_vps_combined,
                  aes(x=factor(bootstraped_vps_combined$timept),y=as.numeric(value),
                      fill=factor(bootstraped_vps_combined$timept)))+
    geom_point(pch = 20,position = position_jitterdodge(), size=0.5,alpha=0.2)+
    geom_boxplot(outlier.shape = NA,size=0.25,alpha=0.9)+
    facet_wrap(~factor(bootstraped_vps_combined$ncombo),  ncol=4)+
    xlab("")+
    ylab(metric_name[i])+ 
    theme_bw()+ 
    theme(axis.title.y = element_text(size=10),
          axis.title.x = element_text(size=10),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=10),
          strip.text.x = element_text(size=15),
          strip.background = element_rect(
            color="black", fill="beige"
          ),
          axis.ticks.x = element_blank(),
          legend.position="top"
          )+
    guides(fill = guide_legend(title = " "))+
    coord_cartesian(ylim=c(0,1.0))

  
  png(paste0('bootstrap_plots_', metric[i], '.png'),width=4.5, height=4.1, units="in", res=300)
  print(plot)
  dev.off()
  
}
  
  

  



    

