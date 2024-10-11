library(ggplot2)
library(tidyverse)
library(ggridges)

metric <- c("response", "ris")
metric_names <- c("Response probability", "RIS")

for(i in 1:length(metric)){
  
  final_data <- NULL
    
  for(j in 1:4){
    
    if(j==1){ # single biomarker
      read_data <- read.csv(paste0('single_', metric[i], '_test.csv')) #thein: test/validate
    }else{
      read_data <- read.csv(paste0('all', j, '_', metric[i], '_validate.csv')) #thein: test/validate
    }
    
    if(metric[i] == "response"){
      final_data = rbind(final_data, cbind(rep(j,length(read_data$response_probability)), read_data$response_probability))
      x_min = 0
      x_max = 1
    }else{
      final_data = rbind(final_data, cbind(rep(j,length(read_data$ris)), read_data$ris))
      x_min = -0.5
      x_max = 0.5
    }
    
  }
  
  final_data <- data.frame(final_data)
  
  plot <- ggplot(final_data, aes(x=as.numeric(final_data[,2]),y=as.factor(final_data[,1]), fill=as.factor(final_data[,1])))+
    geom_density_ridges(alpha = 0.5,jittered_points = FALSE, point_alpha=1,point_shape=21,point_size=0.1, scale=0.95) + 
    labs(x=metric_names[i], y='No. of biomarkers')+ 
    theme_minimal() + 
    theme(axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 13, angle = 0),
          axis.text.y = element_text(size = 13),
          strip.text.x = element_text(size = 15),
          legend.position = "none")+
    scale_fill_manual(values=c("#CC79A7", "#D55E00", "#0072B2", "#F0E442"))

  figname = paste0('density_ridge_',metric[i],'.png')
  png(figname, width = 4, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
}




