#!/usr/bin/env Rscript
library(ggplot2)
library(stringr)
library(tidyverse)
library(ggridges)
# exclude constant quantities
exclude_quantities <- c("Neo-Ag specific T cell clones")

args = commandArgs(trailingOnly = TRUE)
timepts <- str_split(args[1], fixed(","))[[1]]
timepts_name <- str_split(args[2], fixed(","))[[1]]
timepts_name2 <- c("baseline", "d15", "d30", "rel.change d15", "rel.change d30")

metric <- c("response", "ris")
metric_names <- c("Response probability", "RIS")

for(i in 1:length(metric)){
  
  final_data <- NULL
    
  for(k in 1:length(timepts)){
    # single biomarker
    read_data <- read.csv(paste0(timepts[k],'/single_', metric[i], '_test.csv'))
    
    if(timepts_name2[k] != "baseline"){
        if(length(exclude_quantities)>0){
          index_exclude = which(read_data$quantity %in% exclude_quantities) # row number to exclude
          if(length(index_exclude)>0){
            read_data <- data.frame(read_data[-index_exclude,])
          }
          
        }
        
    }
    
    if(metric[i] == "response"){
      final_data = rbind(final_data, cbind(rep(timepts_name2[k],length(read_data$response_probability)), read_data$response_probability))
    }else{
      final_data = rbind(final_data, cbind(rep(timepts_name2[k],length(read_data$ris)), read_data$ris))
    }
  }
  
  final_data <- data.frame(final_data)
  
  if(metric[i]=="response"){
    axis_lim = 0
  }else{
    axis_lim = -1
  }
  
  plot <- ggplot(final_data, aes(x=as.numeric(final_data[,2]),y=as.factor(final_data[,1])))+ 
    geom_density_ridges(alpha = 0.5,
                        jittered_points = FALSE, 
                        point_alpha=1,
                        point_shape=21,
                        point_size=0.4, 
                        scale=0.95, 
                        fill="aquamarine") + 
    #labs(x=metric_names[i], y='Time points')+ 
    labs(x=metric_names[i], y='')+ 
    theme_minimal() + 
    theme(axis.title.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.text.x = element_text(size = 10.5, angle = 0),
          axis.text.y = element_text(size = 11, angle = 0),
          strip.text.x = element_text(size = 13),
          #axis.ticks.x = element_blank(),
          legend.position = "none")
    guides(fill=FALSE,color=FALSE)+
      coord_cartesian(xlim=c(axis_lim,2))

  figname = paste0('density_ridge_',metric[i],'.png')
  png(figname, width = 4, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
}




