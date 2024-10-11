# calculates % increase in resp prob/RIS - filtered the ones with very high increase or decrease
library(ggplot2)
library(dplyr)

# exclude constant quantities
exclude_quantities <- c("Neo-Ag specific T cell clones")

args = commandArgs(trailingOnly = TRUE)
timept_new <- args[1]
timept_ref <- args[2]

metric <- c("response", "ris")
metric_names <- c("response probability", "RIS")

for(i in 1:length(metric)){
  
  read_data_new <- read.csv(paste0(timept_new, '/single_', metric[i], '_test.csv'))
  read_data_ref <- read.csv(paste0(timept_ref, '/single_', metric[i], '_test.csv'))
  
  read_data_new <- data.frame(read_data_new[order(read_data_new$quantity),])
  read_data_ref <- data.frame(read_data_ref[order(read_data_ref$quantity),])
  
  if(length(exclude_quantities)>0){
    index_exclude = which(read_data_new$quantity %in% exclude_quantities) # row number to exclude
    if(length(index_exclude)>0){
      read_data_new <- data.frame(read_data_new[-index_exclude,])
      read_data_ref <- data.frame(read_data_ref[-index_exclude,])
    }
    
  }
  
  
  if(metric[i]=="response"){
    perc_increase_val <- (read_data_new$response_probability-read_data_ref$response_probability)*100/abs(read_data_ref$response_probability)
  }else{
    perc_increase_val <- (read_data_new$ris-read_data_ref$ris)*100/abs(read_data_ref$ris)
  }
  
  perc_increase <- data.frame(cbind(read_data_ref$quantity, perc_increase_val))
  perc_increase_sorted <- perc_increase[order(as.numeric(perc_increase$perc_increase_val), decreasing = TRUE),]
  
  write.csv(read_data_new,paste0(timept_new, '_single_sorted.csv'),row.names = FALSE)
  write.csv(read_data_ref,paste0(timept_ref, '_single_sorted.csv'),row.names = FALSE)
  write.csv(data.frame(perc_increase_sorted),paste0(timept_new,'_', timept_ref,'_', metric[i],'_single_percincrease.csv'),row.names = FALSE)
  
  position = position_dodge(width = .75)
  
  perc_increase_sorted <- perc_increase_sorted %>%
    mutate(perc_increase_positive = as.numeric(perc_increase_sorted[, 2]) > 0)
  
  perc_increase_pos <- perc_increase_sorted[which(as.numeric(perc_increase_sorted[, 2])>0),]
  perc_increase_neg <- perc_increase_sorted[which(as.numeric(perc_increase_sorted[, 2])<0),]
  
  if(nrow(perc_increase_pos)>10){
    perc_increase_pos <- perc_increase_pos[1:10,]
  }
  
  if(nrow(perc_increase_neg)>10){
    perc_increase_neg <- perc_increase_neg[(nrow(perc_increase_neg)-9):nrow(perc_increase_neg),]
  }
  
  plot <- ggplot(perc_increase_pos) +
    geom_bar(aes(x = reorder(as.factor(perc_increase_pos[, 1]), as.numeric(perc_increase_pos[, 2])),
                 y = as.numeric(perc_increase_pos[, 2])),
             stat = "identity", position = position, fill = "#35978f", alpha = 0.9)+
    xlab(paste0("")) + 
    ylab(paste0('% increase in ', metric_names[i])) + 
    theme_bw() + 
    theme(#axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          #strip.text.x = element_text(size = 6)
          ) + 
    coord_flip()
  
  png(paste0('perc_inc_bar_', metric[i],'_',timept_new, '_', timept_ref, '_pos_filtered.png'), width = 7, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  plotneg <- ggplot(perc_increase_neg) +
    geom_bar(aes(x = reorder(as.factor(perc_increase_neg[, 1]), as.numeric(perc_increase_neg[, 2])),
                 y = as.numeric(perc_increase_neg[, 2])),
             stat = "identity", color = "white", position = position, fill = "#bf812d", alpha = 0.9)+
    xlab(paste0("")) + 
    ylab(paste0('% increase in ', metric_names[i])) + 
    theme_bw() + 
    theme(#axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          #strip.text.x = element_text(size = 6)
    )+ 
  coord_flip()
  
  png(paste0('perc_inc_bar_', metric[i],'_',timept_new, '_', timept_ref, '_neg_filtered.png'), width = 7, height = 4, units = "in", res = 300)
  print(plotneg)
  dev.off()
  
}

