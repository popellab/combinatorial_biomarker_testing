# calculates % increase in resp prob/RIS with respect to a reference timept
library(ggplot2)
library(dplyr)
library(ggbreak)

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
  
  write.csv(read_data_new, paste0(timept_new, '_single_sorted.csv'), row.names = FALSE)
  write.csv(read_data_ref, paste0(timept_ref, '_single_sorted.csv'), row.names = FALSE)
  write.csv(data.frame(perc_increase), paste0(timept_new,'_', timept_ref,'_', metric[i],'_single_percincrease.csv'), row.names = FALSE)
  
  position = position_dodge(width = .75)
  
  perc_increase <- perc_increase %>%
    mutate(perc_increase_positive = as.numeric(perc_increase[, 2]) > 0)
  
  plot <- ggplot(perc_increase) +
    geom_bar(aes(x = reorder(as.factor(perc_increase[, 1]), as.numeric(perc_increase[, 2])),
                 y = as.numeric(perc_increase[, 2]), fill = perc_increase_positive),
             stat = "identity", position = position, alpha = 0.9)+
    xlab(paste0("Biomarker candidates")) + 
    ylab(paste0('% increase in ', metric_names[i])) + 
    theme_bw() + 
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank()
          #strip.text.x = element_text(size = 6)
          ) + 
    scale_fill_manual(values = c("#bf812d", "#35978f"))+
    coord_flip()#+ scale_x_break(c(5000, 10000), scales = 1.5)

  png(paste0('perc_inc_bar_', metric[i],'_',timept_new, '_', timept_ref, '.png'), 
      width = 5, height = 6, units = "in", res = 300)
  print(plot)
  dev.off()
  
}

