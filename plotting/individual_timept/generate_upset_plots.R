generate_upset_plots <- function(index, filename_tmp, metric_tmp, colors){
  
  biomarker_list <- list()
  value <- c()
  color_list <- c()
  
  for(i in 1:length(filename_tmp)){ 
    data_biomarker <- read.csv(filename_tmp[i])
    
    if(i == 1){
      # splits string by " + " 
      biomarker_list[[i]] = str_split_1(data_biomarker$quantity[index], fixed(" + "))
      if(metric_tmp == "response"){
          value[i] = data_biomarker$response_probability[index]
      }else{
          value[i] = data_biomarker$ris[index]
      }
      
      color_list[i] = colors[i]
      
    }else{
      
      for(j in 1:nrow(data_biomarker)){
        iterate_output <- iterate_count_matches(j, fulldata_vec_short = data_biomarker, 
                                                biomarker_list_tmp = biomarker_list, 
                                                value_tmp = value, 
                                                metric_tmp)
        biomarker_list = iterate_output[[1]]
        value = iterate_output[[2]]
        if(length(value)>length(color_list)) color_list[length(color_list)+1] = colors[i]
        
      }
      
    }
  }
  
  combined_data <- tibble(biomarker_list, value, color_list)
  
  plot <- ggplot(combined_data, aes(x = biomarker_list, y = value)) +
    geom_bar(stat='identity', fill=color_list, alpha=0.75) + 
    theme_minimal() +
    scale_x_upset(order_by = "degree", position = "top")+
    theme_combmatrix(combmatrix.panel.point.color.fill = color_list,
                     combmatrix.panel.line.size = 1)+ylab("")+xlab("")
  
  widthval=4
  if(metric_tmp == "response"){widthval = 4.5}
  
  figname = paste0(paste0('upset_', metric_tmp,'_panel', index, '.png'))
  png(figname, width = widthval, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
}

