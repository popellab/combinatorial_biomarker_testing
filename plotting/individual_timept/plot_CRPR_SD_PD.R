plot_CRPR_SD_PD <- function(final_data_absolute_tmp, 
                            final_data_fractions_tmp, 
                            metric_names_tmp, 
                            colors_tmp, 
                            colors2_tmp,
                            datasets2_tmp){

  final_data_absolute_tmp = data.frame(final_data_absolute_tmp)
  final_data_fractions_tmp = data.frame(final_data_fractions_tmp)
  
   plot <-  ggplot() +
    geom_bar(aes(x = as.factor(final_data_absolute_tmp$ncombo),
                 y = as.numeric(final_data_absolute_tmp$value),
                 fill = factor(final_data_absolute_tmp$status,levels=c("Total","CR/PR","SD","PD"))), 
             position = "dodge",
             stat = "identity",
             alpha=0.78) +
    ylab("No. of patients") +
    xlab("No. of biomarkers") +
    theme_bw() + 
    theme(axis.title.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.text.x = element_text(size = 11, angle = 0),
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 13),
          legend.position = "bottom") +
    guides(fill = guide_legend(title = " ")) +
    scale_fill_manual(values = c(colors_tmp)) 
  
  figname = paste0('abs_respstatus_',metric_names_tmp,'_',datasets2_tmp,'.png')
  png(figname, width = 4, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  
  plot <-  ggplot() +
    geom_bar(aes(x = as.factor(final_data_fractions_tmp$ncombo),
                 y = as.numeric(final_data_fractions_tmp$value),
                 fill = factor(final_data_fractions_tmp$status,levels=c("CR/PR","SD","PD"))), 
             position = "stack",
             stat = "identity",
             alpha=0.75) +
    ylab("Fraction of patients") +
    xlab("No. of biomarkers") +
    theme_bw() + 
    theme(axis.title.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.text.x = element_text(size = 11, angle = 0),
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 13),
          legend.position = "bottom") +
    guides(fill = guide_legend(title = " ")) +
    scale_fill_manual(values = c(colors2_tmp)) 
  
  figname = paste0('frac_respstatus_',metric_names_tmp,'_',datasets2_tmp,'.png')
  png(figname, width = 4, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  
}