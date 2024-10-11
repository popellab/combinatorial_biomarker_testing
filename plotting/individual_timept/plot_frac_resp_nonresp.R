plot_frac_resp_nonresp <- function(final_data_tmp, colors, metric_tmp, train_or_test){
  
  plot <-  ggplot() +
    geom_bar(aes(x = as.factor(final_data_tmp[, 1]),
                 y = as.numeric(final_data_tmp[, 5]),
                 fill = as.factor(final_data_tmp[, 6])), 
             position = "dodge",
             stat = "identity",
             alpha=0.78) +
    ylab("Fraction of patients") +
    xlab("No. of biomarkers") +
    theme_bw() + 
    theme(axis.title.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.text.x = element_text(size = 11, angle = 0),
          axis.text.y = element_text(size = 11),
          strip.text.x = element_text(size = 13),
          legend.position = "bottom") +
    coord_cartesian(ylim = c(0, 1)) + 
    guides(fill = guide_legend(title = " ")) +
    scale_fill_manual(values = c(colors)) 
  
  figname = paste0('plot_comp_',metric_tmp,'_',train_or_test,'.png')
  png(figname, width = 4, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
}