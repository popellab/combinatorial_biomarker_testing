plot_cutoffs <- function(name_quantity_exact, name_quantity, data_final_tmp, str_condition){
  
  if(nrow(data_final_tmp)>0){
    
    maxval = max(as.numeric(data_final_tmp$response_probability))
    maxval_y=data_final_tmp$response_probability[which(as.numeric(data_final_tmp$response_probability) == maxval)]
    maxval_x=data_final_tmp$cutoff[which(as.numeric(data_final_tmp$response_probability) == maxval)]
    data_highlight_resp = data.frame(cbind(maxval_x, maxval_y))
    
    maxval_ris = max(as.numeric(data_final_tmp$ris))
    maxval_y_ris=data_final_tmp$ris[which(as.numeric(data_final_tmp$ris) == maxval_ris)]
    maxval_x_ris=data_final_tmp$cutoff[which(as.numeric(data_final_tmp$ris) == maxval_ris)]
    data_highlight_ris = data.frame(cbind(maxval_x_ris, maxval_y_ris))
    
    plot <- ggplot(data_final_tmp, aes(x=as.numeric(data_final_tmp$cutoff))) +
      geom_line( aes(y=as.numeric(data_final_tmp$response_probability)), color="cyan4", linewidth=1) +
      geom_line( aes(y=as.numeric(data_final_tmp$ris)), color="goldenrod3", linewidth=1) +
      geom_point( aes(y=as.numeric(data_final_tmp$ris)),size=1.8,shape=21,fill="white",colour="goldenrod3") +
      geom_point( aes(y=as.numeric(data_final_tmp$response_probability)),size=1.8,shape=21,fill="white",colour="cyan4") +
      geom_point(data=data_highlight_resp, aes(y=as.numeric(maxval_y), as.numeric(x=maxval_x)),size=3.5,shape=21,fill="cyan4",colour="black")+
      geom_point(data=data_highlight_ris, aes(y=as.numeric(maxval_y_ris), as.numeric(x=maxval_x_ris)),size=3.5,shape=21,fill="goldenrod3",colour="black")+
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "black", size=15),
        axis.title.x = element_text(color = "black", size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)
      )+
      coord_cartesian(ylim=c(-1,1)) +
      ylab("Response probability or RIS") +
      xlab(name_quantity_exact) 
    
    figname = paste0('output_files/figures/cutoffs/', str_condition,'/',name_quantity, '.png')
    png(figname, width=5, height=4, units="in", res=300)
    print(plot)
    dev.off()
  
  
  }
  
}



