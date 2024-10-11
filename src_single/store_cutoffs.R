store_cutoffs <- function(name_quantity_exact, name_quantity, data_tmp){
  
  data_tmp <- data.frame(data_tmp)
  
  data_tmp_inc=data_tmp[which(data_tmp$sign == "Greater than"),]
  data_tmp_dec=data_tmp[which(data_tmp$sign == "Less than"),]
  
  plot_cutoffs(name_quantity_exact, name_quantity, data_tmp_inc, "Inc")
  plot_cutoffs(name_quantity_exact, name_quantity, data_tmp_dec, "Dec")
  
  
}
