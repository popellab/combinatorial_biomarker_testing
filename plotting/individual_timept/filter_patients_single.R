filter_patients_single <- function(raw_data_biomarker_tmp, read_data_tmp, annotations_tmp,resp_status_tmp){
  
  data_absolute_tmp = NULL
  data_fractions_tmp = NULL
  
  quantity_name = read_data_tmp$quantity[1]
  quantity_threshold = read_data_tmp$cutoff[1]
  quantity_sign = read_data_tmp$sign[1]
  
  index_tmp = which(annotations_tmp == quantity_name)
  
  if(quantity_sign == "Greater than"){
    vp_selected = which(raw_data_biomarker_tmp[, index_tmp] >= quantity_threshold)
  } else {
    vp_selected = which(raw_data_biomarker_tmp[, index_tmp] < quantity_threshold)
  }
  
  filtered_biomarker_data_tmp = raw_data_biomarker_tmp$Response[vp_selected]
  
  tmp_total = length(filtered_biomarker_data_tmp)
  data_absolute_tmp = rbind(data_absolute_tmp, c(1, tmp_total, "Total"))
  
  for(k_tmp in 1:length(resp_status_tmp)){
    count_tmp = length(which(filtered_biomarker_data_tmp == resp_status_tmp[k_tmp]))
    
    data_absolute_tmp = rbind(data_absolute_tmp, c(1, count_tmp, resp_status_tmp[k_tmp]))
    data_fractions_tmp = rbind(data_fractions_tmp, c(1, count_tmp/tmp_total, resp_status_tmp[k_tmp]))
    
  }
  
  colnames(data_absolute_tmp) <- c("ncombo", "value", "status")
  colnames(data_fractions_tmp) <- c("ncombo", "value", "status")
  
  vars_tmp=list(data_absolute_tmp, data_fractions_tmp)
  
}