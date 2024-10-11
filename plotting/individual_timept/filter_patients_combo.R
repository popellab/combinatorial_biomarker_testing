filter_patients_combo <- function(j_tmp, raw_data_biomarker_tmp, read_data_tmp, annotations_tmp, resp_status_tmp){
  
  raw_data_bio_iterate = raw_data_biomarker_tmp
  data_absolute_tmp = NULL
  data_fractions_tmp = NULL
  
  for(i_tmp in 1:j_tmp){
    
    index_quantity = which(colnames(read_data_tmp) == paste0('biomarker_', i_tmp, '_name'))
    index_threshold = which(colnames(read_data_tmp) == paste0('biomarker_', i_tmp, '_cutoff'))
    index_sign = which(colnames(read_data_tmp) == paste0('biomarker_', i_tmp, '_sign'))
      
    quantity_name = read_data_tmp[1, index_quantity]
    quantity_threshold = read_data_tmp[1, index_threshold]
    quantity_sign = read_data_tmp[1, index_sign]
    
    index_tmp = which(annotations_tmp == quantity_name)
    
    if(quantity_sign == "Greater than"){
      vp_selected = which(raw_data_bio_iterate[, index_tmp] >= quantity_threshold)
    } else {
      vp_selected = which(raw_data_bio_iterate[, index_tmp] < quantity_threshold)
    }
    
    raw_data_bio_iterate = raw_data_bio_iterate[vp_selected, ]
  }

  filtered_biomarker_data_tmp = raw_data_bio_iterate$Response
  
  tmp_total = length(filtered_biomarker_data_tmp)
  data_absolute_tmp = rbind(data_absolute_tmp, c(j_tmp, tmp_total, "Total"))
  
  for(k_tmp in 1:length(resp_status_tmp)){
    count_tmp = length(which(filtered_biomarker_data_tmp == resp_status_tmp[k_tmp]))
    
    data_absolute_tmp = rbind(data_absolute_tmp, c(j_tmp, count_tmp, resp_status_tmp[k_tmp]))
    data_fractions_tmp = rbind(data_fractions_tmp, c(j_tmp, count_tmp/tmp_total, resp_status_tmp[k_tmp]))
    
  }
  
  colnames(data_absolute_tmp) <- c("ncombo", "value", "status")
  colnames(data_fractions_tmp) <- c("ncombo", "value", "status")
  vars_tmp=list(data_absolute_tmp, data_fractions_tmp)
  
}