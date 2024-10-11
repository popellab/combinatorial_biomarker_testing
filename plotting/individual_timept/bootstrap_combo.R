bootstrap_combo <- function(ncomb,
                            read_top_biomarkers_tmp, 
                            annotations_raw,
                            raw_data_biomarker,
                            bootstraped_vps_tmp,
                            n_bootstraps,
                            #total_responders,
                            #total_nonresponders,
                            timept){

  
  for (j in 1:n_bootstraps){
    
    vp_selected = c()
    #raw_data_biomarker_iterate = raw_data_biomarker
    names_together = NULL
    threshold_together = NULL
    sign_together = NULL
    
    bootstrapped_samples <- sample(c(1:nrow(raw_data_biomarker)), replace=TRUE)
    bootstrapped_data_iterate <- raw_data_biomarker[bootstrapped_samples,]
      
    total_responders = length(which(bootstrapped_data_iterate[, ncol(bootstrapped_data_iterate)] == "Responders"))
    total_nonresponders = length(which(bootstrapped_data_iterate[, ncol(bootstrapped_data_iterate)] == "Non-responders"))
    
    for(k in 1:ncomb){
      
      vp_selected_raw = c()
      
      index_quantity = which(colnames(read_top_biomarkers_tmp) == paste0('biomarker_', k, '_name'))
      index_threshold = which(colnames(read_top_biomarkers_tmp) == paste0('biomarker_', k, '_cutoff'))
      index_sign = which(colnames(read_top_biomarkers_tmp) == paste0('biomarker_', k, '_sign'))
      
      quantity_name = read_top_biomarkers_tmp[1, index_quantity]
      quantity_threshold = read_top_biomarkers_tmp[1, index_threshold]
      quantity_sign = read_top_biomarkers_tmp[1, index_sign]
      
      index_tmp = which(annotations_raw == quantity_name)
      
      if(quantity_sign == "Greater than"){
        vp_selected_raw = which(bootstrapped_data_iterate[, index_tmp] >= quantity_threshold)
      } else {
        vp_selected_raw = which(bootstrapped_data_iterate[, index_tmp] < quantity_threshold)
      }
      
      bootstrapped_data_iterate = bootstrapped_data_iterate[vp_selected_raw, ]
      
      if(k == 1){
        names_together = quantity_name
        threshold_together = quantity_threshold
        sign_together = quantity_sign
      }else{
        names_together = paste0(names_together, ' + ', quantity_name)
        threshold_together = paste0(threshold_together, ' + ', quantity_threshold)
        sign_together = paste0(sign_together, ' + ', quantity_sign)
      }

      
    }
    
    index_res = which(bootstrapped_data_iterate[, ncol(bootstrapped_data_iterate)] == "Responders")
    index_nonres = which(bootstrapped_data_iterate[, ncol(bootstrapped_data_iterate)] == "Non-responders")
    
    n_tt = length(index_res) + length(index_nonres)
    
    frac_resp = length(index_res) / total_responders
    frac_nonresp = length(index_nonres) / total_nonresponders
    ris_score = frac_resp - frac_nonresp
    resp_prob_score = length(index_res) / (n_tt) # response probability (within subset)
    
    tmp = c(timept,
            ncomb,
            names_together,
            threshold_together,
            sign_together,
            resp_prob_score,
            ris_score, 
            frac_resp,
            frac_nonresp)
    
    bootstraped_vps_tmp <- rbind(bootstraped_vps_tmp, tmp)
    
  }
  
  bootstraped_vps_tmp = data.frame(bootstraped_vps_tmp)
  colnames(bootstraped_vps_tmp) <- c("timept","ncombo","Quantity", "Threshold", "Sign", "Response_probability", "RIS", "Frac_resp", "Frac_nonresp")
  
  return(bootstraped_vps_tmp)
  
  
}


