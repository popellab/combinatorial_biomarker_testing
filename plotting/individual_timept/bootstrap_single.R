bootstrap_single <- function(read_top_biomarkers_tmp, 
                             annotations_raw,
                             raw_data_biomarker,
                             bootstraped_vps_tmp,
                             n_bootstraps,
                             #total_responders,
                             #total_nonresponders,
                             timept){
  
  quantity_name = read_top_biomarkers_tmp$quantity[1]
  quantity_threshold = read_top_biomarkers_tmp$cutoff[1]
  quantity_sign = read_top_biomarkers_tmp$sign[1]
  
  index_tmp = which(annotations_raw == quantity_name)
  
  for (j in 1:n_bootstraps){
    
    bootstrapped_sample <- sample(c(1:nrow(raw_data_biomarker)), replace=TRUE)
    bootstrapped_data = raw_data_biomarker[bootstrapped_sample,]

    total_responders = length(which(bootstrapped_data[, ncol(bootstrapped_data)] == "Responders"))
    total_nonresponders = length(which(bootstrapped_data[, ncol(bootstrapped_data)] == "Non-responders"))
    
    if(quantity_sign == "Greater than"){
      vp_selected = which(bootstrapped_data[, index_tmp] >= quantity_threshold) # row no/patient no
    } else {
      vp_selected = which(bootstrapped_data[, index_tmp] < quantity_threshold)
    }
    
    index_res = which(bootstrapped_data[vp_selected, ncol(bootstrapped_data)] == "Responders")
    index_nonres = which(bootstrapped_data[vp_selected, ncol(bootstrapped_data)] == "Non-responders")
    
    n_tt = length(index_res) + length(index_nonres)
    
    frac_resp = length(index_res) / total_responders
    frac_nonresp = length(index_nonres) / total_nonresponders
    ris_score = frac_resp - frac_nonresp
    resp_prob_score = length(index_res) / (n_tt) # response probability within subset
    
    tmp = c(timept,
            1,
            quantity_name,
            quantity_threshold,
            quantity_sign,
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


