# function to generate VP subsets based on biomarker levels
subset_vp <- function(raw_data, 
                      VP_subsets, 
                      b1, 
                      response_final, 
                      response_final_score, 
                      ris_final,
                      ris_final_score,
                      annotations_tmp, 
                      min_VPs,
                      tt_responders,
                      tt_nonresponders,
                      method_threshold,
                      response_train_data,
                      ris_train_data,
                      consider_SD_as_responder,
                      predictive,
                      train_or_test) {
  
  if(train_or_test == "train"){
    
    # based on method to identify thresholds: uniform, quantiles
    if(method_threshold == "quantiles"){
      threshold <- unname(quantile(raw_data[, b1], probs = seq(from = 0, to = 1, length.out = VP_subsets), na.rm = TRUE)) 
    }else{
      # equally spaced cutoffs to subset virtual patients
      min_val <- min(raw_data[, b1], na.rm = TRUE)
      max_val <- max(raw_data[, b1], na.rm = TRUE)
      threshold <- seq(from = min_val, to = max_val, length.out = VP_subsets)
    }
    
    for (a in 1:length(threshold)) {
      
      for(q in 1:2){
        
        # q=1, subset patients with biomarker levels greater than threshold
        # q=2, less than to threshold
        
        if (q == 1) { 
          index = which(raw_data[, b1] >= threshold[a])
          condition_str = "Greater than"
        } else {
          index = which(raw_data[, b1] < threshold[a])
          condition_str = "Less than"
        }
        
        if (length(index) > 0) {
          
          # calculate RIS, response prob, .. for the subset
          calc_metrics_output_resp <- calc_metrics(raw_data, 
                                              index,
                                              b1, # biomarker loop
                                              a, # threshold loop
                                              min_VPs,
                                              annotations_tmp,
                                              threshold,
                                              condition_str,
                                              tt_responders,
                                              tt_nonresponders,
                                              response_final,
                                              response_final_score,
                                              predictive,
                                              consider_SD_as_responder)
          
          response_final = calc_metrics_output_resp[[1]]
          response_final_score = calc_metrics_output_resp[[2]]
          
          ris_final = response_final
          ris_final_score = response_final_score
          
        }
        
      }
      
    }
    
  } else { # validation
      
    # read threshold from training results
    response_index_train = which(response_train_data$quantity == annotations_tmp$Name[b1])
    ris_index_train = which(ris_train_data$quantity == annotations_tmp$Name[b1])
    
    response_threshold = response_train_data$cutoff[response_index_train]
    ris_threshold = ris_train_data$cutoff[ris_index_train]
    
    response_sign = response_train_data$sign[response_index_train]
    ris_sign = ris_train_data$sign[ris_index_train]
    
    if(response_sign == "Greater than"){
      response_index = which(raw_data[, b1] >= response_threshold)
    }else{response_index = which(raw_data[, b1] < response_threshold)}
    
    if(ris_sign == "Greater than"){
      ris_index = which(raw_data[, b1] >= ris_threshold)
    }else{ris_index = which(raw_data[, b1] < ris_threshold)}
    
    # calculate RIS, response prob, .. for the subset
    calc_metrics_output_resp <- calc_metrics(raw_data, 
                                        response_index,
                                        b1, # biomarker loop
                                        1, # threshold loop
                                        min_VPs,
                                        annotations_tmp,
                                        response_threshold,
                                        response_sign,
                                        tt_responders,
                                        tt_nonresponders,
                                        response_final,
                                        response_final_score,
                                        predictive,
                                        consider_SD_as_responder)
    
    calc_metrics_output_ris <- calc_metrics(raw_data, 
                                             ris_index,
                                             b1, # biomarker loop
                                             1, # threshold loop
                                             min_VPs,
                                             annotations_tmp,
                                             ris_threshold,
                                             ris_sign,
                                             tt_responders,
                                             tt_nonresponders,
                                             ris_final,
                                             ris_final_score,
                                             predictive,
                                             consider_SD_as_responder)
    
    response_final = calc_metrics_output_resp[[1]]
    response_final_score = calc_metrics_output_resp[[2]]
    
    ris_final = calc_metrics_output_ris[[1]]
    ris_final_score = calc_metrics_output_ris[[2]]
    
  }

  list_vars=list(response_final, response_final_score,
                 ris_final, ris_final_score)
  return(list_vars)
  
}
  

  


  


