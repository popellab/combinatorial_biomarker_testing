evaluate_biomarkers <- function(raw_data, 
                                VP_nsubsets, 
                                min_VPs,
                                tt_responders,
                                tt_nonresponders,
                                annotations_filtered, 
                                method_threshold,
                                response_train_data,
                                ris_train_data,
                                consider_SD_as_responder,
                                predictive,
                                train_or_test){
              
  biomarker_summary_response = NULL
  biomarker_summary_ris = NULL
  
  # loop through every biomarker candidate
  for (b1 in 1:(ncol(raw_data) - 1)) {
    
    response_final = NULL
    response_final_score = NULL
    ris_final = NULL
    ris_final_score = NULL
    
    # generating virtual patient subsets
    subset_vp_output <- subset_vp(raw_data, 
                                  VP_nsubsets, 
                                  b1, 
                                  response_final, 
                                  response_final_score, 
                                  ris_final,
                                  ris_final_score,
                                  annotations_filtered, 
                                  min_VPs,
                                  tt_responders,
                                  tt_nonresponders,
                                  method_threshold,
                                  response_train_data,
                                  ris_train_data,
                                  consider_SD_as_responder,
                                  predictive,
                                  train_or_test)
    
    response_final = subset_vp_output[[1]]
    response_final_score = subset_vp_output[[2]]
    ris_final = subset_vp_output[[3]]
    ris_final_score = subset_vp_output[[4]]
    
    if (length(response_final) > 0) {
      response_final = data.frame(response_final) #not used
      response_final_score = data.frame(response_final_score)
      biomarker_summary_response = rbind(biomarker_summary_response, response_final_score)
    }
    
    if (length(ris_final) > 0) {
      ris_final = data.frame(ris_final) #not used
      ris_final_score = data.frame(ris_final_score)
      biomarker_summary_ris = rbind(biomarker_summary_ris, ris_final_score)
    }
    
    if(train_or_test == "train"){
      colnames(response_final_score) = c("compartment", "quantity", "sign", "cutoff", "response_probability", "ris",
                                         "fraction_responders","fraction_nonresponders", "ORR", "units")
      #write.csv(response_final_score, paste0('output_files/test_resp',b1,'.csv'))
      store_cutoffs(annotations_filtered$Name[b1], str_replace_all(annotations_filtered$Name[b1], "/", "__"), response_final_score)
    }
    
  } # end of for loop - biomarker cand.
  
  colname_labels <- c("compartment",
                      "quantity",
                      "sign",
                      "cutoff",
                      "response_probability",
                      "ris",
                      "fraction_responders",
                      "fraction_nonresponders",
                      "ORR",
                      "units")
  
  biomarker_summary_response = data.frame(biomarker_summary_response)
  colnames(biomarker_summary_response) = colname_labels
  
  biomarker_summary_ris = data.frame(biomarker_summary_ris)
  colnames(biomarker_summary_ris) = colname_labels
  
  list_vars=list(biomarker_summary_response, biomarker_summary_ris)
  return(list_vars)
  
  
}
