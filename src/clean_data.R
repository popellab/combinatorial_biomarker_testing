clean_data <- function(compartment,
                       rf_sel_names,
                       ntop_biomarkers,
                       consider_SD_as_responder,
                       data_train_tmp,
                       data_test_tmp,
                       annotations_tmp,
                       top_biomarkers_tmp,
                       Prefiltering){
  
  if (compartment != "all") { # filter only relevant biomarkers
    top_biomarkers_tmp = top_biomarkers_tmp[which(top_biomarkers_tmp$compartment == compartment), ]
  } 
  
  if(nrow(top_biomarkers_tmp)<ntop_biomarkers){
    top_single_biomarkers <- top_biomarkers_tmp
  }else{
    top_single_biomarkers <- top_biomarkers_tmp[1:ntop_biomarkers,]
  }
  
  # identify the biomarker indices to keep; anyway keep the last one: response status
  idx_keep = c(match(top_single_biomarkers$quantity, annotations_tmp$Name), nrow(annotations_tmp))

  filter_data_output <- filter_data(compartment, rf_sel_names, data_train_tmp, data_test_tmp, annotations_tmp, idx_keep, consider_SD_as_responder, Prefiltering)
  data_train_tmp = filter_data_output[[1]]
  data_test_tmp = filter_data_output[[2]]
  annotations_tmp = filter_data_output[[3]]
  
  list_vars <- list(data_train_tmp, data_test_tmp, annotations_tmp, top_single_biomarkers)
  return(list_vars)
  
}



