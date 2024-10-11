evaluate_biomarkers_combo <- function(metric_tmp, 
                                      compartment,
                                      ncombo,  
                                      top_single_biomarkers, 
                                      ntop_biomarkers,
                                      annotations_tmp, 
                                      consider_SD_as_responder, 
                                      data_tmp, 
                                      cutoff_method, 
                                      ncutoffs,
                                      ncores,
                                      train_or_validate,
                                      timept_var,
                                      nmin_patients_tmp,
                                      Prefiltering,
                                      predictive){
  
# print progress on the screen
display_progress(ncombo, compartment, metric_tmp, train_or_validate)

final_summary = NULL
      
if(train_or_validate == "train"){
  
  vec = 1:(ncol(data_tmp) - 1)
  comb_vec = combn(unique(vec), ncombo)
  
  subset_vp_output = mclapply(1:ncol(comb_vec), 
                              subset_vp, 
                              comb_vec = comb_vec, 
                              comb_vec_threshold = NULL,
                              comb_vec_name = NULL,
                              comb_vec_sign = NULL,
                              read_table = data_tmp, 
                              annotations_tmp = annotations_tmp,
                              ncutoffs = ncutoffs,
                              ncombo_tmp = ncombo,
                              top_single_biomarkers = top_single_biomarkers, 
                              cutoff_method = cutoff_method, 
                              train_or_validate = train_or_validate,
                              train_data = NULL,
                              nmin_patients_tmp = nmin_patients_tmp,
                              predictive,
                              mc.cores = ncores)
  
}else{ # validate
  
  read_train_file = read.csv(paste0('output_files/', timept_var, '/', compartment, ncombo, '_', metric_tmp,'_train.csv'))
  read_train_file = read_train_file[,c(-1)] # quantity
  
  calc_comb_vec_output <- calc_comb_vec(read_train_file, ncombo, data_tmp, annotations_tmp)
  comb_vec = calc_comb_vec_output[[1]]
  comb_vec_threshold = calc_comb_vec_output[[2]]
  comb_vec_name = calc_comb_vec_output[[3]]
  comb_vec_sign = calc_comb_vec_output[[4]]
    

  subset_vp_output = mclapply(1:ncol(comb_vec), 
                              subset_vp, 
                              comb_vec = comb_vec, 
                              comb_vec_threshold = comb_vec_threshold,
                              comb_vec_name = comb_vec_name,
                              comb_vec_sign = comb_vec_sign,
                              read_table = data_tmp, 
                              annotations_tmp = annotations_tmp,
                              ncutoffs = ncutoffs,
                              ncombo_tmp = ncombo,
                              top_single_biomarkers = NULL, 
                              cutoff_method = cutoff_method, 
                              train_or_validate = train_or_validate,
                              train_data = read_train_file,
                              nmin_patients_tmp = nmin_patients_tmp,
                              predictive,
                              mc.cores = ncores)
  
}
      
      
for(p in 1:length(subset_vp_output)){
  final_summary = rbind(final_summary, subset_vp_output[[p]])
}

final_summary = data.frame(final_summary)
final_summary_sort_ris <- final_summary[order(as.numeric(final_summary$ris), decreasing = TRUE),]
final_summary_sort_response <- final_summary[order(as.numeric(final_summary$response_probability), decreasing = TRUE),]

final_summary_sort_ris <- final_summary_sort_ris[!duplicated(final_summary_sort_ris$quantity),]
final_summary_sort_response <- final_summary_sort_response[!duplicated(final_summary_sort_response$quantity),]
     
if(Prefiltering == "MLbased" && train_or_validate == "train"){
    
    write.csv(final_summary_sort_ris, paste0('output_files/', timept_var, '/', compartment, ncombo, '_ris_',train_or_validate,'.csv'), row.names=FALSE)
    write.csv(final_summary_sort_response, paste0('output_files/', timept_var, '/', compartment, ncombo, '_response_',train_or_validate,'.csv'), row.names=FALSE)
    
    #apply(final_summary_sort_ris,2,as.character)

}else{
  if(metric_tmp == "ris"){
      write.csv(final_summary_sort_ris, paste0('output_files/', timept_var, '/', compartment, ncombo, '_ris_',train_or_validate,'.csv'), row.names=FALSE)
  } else {
    write.csv(final_summary_sort_response, paste0('output_files/', timept_var, '/', compartment, ncombo, '_response_',train_or_validate,'.csv'), row.names=FALSE)
  }
}
  
} 


  

