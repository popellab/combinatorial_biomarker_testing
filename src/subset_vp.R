subset_vp = function(n, 
                     comb_vec, 
                     comb_vec_threshold,
                     comb_vec_name,
                     comb_vec_sign,
                     read_table, 
                     annotations_tmp,
                     ncutoffs,
                     ncombo_tmp,
                     top_single_biomarkers, 
                     cutoff_method,
                     train_or_validate,
                     train_data,
                     nmin_patients_tmp,
                     predictive) {
  
  print(n)
  col_a <- integer(ncombo_tmp)
  name_cola <- character(ncombo_tmp)
  units_cola <- character(ncombo_tmp)
  sign_a <- character(ncombo_tmp)
  names_together <- NULL
  min_a <- NULL
  max_a <- NULL
  points_a <- NULL
  
  # total number of patients in data
  total_responders = length(which(read_table$Response == "Responders"))
  total_nonresponders = length(which(read_table$Response == "Non-responders"))

  col_a[1:ncombo_tmp] <- comb_vec[1:ncombo_tmp, n]
  units_cola[1:ncombo_tmp] <- annotations_tmp$Unit[comb_vec[1:ncombo_tmp, n]]
  
  if(train_or_validate == "train"){
  
    name_cola[1:ncombo_tmp] <- annotations_tmp$Name[comb_vec[1:ncombo_tmp, n]]
    
    # find the sign of these quantities - this is based on single biomarker analysis
    sign_a[1:ncombo_tmp] <- sapply(1:ncombo_tmp, function(x) top_single_biomarkers$sign[which(top_single_biomarkers$quantity == name_cola[x])])
    names_together <- paste(name_cola, collapse = " + ")
    
    for(j in 1:ncombo_tmp){

      #based on method to identify thresholds: uniform, quantiles
      if(cutoff_method == "quantiles"){
        points_a <- rbind(points_a, unname(quantile(read_table[, col_a[j]], probs = seq(from = 0, to = 1, length.out = ncutoffs), na.rm = TRUE)) )
        
      }else{
        min_a <- c(min_a, min(read_table[, col_a[j]], na.rm = TRUE))
        max_a <- c(max_a, max(read_table[, col_a[j]], na.rm = TRUE))
        points_a <- rbind(points_a, seq(from = min_a[j], to = max_a[j], length.out = ncutoffs))
      }
      
    }
      
  }else if(train_or_validate == "validate"){
    
    name_cola[1:ncombo_tmp] <- comb_vec_name[1:ncombo_tmp, n]
    sign_a[1:ncombo_tmp] <- comb_vec_sign[1:ncombo_tmp, n]
    names_together <- paste(name_cola, collapse = " + ")
    points_a <- t(t(comb_vec_threshold[1:ncombo_tmp, n])) # is this needed thein?
      
  }
  
  all_combos = do.call(expand.grid, split(points_a, rep(1:nrow(points_a), ncol(points_a))))
  #each column will be biomarker 1,2,3,...
  final <- NULL
  
  calc_metrics_output <- lapply(1:nrow(all_combos),
                                calc_metrics,
                                combos = all_combos,
                                read_table = read_table,
                                ncombo_tmp = ncombo_tmp,
                                sign_a = sign_a,
                                col_a = col_a,
                                name_cola = name_cola,
                                tt_responders = total_responders,
                                tt_nonresponders = total_nonresponders,
                                nmin_patients_tmp = nmin_patients_tmp,
                                names_together = names_together,
                                predictive)
  
  for(p1 in 1:length(calc_metrics_output)){
    final = rbind(final, calc_metrics_output[[p1]])
  }
  
  #final = data.frame(final)
  if(length(final)==0){return(final)}
  
    if (nrow(final) > 0) {
      
      str_biomarker_value = NULL
      str_biomarker_name = NULL
      str_biomarker_sign = NULL
      
      for(str_val in 1:ncol(all_combos)){
        str_biomarker_value = c(str_biomarker_value, paste0('biomarker_', str_val, '_cutoff'))
        str_biomarker_name = c(str_biomarker_name, paste0('biomarker_', str_val, '_name'))
        str_biomarker_sign = c(str_biomarker_sign, paste0('biomarker_', str_val, '_sign'))
      }
      
      colnames(final) <- c("quantity",
                           str_biomarker_name,
                           str_biomarker_sign,
                           str_biomarker_value,
                           "response_probability",
                           "ris",
                           "fraction_responders",
                           "fraction_nonresponders")
      
      final <- data.frame(final)
      final_ris <- final[order(as.numeric(final$ris), decreasing = TRUE),] 
      final_response <- final[order(as.numeric(final$response_probability), decreasing = TRUE),] 
      
      final <- data.frame(rbind(final_ris[1,], final_response[1,]))
      
      final <- apply(final,2,as.character) # not required if you dont convert to dataframe
     # write.csv(apply(final,2,as.character), paste0('test.csv'))

      
    }

  return(final)
  
}

