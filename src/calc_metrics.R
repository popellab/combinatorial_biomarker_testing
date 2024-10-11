calc_metrics <- function(a, 
                         combos,
                         read_table,
                         ncombo_tmp,
                         sign_a,
                         col_a,
                         name_cola,
                         tt_responders,
                         tt_nonresponders,
                         nmin_patients_tmp,
                         names_together,
                         predictive){
  
  tmp = NULL
  var = 1
  index = c(1:nrow(read_table))
  
  while (var <= ncombo_tmp) {
    
    if(sign_a[var] == "Greater than"){
      index_tmp = which(read_table[, col_a[var]] >= combos[a, var])
    } else {
      index_tmp = which(read_table[, col_a[var]] < combos[a, var])
    }
    
   if(var == 1){index = index_tmp} else {index = intersect(index_tmp, index)}
    var = var + 1
    
  }
  
  if (length(index) > 0) {
    
    index_res = which(read_table[index, ncol(read_table)] == "Responders")
    index_nonres = which(read_table[index, ncol(read_table)] == "Non-responders")
    
    n_tt = length(index_res) + length(index_nonres)

     if (n_tt >= nmin_patients_tmp) {
      
      frac_resp = length(index_res) / tt_responders
      frac_nonresp = length(index_nonres) / tt_nonresponders
      
      if(predictive == "negative"){
          resp_prob_score = length(index_nonres) / (n_tt) # negative predictive biomarkers
          ris_score = frac_nonresp - frac_resp # negative predictive biomarkers
      }else{
          resp_prob_score = length(index_res) / (n_tt) # response probability within subset
          ris_score = frac_resp - frac_nonresp
      }
      
      tmp = c(names_together,
              name_cola,
              sign_a,
              combos[a, ],
              resp_prob_score,
              ris_score, 
              frac_resp,
              frac_nonresp)

      
    }
    
  }
  
  return(tmp)
  
}

