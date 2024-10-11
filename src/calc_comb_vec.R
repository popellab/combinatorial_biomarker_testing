# read the train file & get comb_vec for validation
calc_comb_vec <- function(train_file_tmp, ncombo, raw_data_tmp, annotations_tmp){
  
  comb_vec_index = NULL
  comb_vec_threshold = NULL
  comb_vec_name = NULL
  comb_vec_sign = NULL
 
  for(k2 in 1:ncombo){
      
      comb_vec_index = rbind(comb_vec_index, match(train_file_tmp[, k2], annotations_tmp$Name))

      comb_vec_name = rbind(comb_vec_name, train_file_tmp[, k2])
      comb_vec_sign = rbind(comb_vec_sign, train_file_tmp[, k2+ncombo])
      comb_vec_threshold = rbind(comb_vec_threshold, train_file_tmp[, k2+ncombo+ncombo])
    
  } 
               
  vars_list = list(as.matrix(comb_vec_index), as.matrix(comb_vec_threshold), 
                   as.matrix(comb_vec_name), as.matrix(comb_vec_sign))
  return(vars_list)
  
}
