iterate_count_matches <- function(n, fulldata_vec_short, biomarker_list_tmp, value_tmp, metric_tmp){
  
  vec_short_data = str_split_1(fulldata_vec_short$quantity[n], fixed(" + "))
  
  if(count_matches(biomarker_list_tmp[[1]], vec_short_data) == length(vec_short_data)){
    biomarker_list_tmp[[length(biomarker_list_tmp)+1]] = vec_short_data
    if(metric_tmp == "response"){
      value_tmp[length(value_tmp)+1] = fulldata_vec_short$response_probability[n]
    }else{
      value_tmp[length(value_tmp)+1] = fulldata_vec_short$ris[n]
    }
  }
  
  return(list(biomarker_list_tmp, value_tmp))
  
}
