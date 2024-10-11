# function to filter raw biomarker data
filter_data <- function (raw_data_temp, raw_data_temp2, annotations_temp, unused_vars) {
  
  # discard unused biomarker candidates
  if (length(unused_vars) > 0) {
    raw_data_temp <- raw_data_temp[, -unused_vars]
    raw_data_temp2 <- raw_data_temp2[, -unused_vars]
    annotations_temp <- annotations_temp[-unused_vars,]
  }
  
  # replace Inf in data by NA
  raw_data_temp <- do.call(data.frame,
                      lapply(raw_data_temp,
                             function(x) replace(x, is.infinite(x), NA)))
  raw_data_temp2 <- do.call(data.frame,
                      lapply(raw_data_temp2,
                             function(x) replace(x, is.infinite(x), NA)))
  
  list_vars = list(raw_data_temp, raw_data_temp2, annotations_temp)
  return(list_vars)
  
}
