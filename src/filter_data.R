filter_data = function(compartment, rf_sel_names,
                       read_table_train, read_table_test, annotations_tmp, idx_keep, consider_SD_as_responder, Prefiltering) {
  
  if(Prefiltering == "MLbased"){
      if (compartment != "all") { # filter only relevant biomarkers
        index_selected = c(which(annotations_tmp$Source == compartment), nrow(annotations_tmp))
        
        annotations_tmp = annotations_tmp[index_selected, ]
        read_table_train <- read_table_train[, index_selected]
        read_table_test <- read_table_test[, index_selected]
      }
      
      idx_keep=c(idx_keep, nrow(annotations_tmp))
  }
  
  read_table_train <- read_table_train[, idx_keep]
  read_table_test <- read_table_test[, idx_keep]
  annotations_tmp <- annotations_tmp[idx_keep,]
  
  # rename "CR/PR" and "SD" as "Responders"
  # and "PD" as "Non-responders"
  
  read_table_train = read_table_train %>%
    mutate(Response = replace(Response, Response == "CR/PR" | (Response == "SD" & consider_SD_as_responder) , "Responders")) %>%
    mutate(Response = replace(Response, Response == "PD" | (Response == "SD" & !consider_SD_as_responder) , "Non-responders")) %>%
    filter(Response != "NP")

  read_table_test = read_table_test %>%
    mutate(Response = replace(Response, Response == "CR/PR" | (Response == "SD" & consider_SD_as_responder) , "Responders")) %>%
    mutate(Response = replace(Response, Response == "PD" | (Response == "SD" & !consider_SD_as_responder) , "Non-responders")) %>%
    filter(Response != "NP")
  
  if(Prefiltering == "MLbased"){
      if(!setequal(annotations_tmp[-nrow(annotations_tmp), 1],rf_sel_names)){
        print("Error: annotations don't match with ML selected features")
      }
  }
  
  list_vars <- list(read_table_train, read_table_test, annotations_tmp)
  return(list_vars)
  
}


