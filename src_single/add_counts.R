# this functions returns the total no. of VPs, responders and non-responders in the dataset
add_counts <- function(raw_data, 
                      consider_SD_as_responder) {

  # total no. of VPs
  tt_VPs = nrow(raw_data)
  
  if(consider_SD_as_responder){ #then consider SD as responders
    tt_responders = length(which(raw_data$Response == "CR/PR")) + length(which(raw_data$Response == "SD"))
    tt_nonresponders = length(which(raw_data$Response == "PD"))
  }else{ # then consider SD as non-responders
    tt_responders = length(which(raw_data$Response == "CR/PR")) 
    tt_nonresponders = length(which(raw_data$Response == "PD")) + length(which(raw_data$Response == "SD"))
  }
  
  list_vars=list(tt_VPs, tt_responders, tt_nonresponders)
  return(list_vars)

}
