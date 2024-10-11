calc_metrics <- function(raw_data, 
             index,
             b1, # biomarker loop
             a, # threshold loop
             min_VPs,
             annotations_tmp,
             threshold,
             condition_str,
             tt_responders,
             tt_nonresponders,
             final_tmp,
             final_score_tmp,
             predictive,
             consider_SD_as_responder){
  
  index_crpr = which(raw_data[index, ncol(raw_data)] == "CR/PR")
  index_sd = which(raw_data[index, ncol(raw_data)] == "SD")
  index_pd = which(raw_data[index, ncol(raw_data)] == "PD")
  
  n_crpr = length(index_crpr)
  n_sd = length(index_sd)
  n_pd = length(index_pd)
  
  # total number of patients within the subgroup
  n_tt = n_crpr + n_sd + n_pd
  
  # fraction of responders within the subgroup (or response probability)
  if(consider_SD_as_responder) {
    frac_resp_subset = (n_crpr + n_sd) / n_tt
  } else {frac_resp_subset = n_crpr / n_tt}
  
  if(predictive == "negative"){
      frac_resp_subset = n_pd / n_tt
  }
  
  frac_resp_orr = n_crpr / n_tt # Objective Response rate; SD excluded
  
  if (n_tt >= min_VPs) {
    
    tmp=cbind(rep(threshold[a],3), c(n_crpr, n_sd, n_pd) / n_tt,
              c("CR/PR", "SD", "PD"), rep(condition_str, 3))
    
    final_tmp = rbind(final_tmp, tmp)
    
    if(consider_SD_as_responder){
      frac_resp_cohort = (n_sd + n_crpr) / tt_responders
      frac_nonresp_cohort = (n_pd / tt_nonresponders)
    }else{
      frac_resp_cohort = (n_crpr) / tt_responders
      frac_nonresp_cohort = (n_sd + n_pd) / tt_nonresponders
    }
    
    # RIS or responder inclusion score
    ris = frac_resp_cohort - frac_nonresp_cohort
    
    if(predictive == "negative"){
        ris = frac_nonresp_cohort - frac_resp_cohort
    }
    
    final_score_tmp = rbind(final_score_tmp, cbind(annotations_tmp$Source[b1],
                                           annotations_tmp$Name[b1],
                                           condition_str,
                                           threshold[a],
                                           frac_resp_subset,
                                           ris,
                                           frac_resp_cohort,
                                           frac_nonresp_cohort,
                                           frac_resp_orr,
                                           annotations_tmp$Unit[b1]))

  }
  
  list_vars=list(final_tmp, final_score_tmp)
  return(list_vars)
  
}

