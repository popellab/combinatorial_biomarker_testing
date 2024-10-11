# sorts the data based on response probability or ris & generates figures
plotting <- function(response_biomarker_summary, 
                     ris_biomarker_summary,
                     train_or_test,
                     method_threshold, 
                     VP_nsubsets,
                     timept_var_tmp,predictive){
  
  # hard coded variables
  ris_col_number = 6
  response_col_number = 5
  
  ylab_text="Response probability"
  if(predictive=="negative"){
      ylab_text="1 - Response probability"
  }
  
  # sort based on RIS
  ris_summary_sorted <- ris_biomarker_summary[order(as.numeric(ris_biomarker_summary[, ris_col_number]), decreasing = TRUE),] 
  #write.csv(ris_summary_sorted, paste0('output_files/', method_threshold, VP_nsubsets, '_ris_summary_', train_or_test, '.csv'))
  
  # sort based on response rates within the group
  response_summary_sorted <- response_biomarker_summary[order(as.numeric(response_biomarker_summary[, response_col_number]), decreasing = TRUE),] 
  #write.csv(response_summary_sorted, paste0('output_files/', method_threshold, VP_nsubsets, '_response_summary_',train_or_test, '.csv'))
  
  # filter the entry with best cutoff for each biomarker
  ris_filtered = ris_summary_sorted
  response_filtered = response_summary_sorted
  uniq_names = unique(ris_summary_sorted[, 2])
  
  for(i in 1:length(uniq_names)){
    
    ind_ris = which(ris_filtered[, 2] == uniq_names[i])
    if (length(ind_ris)>1) {ris_filtered = ris_filtered[-ind_ris[2:length(ind_ris)],]}
    
    ind_response = which(response_filtered[,2] == uniq_names[i])
    if (length(ind_response)>1) {response_filtered = response_filtered[-ind_response[2:length(ind_response)],]}
    
  }
  
  write.csv(ris_filtered, paste0('output_files/', timept_var_tmp, '/single_ris_', train_or_test, '.csv'), row.names=FALSE)
  write.csv(response_filtered, paste0('output_files/', timept_var_tmp, '/single_response_', train_or_test, '.csv'), row.names=FALSE)
  
  # backup
  ris_filtered_complete = ris_filtered
  response_filtered_complete = response_filtered
  
  ris_filtered = ris_filtered[1:15,]
  response_filtered = response_filtered[1:15,]
  position = position_dodge(width = .75)
  
  suffix_fignames = paste0('_', train_or_test, '_', method_threshold, VP_nsubsets, '.png')
  
  # barplot for scores
  plot <-  ggplot(ris_filtered) +
    geom_bar_pattern(aes(x = reorder(as.factor(ris_filtered[, 2]), as.numeric(ris_filtered[, 6])),
                         y = as.numeric(ris_filtered[, 6]),
                         pattern = as.factor(ris_filtered[, 3])),
                     stat = "identity",
                     #color = "white",
                     fill = "yellow3",
                     pattern_fill = "grey",
                     pattern_density = 0.05,
                     pattern_key_scale_factor = 0.6,
                     pattern_spacing = 0.01,
                     position = position,
                     alpha = 0.95) +
   # geom_errorbar(data = score_filtered, aes(x = reorder(as.factor(score_filtered[, 2]), -as.numeric(score_filtered[, 6])),
    #                                         ymin = as.numeric(score_filtered[, 16]),
     #                                        ymax = as.numeric(score_filtered[, 17])),
      #            color = "grey",
       #           width = 0.4) +
    xlab(paste0(" ")) +
    ylab("RIS") + 
    theme_bw() + 
    theme(#axis.ticks.x = element_text(size = 15), 
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 10),
        #  strip.text.x = element_text(size = 6),
          legend.position = "none")+ 
    guides(fill = guide_legend(title = "Response status")) +
    scale_fill_manual(values = c("#DB72FB", "#00C19F", "#00BA38")) +
    scale_pattern_manual(values = c("none", "none")) +
    #geom_text(aes(x = reorder(as.factor(ris_filtered[, 2]), -as.numeric(ris_filtered[, 6])),
     #             y = as.numeric(ris_filtered[, 6]),
      #            label = as.factor(ris_filtered[, 2])),
       #       size = 2.7,
        #      angle = 90,
         #     hjust = 1.1,
          #    position = position) +
    scale_y_continuous(limits = c(0,1.09), expand = c(0,0))+
    coord_flip()
  
  png(paste0('output_files/figures/score', suffix_fignames), width = 6, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  # response
  plot <-  ggplot(response_filtered) +
    geom_bar_pattern(aes(x = reorder(as.factor(response_filtered[, 2]), as.numeric(response_filtered[, 5])),
                         y = as.numeric(response_filtered[, 5]),
                         pattern = as.factor(response_filtered[, 3])),
                     stat = "identity",
                    # color = "white",
                     fill = "cornflowerblue",
                     pattern_fill = "grey",
                     pattern_density = 0.05,
                     pattern_key_scale_factor = 0.6,
                     pattern_spacing = 0.01,
                     position = position,
                     alpha = 0.95) +
    #geom_errorbar(data = response_filtered,
     #             aes(x = reorder(as.factor(response_filtered[, 2]), -as.numeric(response_filtered[, 5])),
      #                ymin = as.numeric(response_filtered[, 11]),
       #               ymax = as.numeric(response_filtered[, 12])),
        #          color = "grey",
         #         width = 0.4) +
    xlab(paste0(" ")) +
    ylab(ylab_text) + 
    theme_bw() + 
    theme(#axis.ticks.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 10),
          #strip.text.x = element_text(size = 6),
          legend.position = "none") + 
    guides(fill = guide_legend(title = "Response status")) +
    scale_fill_manual(values = c("#DB72FB", "#00C19F", "#00BA38")) +
    scale_pattern_manual(values = c("none", "none")) +
    #geom_text(aes(x = reorder(as.factor(response_filtered[, 2]), -as.numeric(response_filtered[, 5])),
     #             y = as.numeric(response_filtered[, 5]),
       #           label = as.factor(response_filtered[, 2])),
        #      size = 2.7,
         #     angle = 90,
          #    hjust = 1.10,
           #   position = position)+
    scale_y_continuous(limits = c(0,1.09), expand = c(0, 0))+
    coord_flip()#+
   # scale_y_continuous(expand = c(0, 0))
  
  png(paste0('output_files/figures/response', suffix_fignames), width = 6.5, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  # grouped bar plots
  ris_grouped = NULL
  data_resp = cbind(ris_filtered[, 2:7], rep("Responders", length(ris_filtered[, 7])))
  #data_resp = cbind(data_resp, ris_filtered[, 18:19])
  data_nonresp = cbind(ris_filtered[, 2:6], ris_filtered[, 8], 
                       rep("Non responders", length(ris_filtered[, 7])))
  #data_nonresp = cbind(data_nonresp, score_filtered[, 20:21])
  colnames(data_resp) = c("Quantity",
                          "Greater_or_less_than",
                          "Value",
                          "Response",
                          "score",
                          "fraction_resp_nonresp",
                          "Response_status"
                          #"CI_min",
                          #"CI_max"
                          )
  colnames(data_nonresp) = colnames(data_resp)
  ris_grouped = rbind(data_resp, data_nonresp)
  ris_grouped = data.frame(ris_grouped)
  
  
  plot <-  ggplot(ris_grouped, aes(x = reorder(as.factor(ris_grouped[, 1]), as.numeric(ris_grouped[, 5])),
                                     y = as.numeric(ris_grouped[, 6]),
                                     fill = as.factor(ris_grouped[, 7]))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    #geom_errorbar(data = score_grouped, aes(x = reorder(as.factor(score_grouped[, 1]), -as.numeric(score_grouped[, 5])),
     #                                       ymin = as.numeric(score_grouped[, 8]),
      #                                      ymax = as.numeric(score_grouped[, 9])),
       #           width = 0.3,
        #          position = position_dodge(0.9),
         #         color = "black") +
    xlab(paste0(" ")) +
    ylab("Fraction of (non)responders") + 
    theme_bw() + 
    theme(axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15, angle = 0),
          axis.text.y = element_text(size = 10),
         # strip.text.x = element_text(size = 6),
          legend.position = "none") + 
    guides(fill = guide_legend(title = "Response status")) +
    scale_fill_manual(values = c("#DB72FB", "#00C19F", "#00BA38")) +
    scale_y_continuous(limits=c(0,1))+
    coord_flip()
  
  png(paste0('output_files/figures/score_frac_resp_nonresp', suffix_fignames), width = 6, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  response_grouped = NULL
  data_resp = cbind(response_filtered[, 2:7], rep("Responders", length(response_filtered[, 7])))
  #data_resp = cbind(data_resp, response_filtered[, 18:19])
  data_nonresp = cbind(response_filtered[, 2:6],
                       response_filtered[, 8],
                       rep("Non responders", length(response_filtered[, 7])))
  #data_nonresp = cbind(data_nonresp, response_filtered[, 20:21])
  
  colnames(data_resp) = c("Quantity",
                          "Greater_or_less_than",
                          "Value",
                          "Response",
                          "score",
                          "fraction_resp_nonresp",
                          "Response_status"
                         # "CI_min",
                         # "CI_max"
                         )
  
  colnames(data_nonresp) = colnames(data_resp)
  response_grouped = rbind(data_resp,data_nonresp)
  response_grouped = data.frame(response_grouped)
  
  plot <-  ggplot(response_grouped, aes(x = reorder(as.factor(response_grouped[, 1]), as.numeric(response_grouped[, 4])),
                                        y = as.numeric(response_grouped[, 6]),
                                        fill = as.factor(response_grouped[, 7]))) +
    geom_bar(stat = "identity",
             #color = "white",
             position = position_dodge())+
    #geom_errorbar(data = response_grouped, aes(x = reorder(as.factor(response_grouped[, 1]), -as.numeric(response_grouped[, 4])),
     #                                          ymin = as.numeric(response_grouped[, 8]),
      #                                         ymax = as.numeric(response_grouped[, 9])),
       #           width = 0.3,
        #          position = position_dodge(0.9),
         #         color = "black") +
    xlab(paste0(" ")) +
    ylab("Fraction of (non)responders") + 
    theme_bw()+ 
    theme(axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15, angle = 0),
          axis.text.y = element_text(size = 10),
          #strip.text.x = element_text(size = 6),
          legend.position = "none")+ 
    guides(fill = guide_legend(title = "Response status")) +
    scale_fill_manual(values = c("#DB72FB", "#00C19F", "#00BA38")) +
    scale_y_continuous(limits=c(0,1))+
    coord_flip()
  
  png(paste0('output_files/figures/response_frac_resp_nonresp', suffix_fignames), width = 6, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  
}

