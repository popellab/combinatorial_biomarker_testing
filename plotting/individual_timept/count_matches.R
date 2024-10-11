# counts number of matches between two string vectors
count_matches <- function(vec_long, vec_short){
  
  # substitute "(, ), +" with "zyx"
  patterns_to_replace <- c("(", ")", "+")

  for(patterns in 1:length(patterns_to_replace)){
    vec_long=str_replace_all(vec_long, fixed(patterns_to_replace[patterns]), "zyx")
    vec_short=str_replace_all(vec_short, fixed(patterns_to_replace[patterns]), "zyx")

  }
  
  count=0
  for(jc in 1:length(vec_short)){
    count = count + sum(grepl(paste0('^',vec_short[jc],'$'), vec_long))
  }
  
  return(count)
  
}

