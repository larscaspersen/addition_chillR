return_weights <- function(predicted, confidence, n_fail = 5, max_weight = NULL){
  
  apply(predicted, MARGIN = 1, function(x){
    
    #create intermediate object which I can manipulate
    conf <- confidence
    
    if(sum(is.na(x)) != 0){
      #position of NA
      pos.na <- which(is.na(x))
      
      #calculate replace the confidence at position of NA with zero
      conf[pos.na] <- 0
    }
    
    
    conf_out <- rep(0, length(conf))
    #calculate the relative weight of confidence
    if(sum(conf) !=0) conf_out <- conf / sum(conf)
    
    #in case we restrict the maximum weight
    if(is.null(max_weight) == FALSE & all(conf == 0) == FALSE){
      
      #position where confidence exceeds max value
      pos_toohigh <- conf_out > max_weight
      
      #check if the remaining entries have any confidences (are not NA)
      #if so, skip the routine and keep weights as they are
      if(sum(conf_out[pos_toohigh == FALSE]) != 0){
        
        #calculate how much confidence to allocate to remaining entries
        exceed_conf <- ifelse(conf_out > max_weight, yes = conf_out - max_weight, no = 0) %>% sum()
        
        
        #distribute exceed confidence
        weight_rest_relative <- conf_out / sum(conf_out[pos_toohigh == FALSE])
        weight_rest_relative[pos_toohigh] <- 0
        
        extra_conf <- weight_rest_relative * exceed_conf
        conf_out[pos_toohigh] <- max_weight
        conf_out <- conf_out + extra_conf
      }
      
    }
    #make sure the sum is always 1 (usual case), 0 (when all predictions are NA or max_weight)
    stopifnot(round(sum(conf_out), digits = 2) %in% c(1, 0, max_weight))
    
    return(conf_out)
  }) %>% 
    t() %>% 
    return()
}