#' Process data.frame of predictions to weighted mean prediction
#' 
#' Helps to process already made predictions to weighted mean predictions. Has a routine how to handle predictions
#' of bloom failure. The function makes strong assumptions on the data frame of the predictions and the data frame 
#' containing the weights (confidence)
#' 
#' 
#' @param predicted data.frame with individual predictions, need to contain column called id
#' role of id column is to match predictions with the entries of the confidence input
#' so each cultivar would get their own id, can be the name, or something else
#' assume wide format --> so one column for each prediction
#' that means for one particular year we have ten columns with the ten predictions for that year (assuming you have ten repetitions)
#' function assumes that the names of the columns containing the predictions will be also present in the confidence input
#' so if the predictions are stored in columns R1 to R10, these column names need to be present in confidence, too.
#' @param confidence data.frame, needs to contain same columns as predicted (through predicted can have many more additional columns)
#' assume wide format, so 10 columns when ten repetitions, plus id
#' assumes that larger values mean more confidence
#' id entries need to match with id entries of the first input ('predicted')
#' @param return_se logical, decides if standard deviation of the predictions around the weighted mean should be returned as well
#' @param n_fail numeric, decides the cut-off number of failure predictions of the weighted mean members so that the weighted mean also returns a failure
#' in case n_fail = 5: if we have 4 or less failure predictions --> get ignored and the weighted mean is calculated based on remaining results
#' if we have 5 or more failure predictions --> weighted mean is failure as well
#' @param max_weight by default NULL, when number between 0 and 1 supplied, it expresses how much
#' weight an individual prediction can get. Can prevent that one prediction dominates all the 
#' remaining ones because the confidence score may be inflated
#' @param .progress logical, if set TRUE than process par appears indicating the remaining time until the function completes
#' @returns  same as 'predicted' but with additional column containing weighted mean and standard deviation
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr set_colnames
#' @importFrom reshape2 melt
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr pull
#' @importFrom stats sd
#' @export weighted_mean_with_fail
weighted_mean_with_fail <- function(predicted, confidence, return_se = TRUE, n_fail = 5, max_weight = NULL, .progress = FALSE){
  
  #check if the required colnames are present and that they match
  stopifnot('id' %in% colnames(confidence))
  stopifnot('id' %in% colnames(predicted))
  
  other_colnames <- colnames(confidence)[colnames(confidence) != 'id']
  stopifnot(all(other_colnames %in% colnames(predicted)))
  
  #save the old predicted
  predicted_old <- predicted
  confidence_old <- confidence
  
  #iterate other the different ids
  pred_out <- purrr::map(unique(predicted_old$id), function(i){
    
    #i <- unique(predicted_old$id)[2]
    
    #subset to the columns we need
    predicted <- predicted_old %>% 
      dplyr::filter(id == i) %>% 
      dplyr::select(all_of(other_colnames)) %>% 
      as.matrix()
    
    confidence <- confidence_old %>% 
      dplyr::filter(id == i) %>% 
      dplyr::select(all_of(other_colnames)) %>% 
      unlist() %>% 
      unname()
    
    #if as many as n_fail model runs indicate failure, then mark as NA, otherwise discard them and calculate mean of the remaining ones
    pred_na <- apply(predicted, MARGIN = 1, FUN = function(x) sum(is.na(x)) >= n_fail) 
    
    
    #create a weight data.frame with the same dimensions as predicted
    weights <- return_weights(predicted = predicted,
                              confidence = confidence, 
                              n_fail = n_fail, 
                              max_weight = max_weight)
    
    #calculate sd, excluding NA values
    sd_pred <- predicted %>% 
      t() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames( 1:nrow(predicted)) %>% 
      reshape2::melt(id.vars = NULL) %>% 
      dplyr::group_by(.data$variable) %>% 
      dplyr::summarise(sd = stats::sd(.data$value, na.rm = TRUE)) %>% 
      dplyr::pull(.data$sd)
    
    #replace predicted NA with 0s
    predicted_tmp <- predicted %>% replace(is.na(.data), 0)
    
    #calculate weighted individual pred, then get the sum
    weighted_pred <- predicted_tmp * weights 
    pred_out <- rowSums(weighted_pred)
    
    #in case too many models indicated NA, then the whole prediction becomes NA
    pred_out[pred_na] <- NA
    
    predicted_old %>% 
      filter(id == i) %>% 
      mutate(weighted_pred = pred_out,
             sd = sd_pred) %>% 
      return()
  }, .progress = .progress) %>% 
    bind_rows()
  
  if(return_se == FALSE){
    pred_out <- pred_out %>% 
      select(-sd)
  }
  
  return(pred_out)
}