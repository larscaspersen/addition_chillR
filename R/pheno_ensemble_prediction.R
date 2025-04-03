#'Get weighted average prediction of several PhenoFlex models
#'
#'Outdated
#'
#'This function takes the fitted models and temperature data and returns a weighted average prediction.
#'Weights are assigned using the confidence argument. 
#'The function assumes that theta_star and Tc are fixed. The 
#'
#' @param par_list the output of the phenology fitting done using MEIGO package
#' @param confidence numeric vector, containing the weights assign to the predictions. Assumes that higher value is more confidence. Values
#' get re-scaled by dividing by the sum of individual confidence scores. 
#' @param modelfn function that takes one set of parameters and one entry of temp as input and returns bloomdate
#' @param temp SeasonList for the prediction, contains the hourly weather data of the weather stations
#' @param return_se boolean, by default set TRUE. Decides if sd of prediction and individual ensemble members predictions are returned as well
#' @param ... further inputs for modelfn
#' @return By default returns a list with three components: predicted, sd and individual_pred. Predicted
#' contains the weighted predictions of the ensemble members. sd contains the unweighted 
#' standard deviation of the members predictions. Individual_pred contains the ensemble members individual predictions
#' 
#' @author Lars Caspersen
#' @keywords utility
#' 
#' @importFrom magrittr set_colnames
#' @importFrom reshape2 melt
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr pull
#' @importFrom stats sd
#' @importFrom dplyr bind_cols
#' 
#' @export pheno_ensemble_prediction

pheno_ensemble_prediction <- function(par_list, confidence, modelfn, temp, return_se = TRUE, ...){
  
  
  predicted <- purrr::map(par_list, function(x){
    par <- x$xbest
    
    return_predicted_days(convert_parameters(par), 
                          modelfn = modelfn, 
                          SeasonList =temp,
                          ...)
  }) %>% 
    stats::setNames(1:length(par_list)) %>% 
    dplyr::bind_cols() %>% 
    as.matrix()
  weights <- confidence / sum(confidence)
  
  weighted_pred <- as.vector(predicted %*% weights)
  
  sd_pred <- predicted %>% 
    t() %>% 
    as.data.frame() %>% 
    magrittr::set_colnames( 1:nrow(predicted)) %>% 
    reshape2::melt(id.vars = NULL) %>% 
    dplyr::group_by(.data$variable) %>% 
    dplyr::summarise(sd = stats::sd(.data$value)) %>% 
    dplyr::pull(.data$sd)
  #scale rpiq_values
  
  
  if(return_se){
    return(list(predicted = weighted_pred, sd = sd_pred,
                individual_pred = predicted))
  } else{
    return(weighted_pred)
  }
  
}