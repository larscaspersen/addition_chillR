#'Get weighted average prediction of several PhenoFlex models
#'
#'This function takes the fitted models and temperature data and returns a weighted average prediction.
#'Weights are assigned using the confidence argument. 
#'The function assumes that theta_star and Tc are fixed. The 
#'
#' @param par_list the output of the phenology fitting done using MEIGO package
#' @param confidence numeric vector, contaning the weights assigne to the predictions. Assumes that higher value is more confidence. Values
#' get rescaled by deviding by the sum of individual confidence scores. 
#' @param temp SeasonList for the prediction, contains the hourly weather data of the weather stations
#' @param theta_star parameter of the chill accumulation model of PhenoFlex. Marks the optimal temperature in K for chill accumulation. By default 279 K
#' @param Tc parameter of the heat accumulation submodel of PhenoFlex. Marks the critical temperatur for heat accumulation, above which no heat gets accumulated. By default 36 degree C
#' @param return_se boolean, by default set TRUE. Decides if sd of prediction and individual ensemble memebers predictions are returned as well
#'
#' @return By default returns a list with three components: predicted, sd and individual_pred. Predicted
#' contains the weighted predictions of the ensemble members. sd contains the unweighted 
#' standard deviation of the memebers predictions. Individual_pred contains the ensemble members indivudal predictions
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


pheno_ensemble_prediction <- function(par_list, confidence, temp, theta_star = 279, Tc = 36, return_se = TRUE){
  
  
  predicted <- purrr::map(par_list, function(x){
    par <- x$xbest
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    return_predicted_days(convert_parameters(par), 
                          modelfn = custom_PhenoFlex_GDHwrapper, 
                          SeasonList =temp)
  }) %>% 
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