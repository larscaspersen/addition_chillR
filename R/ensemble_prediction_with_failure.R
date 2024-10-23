#' Make weighted mean prediction from scratch
#' 
#' Takes the parameters used in  \link[chillR]{phenologyFitter} and brings it to
#' the format used in \code{\link{evaluation_function_meigo_nonlinear}}.
#' 
#' @param par_list list of the parameters entries contain one read of the read with the function 
#' LarsChill::load_fitting_result(), each entry is a repetition of the same cultivar
#' parameters are extracted from the 'xbest' entry within the individual list members
#' I assumed ten model parameters with theta_star and Tc being fixed, but you can change that in the code after the line par <- x$xbest
#' order of entries is identical with order in confidence (see next input)
#' @param confidence gives the weights to the individual predictions
#' numeric vector of same length as the par_list
#' assumes that bigger is better
#' @param temp seasonlist containing hourly temperature data for the predictions
#' generated with chillR::genSeasonList description
#' @param theta_star fixed value for theta_star
#' @param Tc fixed value for Tc
#' @param return_se logical, decides if standard deviation of the predictions around the weighted mean should be returned as well
#' @param n_fail numeric, decides the cut-off number of failure predictions of the weighted mean members so that the weighted mean also returns a failure
#' in case n_fail = 5: if we have 4 or less failure predictions --> get ignored and the weighted mean is calculated based on remaining results
#' if we have 5 or more failure predictions --> weighted mean is failure as well
#' @param max_weight by default NULL, when number between 0 and 1 supplied, it expresses how much
#' weight an individual prediction can get. Can prevent that one prediction dominates all the 
#' remaining ones because the confidence score may be inflated
#' @returns  when return_se = TRUE --> list with weighted mean, sd and individual model predictions
#' when return_se = FALSE --> vector with weighted means
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom nleqslv nleqslv
#' @export ensemble_prediction_with_failure
ensemble_prediction_with_failure <- function(par_list, confidence, temp, theta_star = 279, Tc = 36, return_se = TRUE, n_fail = 5,
                                             max_weight = NULL){
  
  
  predicted <- purrr::map(par_list, function(x){
    par <- x$xbest
    par <- c(par[1:4], theta_star, par[5:8], Tc, par[9:10])
    
    return_predicted_days(convert_parameters(par), 
                          modelfn = custom_PhenoFlex_GDHwrapper, 
                          SeasonList =temp,
                          na_penalty = NA)
  }) %>% 
    stats::setNames(1:length(par_list)) %>% 
    dplyr::bind_cols() %>% 
    as.matrix()
  
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
  predicted_tmp <- predicted %>% replace(is.na(.), 0)
  
  #calculate weighted individual pred, then get the sum
  weighted_pred <- predicted_tmp * weights 
  pred_out <- rowSums(weighted_pred)
  
  #in case too many models indicated NA, then the whole prediction becomes NA
  pred_out[pred_na] <- NA
  
  
  
  if(return_se){
    return(list(predicted = pred_out, sd = sd_pred,
                individual_pred = predicted))
  } else{
    return(weighted_pred)
  }
  
}