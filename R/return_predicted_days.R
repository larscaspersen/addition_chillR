#' Returns predicted bloomdays for a list of temperature time series and model parameters
#' 
#' This is a convenience function which allows to get the predicted bloom dates for a
#' set of model parameters and temperature observations. Arguments are identical
#' to \code(\link{evaluation_function_meigo}), except that the argument bloomJDays is
#' not needed
#' 
#' 
#' 
#' @param par traditional model parameters of PhenoFlex in the order yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope.
#' @param modelfn function used within the evaluation function to calculate the actual bloomday, often we use
#' the 'custom_GDH_wrapper' function for that
#' @param SeasonList list of hourly temperatures for the individual phenological seasons. Each element should contain a data.frame
#' with the columns "Temp" (for the hourly temperature) and "JDay" for the corresponding Julian day. Is usually
#' generated using \code(\link{chillR::genSeasonList})
#' @param na_penalty numeric, value which is used when the model fails to generate a prediction
#' for the bloom date. By default 365
#' @return numeric vector with the predicted bloom dates
#' 
#' @author Lars Caspersen
#' @keywords utility
#' 
#' @export return_predicted_days

return_predicted_days <- function(par, 
                                  modelfn,
                                  SeasonList,
                                  na_penalty = 365){
  
  #innput:
  #         x is the parameters in meigo
  #         modelfn is the function used to calculate the bloomdays
  #         SeasonList contains the weather data for the different years
  #         na_penalty is the value used if the model failed to predict any day with the current set of parameters for a particular year
  
  #output: inequality constraints g
  #        model performance value F
  
  #change the name of the parameters, dont know if necessary
  par 
  
  #calculate the predicted flower dates
  pred_bloom <- unlist(lapply(X = SeasonList, FUN = modelfn, par = par))
  
  #if the model returns no bloom day, then give penalty term instead
  pred_bloom <- ifelse(is.na(pred_bloom), yes = na_penalty, no = pred_bloom)
  
  return(pred_bloom)
  
}