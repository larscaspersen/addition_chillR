#' Function used in the optimization process to generate bloom data
#' 
#' This function takes the temperature data, the phenological observations and model parameters and returns
#' a model performance score. This score is used by the optimization algorithm to 
#' find better model parameters.
#' 
#' 
#' 
#' 
#' @param x traditional model parameters of PhenoFlex in the order yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope.
#' @param modelfn function used within the evaluation function to calculate the actual bloomday, often we use
#' the 'custom_GDH_wrapper' function for that
#' @param bloomJDays numeric containing the days of the year witht the observed bloom
#' @param SeasonList list of hourly temperatures for the individual phenological seasons. Each element should contain a data.frame
#' with the columns "Temp" (for the hourly temperature) and "JDay" for the corresponding Julian day. Is usually
#' generated using \link[chillR]{genSeasonList}
#' @param na_penalty numeric, value which is used when the model fails to generate a prediction
#' for the bloom date. By default 365
#' @return list with two elements. First is called 'f' and contains the residual sum of squares of the model. The 
#' second is 'g' which is the values of the additional model constraints defined in the function.
#' 
#' @author Lars Caspersen
#' @keywords utility
#'  
#' @export evaluation_function_meigo

evaluation_function_meigo <- function(x, 
                                      modelfn,
                                      bloomJDays,
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
  par <- x
  
  #calculate the predicted flower dates
  pred_bloom <- unlist(lapply(X = SeasonList, FUN = modelfn, par = par))
  
  #if the model returns no bloom day, then give penalty term instead
  pred_bloom <- ifelse(is.na(pred_bloom), yes = na_penalty, no = pred_bloom)
  
  #calculate the model performance value
  F <- sum((pred_bloom - bloomJDays)^2)
  
  
  
  #####
  #inequality constraints
  #####
  
  #this is the vector containing the values for the inequality constraints
  #at first initialize the vector
  g <- rep(0,5)
  
  
  #equality constraints should be always stated before inequality constraints, according to meigo vignette
  #(but we dont have any in this case)
  
  
  #inequality constraints are reformulated as differences
  
  #Tu >= Tb
  g[1] <- x[4] - x[11]
  #Tx >= Tb
  g[2] <- x[10] - x[11]
  #Tc >= Tu
  g[3] <- x[10] - x[4]
  
  
  #the q10 value should be between 1.5 and 3.5
  #q10 formation = exp((10*E0)/(T2-T1))
  #q10 destruction = exp((10*E1)/(T2-T1))
  #T1 = 297
  #T2 = 279
  #parameters T1 and T2 chosen after Egea 2020
  #ranges for q10 are provided in c_L and c_U
  g[4] <- exp((10 * x[5]) / (297 * 279))
  
  g[5] <- exp((10 * x[6]) / (297 * 279))
  
  
  return(list(F=F, g=g))

}