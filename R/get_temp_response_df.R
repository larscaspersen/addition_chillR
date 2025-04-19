#' Return temperature response
#' 
#' Applies constant temperatures to chill / heat model and returns temperature response.
#' 
#' The output of the function is used in the temperature response plots, but it can be
#' handy to create those manually, especially when comparing temperature responses of
#' several cultivars or different set of parameters of same cultivar.
#' 
#' 
#' @param par traditional model parameters of PhenoFlex in the order yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope 
#' @param temp_values numeric, vector containing the temperatures for which the temperature responses should be calculated
#' @return data.frame with the columns "Temperature", "Chill_response" and "Heat_response"
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @import chillR 
#' 
#' @export get_temp_response_df

get_temp_response_df <- function(par, 
                                   temp_values){
  

  temp_response <- data.frame(
      Temperature = temp_values,
      Chill_response = gen_bell(par, temp_values),
      Heat_response = GDH_response(temp_values, par)
    )

  return(temp_response)
}
