#' Return temperature response
#' 
#' Applies constan temperatures to chill / heat model and returns temperature response.
#' 
#' The output of the function is used in the temperature response plots, but it can be
#' handy to create those manually, especially when comparing temperature responses of
#' several cultivars or different set of parameters of same cultivar.
#' 
#' 
#' @param par traditional model parameters of PhenoFlex in the order yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope (default)
#' or the new model parameters in the order yc, zc, s1, Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope. In the latter case the
#' the argument 'par_type' should be set equal to TRUE
#' @param temp_values numeric, vector containing the temperatures for which the temperature responses should be calculated
#' @return data.frame with the columns "Temperature", "Chill_response" and "Heat_response"
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @import chillR
#' 
#' @export get_temp_response_df

get_temp_response_plot <- function(par, 
                                   temp_values){
  

  temp_response <- data.frame(
      Temperature = temp_values,
      Chill_response = gen_bell(par, temp_values),
      Heat_response = GDH_response(temp_values, par)
    )

  return(temp_response)
}




gen_bell <- function(par, temp_values = seq(-5, 20, 0.1)) {
  E0 <- par[5]
  E1 <- par[6]
  A0 <- par[7]
  A1 <- par[8]
  Tf <- par[9]
  slope <- par[12]
  
  y <- c()
  for (i in seq_along(temp_values)) {
    y[i] <- apply_const_temp(
      temp = temp_values[i],
      A0 = A0,
      A1 = A1,
      E0 = E0,
      E1 = E1,
      Tf = Tf,
      slope = slope
    )
  }
  return(invisible(y))
}


GDH_response <- function(T, par)
{
  Tb <- par[11]
  Tu <- par[4]
  Tc <- par[10]
  GDH_weight <- rep(0, length(T))
  GDH_weight[which(T >= Tb & T <= Tu)] <-
    1 / 2 * (1 + cos(pi + pi * (T[which(T >= Tb &
                                          T <= Tu)] - Tb) / (Tu - Tb)))
  GDH_weight[which(T > Tu & T <= Tc)] <-
    (1 + cos(pi / 2 + pi / 2 * (T[which(T >  Tu &
                                          T <= Tc)] - Tu) / (Tc - Tu)))
  return(GDH_weight)
}


apply_const_temp <- function(temp,
                             A0,
                             A1,
                             E0,
                             E1,
                             Tf,
                             slope,
                             portions = 1200,
                             deg_celsius = TRUE){
  temp_vector <- rep(temp, times = portions)
  res <- chillR::DynModel_driver(
    temp = temp_vector,
    A0 = A0,
    A1 = A1,
    E0 = E0,
    E1 = E1,
    Tf = Tf,
    slope = slope,
    deg_celsius = deg_celsius
  )
  return(res$y[length(res$y)])
}
