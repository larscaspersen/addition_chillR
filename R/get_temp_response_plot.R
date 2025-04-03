#' Draws temperature response plot from PhenoFlex parameters
#' 
#' Returns a plot showing the chilling and forcing reaction of the PhenoFlex
#' model parameters for certain temperatures.
#' 
#' Furthermore, it can highlight which temperatures where observed in the 
#' data set used to generate the model parameters. The function can handle
#' the common PhenoFlex parameters (which include E0, E1, A0 and A1) and the
#' "new" parameters (which include theta_star, theta_c, tau and pie_c instead of the
#' earlier mentioned parameters). 
#' 
#' 
#' @param par traditional model parameters of PhenoFlex in the order yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope (default)
#' or the new model parameters in the order yc, zc, s1, Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope. In the latter case the
#' the argument 'par_type' should be set equal to TRUE
#' @param temp_values numeric, vector containing the temperatures for which the temperature responses should be calculated
#' @param par_type character, by default 'old'. If set to 'new' than the supplied parameters should contain theta_star, theta_c, tau and pie_c. These
#' get converted to E0, E1, A0 and A1 in an intermediate step 
#' @param log_A boolean, by default FALSE. If set TRUE, then the parameters A1 and A1 are assumed to be supplied 
#' log_transformed and get converted back before calculating the temperature responses
#' with the columns "Temp" (for the hourly temperature) and "JDay" for the corresponding Julian day. Is usually
#' generated using \link[chillR]{genSeasonList}
#' @param hourtemps data.frame containing hourly temperature, by default set to NULL. If supplied, the columns
#' 'Month' and 'Temp' should be present.
#' @param chill_months numeric vector, indicating for which months the frequency of observed temperature should be calculated for 
#' the chill temperature response. By default set to c(11:12,1:3)
#' @param heat_months numeric vector, indicating for which months the frequency of observed temperature should be calculated for 
#' the heat temperature response. By default set to c(1:5)
#' @param weather_freq_plot character, only applicable when `hourtemps` is supplied. 
#' Decides how temperature observations should be represented in the temperature
#' response plot. By default = 'histogram', which includes a histogram for the hourly
#' temperature. Other option is 'gradient' which represents the frequency of observed temperature
#' intervals in form of color gradient in the temperature response plot. Darker
#' color indicates most frequent temperature intervals, bright color less frequent ones.
#' @return ggplot of the modeled temperature response
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @import ggplot2 chillR dplyr patchwork graphics
#' @importFrom magrittr "%>%"
#' 
#' @export get_temp_response_plot

get_temp_response_plot <- function(par, temp_values,
                                   par_type = 'old',
                                   log_A = FALSE, 
                                   hourtemps = NULL, 
                                   chill_months = c(11:12,1:2),
                                   heat_months = 1:5, 
                                   weather_freq_plot = 'histogram'){
  
  if(par_type == 'new')  par <- convert_parameters(par)
  
  if(is.null(hourtemps) == FALSE){
    
    
    if(weather_freq_plot == 'gradient'){
      n_bins <- 100
    } else if(weather_freq_plot == 'histogram'){
      n_bins <- 30
    }
    
    chill_temp_obs <- hourtemps %>% 
      filter(.data$Month %in% chill_months) %>% 
      summarise(density = hist(.data$Temp, breaks = n_bins, plot = FALSE)$density,
                count = hist(.data$Temp, breaks = n_bins, plot = FALSE)$counts,
                Temperature = hist(.data$Temp, breaks = n_bins, plot = FALSE)$mids) %>% 
      mutate(Chill_response = .data$density / max(.data$density,na.rm = TRUE))
    
    heat_temp_obs <- hourtemps %>% 
      filter(.data$Month %in% heat_months) %>% 
      summarise(density = hist(.data$Temp, breaks = n_bins, plot = FALSE)$density,
                count = hist(.data$Temp, breaks = n_bins, plot = FALSE)$counts,
                Temperature = hist(.data$Temp, breaks = n_bins, plot = FALSE)$mids) %>% 
      mutate(Heat_response = .data$density / max(.data$density,na.rm = TRUE))
    
    density_df <- merge.data.frame(chill_temp_obs[,c('Temperature', 'Chill_response')], heat_temp_obs[,c('Temperature', 'Heat_response')],
                                   by = 'Temperature', all = TRUE)
    
    density_df_long <- reshape2::melt(density_df, id.vars = 'Temperature', value.name = 'density')
    
    temp_response <- data.frame(
      Temperature = temp_values,
      Chill_response = gen_bell(par, temp_values),
      Heat_response = GDH_response(temp_values, par)
    )
    
    melted_response <- reshape2::melt(temp_response, id.vars = 'Temperature')
    melted_response <- merge(melted_response, density_df_long, by.x = c('Temperature', 'variable'))
    
    
    if(weather_freq_plot == 'gradient'){
      p1 <- melted_response %>% 
        filter(.data$variable == 'Chill_response') %>% 
        ggplot(aes(x = .data$Temperature, y = .data$value, color = .data$density)) +
        geom_line(size = 2) +
        ylab("Temperature response (arbitrary units)") +
        xlab("Temperature (\u00B0C)") +
        facet_wrap(vars(.data$variable),
                   scales = "free",
                   labeller = labeller(variable = c(
                     Chill_response = c("Chill response"),
                     Heat_response = c("Heat response")
                   ))) +
        scale_colour_distiller(palette = "PuBu",direction = 1, name = 'Relative Frequency\nof temperature')+
        #scale_colour_brewer(palette = "YlOrRd")+
        #scale_color_manual(values = c("Chill_response" = "blue", "Heat_response" = "red")) +
        theme_bw(base_size = 15) +
        theme(legend.position = "none")
      
      p2 <- melted_response %>% 
        filter(.data$variable == 'Heat_response') %>% 
        ggplot(aes(x = .data$Temperature, y = .data$value, color = .data$density)) +
        geom_line(size = 2) +
        xlab("Temperature (\u00B0C)") +
        ylab('') +
        facet_wrap(vars(.data$variable),
                   scales = "free",
                   labeller = labeller(variable = c(
                     Chill_response = c("Chill response"),
                     Heat_response = c("Heat response")
                   ))) +
        scale_colour_distiller(palette = "Reds",direction = 1, name = '')+
        #scale_color_manual(values = c("Chill_response" = "blue", "Heat_response" = "red")) +
        theme_bw(base_size = 15) +
        theme(legend.position = "none")
      
      xlab <- p1$labels$x
      p1$labels$x <- p2$labels$x <- " "
      
      
      p3 <- data.frame(l = xlab, x = 1, y = 20) %>% 
        ggplot() +
        geom_text(aes(x = .data$x, y = .data$y, label = .data$l), size = 6) + 
        theme_void(base_size = 15) +
        coord_cartesian(clip = "off")+
        theme(plot.margin = margin(b = 0))
      
      
      p <- (p1 + p2) / p3 +   plot_layout(guides = 'collect') +
        plot_layout(heights = c(25, 1))
    } else if(weather_freq_plot == 'histogram') {
      #transform the frequency data of chill, to make it fit to the scale (no need for heat because scale is always from 0 to 1)
      melted_response[melted_response$variable == 'Chill_response',]$density <- melted_response[melted_response$variable == 'Chill_response',]$density * max(melted_response[melted_response$variable == 'Chill_response',]$value)
      
      
      p <-melted_response %>% 
        ggplot(aes(x = .data$Temperature, y = .data$value)) +
        geom_bar(stat = 'identity', aes(x = .data$Temperature, y = .data$density, fill = 'observed weather'),
                 width = 2) +
        geom_line(size = 2, aes(col = .data$variable)) +
        ylab("Temperature response (arbitrary units)") +
        xlab("Temperature (\u00B0C)") +
        scale_fill_manual(values = 'grey80', breaks = 'observed weather')+
        #sec.axis = sec_axis(~.*coeff, name="Price ($)"))
        #scale_y_continuous(sec.axis=sec_axis(~.*,name="Relative Frequency"))+
        facet_wrap(vars(.data$variable),
                   scales = "free",
                   labeller = labeller(variable = c(
                     Chill_response = c("Chill response"),
                     Heat_response = c("Heat response")
                   ))) +
        scale_color_manual(values = c("Chill_response" = "blue", "Heat_response" = "red")) +
        theme_bw(base_size = 15)
    }
    
    
  #if no hourly temperature is supplied  
  } else {
    temp_response <- data.frame(
      Temperature = temp_values,
      Chill_response = gen_bell(par, temp_values),
      Heat_response = GDH_response(temp_values, par)
    )
    
    melted_response <- reshape2::melt(temp_response, id.vars = "Temperature")
    
    p <- melted_response %>% 
      ggplot(aes(x = .data$Temperature, y = .data$value)) +
      geom_line(size = 2, aes(col = .data$variable)) +
      ylab("Temperature response (arbitrary units)") +
      xlab("Temperature (\u00B0C)") +
      facet_wrap(vars(.data$variable),
                 scales = "free",
                 labeller = labeller(variable = c(
                   Chill_response = c("Chill response"),
                   Heat_response = c("Heat response")
                 ))) +
      scale_color_manual(values = c("Chill_response" = "blue", "Heat_response" = "red")) +
      theme_bw(base_size = 15) +
      theme(legend.position = "none")
  }
  
  
  
  
  
  return(p)
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
