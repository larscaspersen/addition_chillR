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
#' @param temp_values numeric, vector containing the temperatures for which the temperature responses should be calcualted
#' @param par_type character, by default 'old'. If set to 'new' than the supplied parameters should contain theta_star, theta_c, tau and pie_c. These
#' get converted to E0, E1, A0 and A1 in an intermediate step using the \code(\link{nleqslv}) package
#' @param log_A boolean, by default FALSE. If set TRUE, then the parameters A1 and A1 are assumed to be supplied 
#' log_transformed and get converted back before calculating the temperature responses
#' with the columns "Temp" (for the hourly temperature) and "JDay" for the corresponding Julian day. Is usually
#' generated using \code(\link{chillR::genSeasonList})
#' @param hourtemps data.frame containing hourly temperature, by default set to NULL. If supplied, the columns
#' 'Month' and 'Temp' should be present.
#' @param chill_months numeric vector, indicating for which months the frequency of observed temperature should be calculated for 
#' the chill temperature response. By default set to c(11:12,1:3)
#' @param heat_months numeric vector, indicating for which months the frequency of observed temperature should be calculated for 
#' the heat temperature response. By default set to c(1:5)
#' @param type numeric, decides which type of plot should be returned. By default set to 0, which returns a
#' simple temperarture response plot. If set equals to 1 or 2 hourtemps needs to be supplied. In case of type = ,frequency of observed temperature is
#' indicated in the plot with a color gradient. If type = 2 the temperature frequency is indicated with a histogram.
#' @return ggplot of the mdoelled temperature response
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @import ggplot2 chillR dplyr
#' @importFrom magrittr "%>%"
#' 
#' @export get_temp_response_plot

get_temp_response_plot <- function(par, temp_values,
                                   par_type = 'old',
                                   log_A = FALSE, 
                                   hourtemps = NULL, chill_months = c(11:12,1:2),
                                   heat_months = 1:5, type = 0){
  
  if(par_type == 'new'){
    params<-numeric(4)
    
    params[1] <- par[5]   #theta*
    params[2] <- par[6]    #theta_c
    params[3] <- par[7]    #Tau(thetha*)
    params[4] <- par[8]     #pi_c
    
    
    output<-nleqslv::nleqslv(c(500, 15000), solve_nle, jac=NULL, params, xscalm="auto", method="Newton",
                    control=list(trace=0,allowSingular=TRUE))
    
    
    #This is a numerical method which can produce non-convergence. Check this
    if (output$termcd >= 3){
      #if the nle algorithm has stalled just discard this solution
      E0<-NA; E1<-NA; A0<-NA; A1<-NA
      error('Could not find corresponding values of E0, E1, A0 and A1')
      
      #You would add here a flag to let your optimization procedure know
      #That this solution should be ignored by lack of convergence
      
    } else {
      
      par[5] <- E0 <- output$x[1]
      par[6] <- E1 <- output$x[2]
      
      #A1 and A0 can be calculated through Equations 36 and 37
      
      q=1/params[1]-1/params[2]
      
      par[8] <- A1 <- -exp(E1/params[1])/params[3]*log(1-exp((E0-E1)*q))
      par[7] <- A1*exp((E0-E1)/params[2])
    }
    
  } else {
    if(log_A){
      par[7]<-exp(par[7])
      par[8]<-exp(par[8])
    }
  }
  
  
  
  
  if(is.null(hourtemps) == FALSE){
    
    #maybe get 30 bins?
    n_bins <- 30
    
    chill_temp_obs <- hourtemps %>% 
      filter(Month %in% chill_months) %>% 
      summarise(density = hist(Temp, breaks = n_bins, plot = FALSE)$density,
                count = hist(Temp, breaks = n_bins, plot = FALSE)$counts,
                Temperature = hist(Temp, breaks = n_bins, plot = FALSE)$mids) %>% 
      mutate(Chill_response = density / max(density,na.rm = TRUE))
    
    heat_temp_obs <- hourtemps %>% 
      filter(Month %in% heat_months) %>% 
      summarise(density = hist(Temp, breaks = n_bins, plot = FALSE)$density,
                count = hist(Temp, breaks = n_bins, plot = FALSE)$counts,
                Temperature = hist(Temp, breaks = n_bins, plot = FALSE)$mids) %>% 
      mutate(Heat_response = density / max(density,na.rm = TRUE))
    
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
    
    
    #color gradient for density of observations
    
    if(type == 1){
      p1 <- melted_response %>% 
        filter(variable == 'Chill_response') %>% 
        ggplot(aes(x = Temperature, y = value, color = density)) +
        geom_line(size = 2) +
        ylab("Temperature response (arbitrary units)") +
        xlab("Temperature (°C)") +
        facet_wrap(vars(variable),
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
        filter(variable == 'Heat_response') %>% 
        ggplot(aes(x = Temperature, y = value, color = density)) +
        geom_line(size = 2) +
        xlab("Temperature (°C)") +
        ylab('') +
        facet_wrap(vars(variable),
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
      
      p3 <- ggplot(data.frame(l = xlab, x = 1, y = 20)) +
        geom_text(aes(x, y, label = l), size = 6) + 
        theme_void(base_size = 15) +
        coord_cartesian(clip = "off")+
        theme(plot.margin = margin(b = 0))
      
      library(patchwork)
      p <- (p1 + p2) / p3 +   plot_layout(guides = 'collect') +
        plot_layout(heights = c(25, 1))
    } else if(type == 2) {
      #transform the frequency data of chill, to make it fit to the scale (no need for heat because scale is always from 0 to 1)
      melted_response[melted_response$variable == 'Chill_response',]$density <- melted_response[melted_response$variable == 'Chill_response',]$density * max(melted_response[melted_response$variable == 'Chill_response',]$value)
      
      
      p <-melted_response %>% 
        ggplot(aes(x = Temperature, y = value)) +
        geom_bar(stat = 'identity', aes(x = Temperature, y = density, fill = 'frequency of temperature\nduring chilling / forcing')) +
        geom_line(size = 2, aes(col = variable)) +
        ylab("Temperature response (arbitrary units)") +
        xlab("Temperature (°C)") +
        scale_fill_manual(values = 'grey80', breaks = 'frequency of temperature\nduring chilling / forcing')+
        #sec.axis = sec_axis(~.*coeff, name="Price ($)"))
        #scale_y_continuous(sec.axis=sec_axis(~.*,name="Relative Frequency"))+
        facet_wrap(vars(variable),
                   scales = "free",
                   labeller = labeller(variable = c(
                     Chill_response = c("Chill response"),
                     Heat_response = c("Heat response")
                   ))) +
        scale_color_manual(values = c("Chill_response" = "blue", "Heat_response" = "red")) +
        theme_bw(base_size = 15)
    }
    
    
    
  } else {
    temp_response <- data.frame(
      Temperature = temp_values,
      Chill_response = gen_bell(par, temp_values),
      Heat_response = GDH_response(temp_values, par)
    )
    
    melted_response <- reshape2::melt(temp_response, id.vars = "Temperature")
    
    p <- ggplot(melted_response, aes(x = Temperature, y = value)) +
      geom_line(size = 2, aes(col = variable)) +
      ylab("Temperature response (arbitrary units)") +
      xlab("Temperature (°C)") +
      facet_wrap(vars(variable),
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