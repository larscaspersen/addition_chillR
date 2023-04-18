#' Draws temperature response plot from PhenoFlex parameters for several cultivars
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
#' @param performance_df data.frame with at least the columns 'cultivar' and columns with the parameters name 
#' @param weather_list list with hourly weather observations, needed to create the histogram with the temperature
#' frequency
#' @param temp_values numeric, vector containing the temperatures for which the temperature responses should be calculated
#' @param legend.pos character, by default 'bottom'. Specifies where the legend goes. Can take the same values as in ggplot2
#' @param col_palette character vector, contains the hexcode of the colors used to draw the temperature responses for each cultivar
#' @param hourtemps data.frame containing hourly temperature, by default set to NULL. If supplied, the columns
#' 'Month' and 'Temp' should be present.
#' @param par_names character vector, specifies the names of the parameters, by default the 'new' PhenoFlex parameters
#' c('yc', 'zc', 's1', 'Tu', 'theta_star', 'theta_c', 'tau', 'pie_c','Tf', 'Tc', 'Tb', 'slope') 
#' @param chill_months numeric vector, indicating for which months the frequency of observed temperature should be calculated for 
#' the chill temperature response. By default set to c(11:12,1:3)
#' @param heat_months numeric vector, indicating for which months the frequency of observed temperature should be calculated for 
#' the heat temperature response. By default set to c(1:5)
#' @return ggplot of the mdoelled temperature response
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @import ggplot2 chillR dplyr patchwork graphics
#' @importFrom magrittr "%>%"
#' 
#' @export gen_combined_temp_response_plot

gen_combined_temp_response_plot <- function(performance_df,  
                                            weather_list, 
                                            temps = seq(-5, 50, by = 0.1),
                                            legend.pos = 'bottom', 
                                            col_palette = c("#E69F00", "#56B4E9", "#009E73",
                                                            "#F0E442", "#0072B2", "#D55E00",
                                                            "#CC79A7", "#999999"),
                                            par_names = c('yc', 'zc', 's1', 'Tu', 'theta_star', 'theta_c', 'tau', 'pie_c',
                                                          'Tf', 'Tc', 'Tb', 'slope'),
                                            chill_months = c(11:12,1:2),
                                            heat_months = 1:3){
  
  temp_response_df <- data.frame()
  for(i in 1:nrow(performance_df)){
    
    par <- unlist(performance_df[i,par_names])
    # if(performance_df$cultivar[i] == 'Mission'){
    #   par[12] <- 1.6
    #   par[9] <- 2
    # }
    
    temp_response_df <- rbind(temp_response_df,
                              data.frame(cultivar = performance_df$cultivar[i],
                                         get_temp_response_df(convert_parameters(par),
                                                              temp_values = temps))
    )
  }
  
  
  chill_temp_obs <- weather_list %>% 
    bind_rows() %>% 
    filter(Month %in% chill_months) %>% 
    summarise(density = hist(Temp, breaks = 30, plot = FALSE)$density,
              count = hist(Temp, breaks = 30, plot = FALSE)$counts,
              Temperature = hist(Temp, breaks = 30, plot = FALSE)$mids) %>% 
    mutate(Chill_response = density / max(density,na.rm = TRUE))
  
  heat_temp_obs <- weather_list %>% 
    bind_rows() %>%  
    filter(.data$Month %in% heat_months) %>% 
    summarise(density = hist(.data$Temp, breaks = 30, plot = FALSE)$density,
              count = hist(.data$Temp, breaks = 30, plot = FALSE)$counts,
              Temperature = hist(.data$Temp, breaks = 30, plot = FALSE)$mids) %>% 
    mutate(Heat_response = .data$density / max(.data$density,na.rm = TRUE))
  
  density_df <- merge.data.frame(chill_temp_obs[,c('Temperature', 'Chill_response')], heat_temp_obs[,c('Temperature', 'Heat_response')],
                                 by = 'Temperature', all = TRUE)
  
  density_df_long <- reshape2::melt(density_df, id.vars = 'Temperature', value.name = 'density')  
  
  #merge temperature frequency and temperature response data
  melted_response <- reshape2::melt(temp_response_df, id.vars = c('cultivar', 'Temperature'))
  melted_response <- merge.data.frame(melted_response, density_df_long, by.x = c('Temperature', 'variable'), all.x = TRUE)
  
  melted_response[melted_response$variable == 'Chill_response',]$density <- melted_response[melted_response$variable == 'Chill_response',]$density * max(melted_response[melted_response$variable == 'Chill_response',]$value)
  
  
  
  chill_plot <- melted_response %>% 
    filter(variable == 'Chill_response') %>% 
    ggplot(aes(x = Temperature)) +
    geom_bar(data = melted_response[melted_response$variable == 'Chill_response' & melted_response$cultivar == performance_df$cultivar[1],],
             stat = 'identity', aes(x = Temperature, y = density, fill = 'Observed Temperatures'),
             width = 2) +
    geom_line(size = 2, aes(col = cultivar, y = value)) +
    ylab("Temperature response\n(arbitrary units)") +
    xlab("Temperature (°C)") +
    scale_fill_manual(values = 'grey80', breaks = 'Observed Temperatures')+
    facet_wrap(vars(variable),
               labeller = labeller(variable = c(
                 Chill_response = c("Chill response")
               ))) +
    
    
    scale_color_manual(values = col_palette[1:nrow(performance_df)]) +
    xlim(-5, 20)+
    #annotate('text', label = 'A', x = -5, y = 33, size = 5)+
    theme_bw(base_size = 15)+
    theme(legend.title=element_blank()) 
  
  heat_plot <- melted_response %>% 
    filter(variable == 'Heat_response') %>% 
    ggplot(aes(x = Temperature)) +
    geom_bar(data = melted_response[melted_response$variable == 'Heat_response' & melted_response$cultivar == performance_df$cultivar[1],],
             stat = 'identity', aes(x = Temperature, y = density, fill = 'Observed Temperatures'),
             width = 2) +
    geom_line(size = 2, aes(col = cultivar, y = value)) +
    ylab("") +
    xlab("Temperature (°C)") +
    scale_fill_manual(values = 'grey80', breaks = 'Observed Temperatures')+
    facet_wrap(vars(variable),
               labeller = labeller(variable = c(
                 Heat_response = c("Heat response")
               ))) +
    scale_color_manual(values = col_palette[1:nrow(performance_df)]) +
    xlim(0, 50)+
    #annotate('text', label ='B', x = 0, y = 1, size = 5)+
    theme_bw(base_size = 15)+
    theme(legend.title=element_blank())
  
  p <- (chill_plot + heat_plot) +  plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'A') &
    theme(legend.position=legend.pos) 
  
  return(p)
}