#' Calculate bloom timewindow based on thermal risks
#' 
#' Calculates risks for frost and heat and calculates upper and lower day of the year with viable flowering periods for each species and location.
#' 
#' @param weather_list_obs list of daily weather observations, which need to contain at least the columns Date, Year, Month, Day, Tmin and Tmax. Names of list elements need to
#' correspond to the names supplied in the observation_df. Based on this list, the risk thresholds are calculated tolerated by each species
#' @param weather_list_pred same structure as weather_list_obs. Based on this input the actual day ranges for each species and location are calculated. This
#' allows for variable ranges with expected changes in weather. If NULL, then the weather_list_ob is used for that, too
#' @param observation_df data.frame containing the phenological observations. Need to contain the columns location (with the same names used as the named elements of weather_list_obs) and the phenological
#' observations in Date format
#' @param frost_threshold numeric, by default 0. Decided the temperature in degree centigrade for which the lower temperature risk gets calculated.
#' @param heat_threshold numeric, by default 32. Decides the temperature in degree centigrade for which the higher temperature risk gets calculated.
#' @param target_col_obs character, by default 'flowering_f50'. Specifies the column name of observation_df where the phenological records are stored.
#' @param run_mean_window numeric, by default 10. Decides the window length of the running mean, which is used to smooth the calculated risks.
#' @param padding numeric, by default 0.05. Specifies how much extra risk toleance is added to the calculated risk thresholds (between 0 and 1)
#' @return data.frame containing the species, location, upper day of the year and lower day of the year for the bloom timewindow
#' @details there are several assumptions in the calculation of time windows. Lower day of the year cannot be lower than 1. Upper end is assumed to be before the maximum heat risk. If 
#' calculated heat tolerance of a species exceeds the maximum risk of a location, then the upper end of the bloom window is assumed to be the peak of heat risk.
#' @author Lars Caspersen
#' @keywords utility
#' 
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarise
#' @importFrom tidyr expand_grid
#' @importFrom lubridate yday
#' @importFrom stats na.omit
#' @importFrom dplyr filter
#' 
#' @export get_thermal_window_phenology

#input:
# weather_list_obs:   list of observed weather which corresponds to the phenology data.frame (observation_df)
#                     one element for one weather station. each element is data.frame of minimum and maximum temperature for the whole observation period
#                     needs a column called Date
# weather_list_pred:  list of weather, for which the time windows should be applied to (the ones which are also used to make phenological predictions)
#                     can be the same as weather_list_obs
#                     also contains daily Tmin and Tmax
#                     needs a colum called date
# observation_df:     data.frame which contains the phenological records
#                     phenological records should be in date.format
#                     needs column called species and location (location names need to be identical with element names of weather_list_obs and weather_list_pred)
# frost_threshold:    numeric, temperature in degree C for which the frost risk should be calculated
# heat_threshold:     numeric, temperature in degree C for whioch the heat risk should be calculated. 
#                     when defining the temperature window, it is always assumed that the heat risk is "left" to the maximum heat risk for the location (which works only for northern latitudes)
# target_col_obs:     character, name of the column in which the phenological records are stored in the observation_df
# run_mean_window:    numeric, by default 10. when calculating frost and heat risk, the individual risks per day of the year are smoothed by calculating the running mean
#                     this argument defines the window width of the running mean
#                     
# output:             data.frame containing the upper and lower boundary of thermal time window (in day of the year)
#                     has columns for species, location, flowering_type (same as input argument target_col_obs) and upper and lower for the day of the year thresholds
#

get_thermal_window_phenology <- function(weather_list_obs,
                                         weather_list_pred = NULL,
                                         observation_df, 
                                         frost_threshold = 0, 
                                         heat_threshold = 32,
                                         target_col_obs = 'flowering_f50',
                                         run_mean_window = 10,
                                         padding = 0.05){
  
  if(is.null(weather_list_pred)){
    weather_list_pred <- weather_list_obs
  }
  
  #species to do the analysis for
  species <- unique(observation_df$species)
  
  
  #check if DATE is present, if so make it to Date
  weather_list_obs <- purrr::map(weather_list_obs, function(x){
    if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
      
      #find out which version is present
      i <- which(tolower(colnames(x)) == 'date')[1]
      
      
      x$Date <- x[,i]
      
      return(x)
    } else if('date' %in% tolower(colnames(x)) == FALSE){
      stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
    } else { 
      return(x)}
  })
  
  #check if DATE is present, if so make it to Date
  weather_list_pred <- purrr::map(weather_list_pred, function(x){
    if('date' %in% tolower(colnames(x)) & ('Date' %in% colnames(x)) == FALSE){
      
      #find out which version is present
      i <- which(tolower(colnames(x)) == 'date')[1]
      
      
      x$Date <- x[,i]
      
      return(x)
    } else if('date' %in% tolower(colnames(x)) == FALSE){
      stop('weather_list_obs does not contain a column which is called "Date", "DATE" or written in a different case. All weather data need a date column for the function to work')
    } else { 
      return(x)}
  })
  
  
  #-------------------------------------------------------------------------#
  # establish risks of the phenology data and the corrosponding weather data
  #-------------------------------------------------------------------------#
  
  
  
  #get frost and heat risk for each day of the year
  frost_risk <- purrr::map(weather_list_obs, function(x){
    x %>% 
      dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
      dplyr::group_by(.data$doy) %>% 
      dplyr::summarise(chance_frost = sum(.data$Tmin <= frost_threshold) / n()) %>% 
      dplyr::mutate(run_mean_frost = chillR::runn_mean(.data$chance_frost, runn_mean = run_mean_window))
  }) %>% 
    dplyr::bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  
  #heat risk for each day of the year
  heat_risk <- purrr::map(weather_list_obs, function(x){
    x %>% 
      dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
      dplyr::group_by(.data$doy) %>% 
      dplyr::summarise(chance_heat = sum(.data$Tmax >= heat_threshold) / n()) %>% 
      dplyr::mutate(run_mean_heat = chillR::runn_mean(.data$chance_heat, runn_mean = run_mean_window))
  }) %>% 
    dplyr::bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  
  #save the target column in a different column, which I can access by name
  observation_df$target_pheno <- observation_df[, target_col_obs]
  
  
  #summarize the phenology data, know for each day of the year how many flowering records were measured for each species and location
  flower_sum <- observation_df %>% 
    dplyr::mutate(doy_pheno = lubridate::yday(.data$target_pheno)) %>% 
    dplyr::group_by(.data$species, .data$location, .data$doy_pheno) %>% 
    dplyr::summarise(n_flower = n())
  
  
  
  max_frost <- observation_df %>%
    dplyr::mutate(doy_pheno = lubridate::yday(.data$target_pheno)) %>%
    dplyr::select(.data$species, .data$location, .data$cultivar, .data$flowering_f50, .data$doy_pheno) %>%
    merge(frost_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
    stats::na.omit() %>%
    dplyr::group_by(.data$species) %>%
    dplyr::summarise(max_risk_frost = max(.data$run_mean_frost))

  max_heat <- observation_df %>%
    dplyr::mutate(doy_pheno = lubridate::yday(.data$target_pheno)) %>%
    dplyr::select(.data$species, .data$location, .data$cultivar, .data$flowering_f50, .data$doy_pheno) %>%
    merge(heat_risk, by.x = c('species', 'location', 'doy_pheno'), by.y = c('species', 'location', 'doy'), all = TRUE) %>%
    stats::na.omit() %>%
    dplyr::group_by(.data$species) %>%
    dplyr::summarise(max_risk_heat = max(.data$run_mean_heat))
  
  observation_df$target_doy <- lubridate::yday(observation_df$target_pheno)




  #get maximum frost risk accepted per species
  #
  #assign the calculated frost risk for each day for which we had flowering data
  #group by species (so have different locations in the same group) and calculate the maximum frost risk associated with flowering for the species in the dataset
  #say that this is the maximum frost tolerated
  #
  #problem: this leads to very narrow windows, because lower end is very strict if you have only data from tunisia or morocco
  #         how can I reduce the "exclusivity" of the lower boundary? round values? add an extra tolerance of 5%?
  #
  
  frost_limit <- frost_risk %>% 
    dplyr::group_by(.data$species) %>% 
    merge(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
    stats::na.omit() %>% 
    dplyr::group_by(.data$species) %>% 
    dplyr::summarise(max_frost_risk = max(.data$run_mean_frost)) %>% 
    dplyr::mutate(flowering_type = .data$target_col_obs,
           max_frost_risk_padded = .data$max_frost_risk + padding)
  
  
  heat_limit <- heat_risk %>% 
    dplyr::group_by(.data$species) %>% 
    merge(flower_sum, by.y = c('location', 'doy_pheno', 'species'), by.x = c('location', 'doy', 'species'), all.x = TRUE) %>% 
    stats::na.omit() %>% 
    dplyr::group_by(.data$species) %>% 
    dplyr::summarise(max_heat_risk = max(.data$run_mean_heat)) %>% 
    dplyr::mutate(flowering_type = .data$target_col_obs,
           max_heat_risk_padded = .data$max_heat_risk + padding)
  
  
  #combine upper and lower risk limit
  #data.frame for each species saying what is the maximum frost and heat risk tolerated
  limits_df <-  merge(frost_limit, heat_limit, by = c('species', 'flowering_type'))
  
  
  
  
  #---------------------------------------------------------------------------#
  #calculate window for the weahter data set used for phenological predictions
  #---------------------------------------------------------------------------#
  
  
  #calculate frost and heat risk for the other weather data
  
  #get frost and heat risk for each day of the year
  frost_risk <- purrr::map(weather_list_pred, function(x){
    x %>% 
      dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
      dplyr::group_by(.data$doy) %>% 
      dplyr::summarise(chance_frost = sum(.data$Tmin <= frost_threshold) / n()) %>% 
      dplyr::mutate(run_mean_frost = chillR::runn_mean(.data$chance_frost, runn_mean = run_mean_window))
  }) %>% 
    dplyr::bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  #heat risk for each day of the year
  heat_risk <- purrr::map(weather_list_pred, function(x){
    x %>% 
      dplyr::mutate(doy = lubridate::yday(.data$Date)) %>% 
      dplyr::group_by(.data$doy) %>% 
      dplyr::summarise(chance_heat = sum(.data$Tmax >= heat_threshold) / n()) %>% 
      dplyr::mutate(run_mean_heat = chillR::runn_mean(.data$chance_heat, runn_mean = run_mean_window))
  }) %>% 
    dplyr::bind_rows(.id = 'location') %>% 
    tidyr::expand_grid(species = species)
  
  
  
  #translate the risk into boundaries in terms of doy
  lower <- frost_risk %>% 
    merge(limits_df, by = c('species')) %>% 
    dplyr::filter(.data$run_mean_frost <=  .data$max_frost_risk) %>% 
    dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
    dplyr::summarise(min_doy = min(.data$doy))
  
  #padded
  lower_padded <- frost_risk %>% 
    merge(limits_df, by = c('species')) %>% 
    dplyr::filter(.data$run_mean_frost <=  .data$max_frost_risk_padded) %>% 
    dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
    dplyr::summarise(min_doy_padded = min(.data$doy))
  
  
  
  
  #only take values left of the maximum heat risk
  max_heat_risk <- heat_risk %>% 
    dplyr::group_by(location, .drop = FALSE) %>% 
    dplyr::summarise(max_risk = max(.data$run_mean_heat))
  
  doy_max_risk_df <- merge(heat_risk, max_heat_risk, by = 'location') %>% 
    dplyr::group_by(.data$location) %>% 
    dplyr::filter(.data$run_mean_heat == .data$max_risk) %>% 
    dplyr::summarise(doy_max_risk = min(.data$doy))
  
  upper <- heat_risk %>% 
    merge(doy_max_risk_df, by = 'location') %>% 
    dplyr::filter(.data$doy <= .data$doy_max_risk) %>% 
    merge(limits_df, by = c('species')) %>% 
    dplyr::filter(.data$run_mean_heat <= .data$max_heat_risk) %>% 
    dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
    dplyr::summarise(max_doy = max(.data$doy))
  
  upper_padded <- heat_risk %>% 
    merge(doy_max_risk_df, by = 'location') %>% 
    dplyr::filter(.data$doy <= .data$doy_max_risk) %>% 
    merge(limits_df, by = c('species')) %>% 
    dplyr::filter(.data$run_mean_heat <= .data$max_heat_risk_padded) %>% 
    dplyr::group_by(.data$location, .data$species, .data$flowering_type) %>% 
    dplyr::summarise(max_doy_padded = max(.data$doy))
  
  
  upper <- merge(upper, upper_padded, by = c('location', 'species', 'flowering_type'))
  lower <- merge(lower, lower_padded, by = c('location', 'species', 'flowering_type'))
  
  
  thermal_time_window_final <- merge.data.frame(lower, upper, by = c('location', 'species', 'flowering_type'))
  

  return(thermal_time_window_final)
  
}