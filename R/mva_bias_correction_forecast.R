#' Perform mean and variance adjustment (MVA) bias correction
#' 
#' Calculates mean biases and standard deviation for forecast data and local observed temperature
#' data. Performs bias correction on monthly means, then uses the ratio of corrected and uncorrected means
#' to scale the daily and sub-daily observations. Follows procedure outlined by: Manazanas et al. (2019) and
#' Leung et al. (1999). Returns a data.frame with corrected and original forecast data.
#' 
#' Bias correction does three steps for each month of the year:
#' * Calculate monthly means and sd for observed and forecasted data
#' * Correct forecasted monthly mean with the formula: (mean_fcst_m_t - mean_fcst) x (sd_obs / sd_fcst) + mean_obs
#' * Scale (sub) daily observation using share of corrected and uncorrected forecast mean
#' 
#' mean_fcst_m_t = monthly mean of forecast member m, at year t
#' mean_fcst = monthly mean of all forecast members at all years
#' sd_obs = standard deviation of observed monthly means across all years
#' df_fcst = standard deviation of monthly means across all members m, across all years
#' mean_obs = monthly mean of observed temperature, across all years
#' 
#' Important note: as the function corrects using a factor, it is sensitive to sub-zero values. The function 
#' carries the mean correction out in Kelvin behin the scenes. 
#' 
#' @param observed data.frame, contains observed temperature. Is usually the same format as 
#' when downloaded via chillR package. Should contain columns: Year, Month, Day, and a column with temperature. Valid options are:
#' Tmean, Temp, or combination of Tmin and Tmax.
#' 
#' @param predicted data.frame containing the forecast data. Usually the same format as when running the
#' extract_seaonal_forecast() function. Should contain columns: Year, Month, Day, Temp, unit
#' 
#' @param unit_observed, character, by default "C". Indicates the unit of the input temperature. Bias correction is done in Kelvin.
#' To indicate Fahrenheit, use "F". To indicate Kelvin use "K"
#' 
#' @return data.frame, same format as "predicted", with additional column indicating correction factor and corrected (sub) daily temperature
#' 
#' @author Lars Caspersen
#' 
#' @examples \dontrun{
#' 
#' #download a simple dataset for a one week forecast
#' #takes one or two minutes
#' 
#' req <- download_seasonal_forecast(year = 1993:2000,
#' month = '11', 
#' area = c(51, 6.5, 50, 7.5),
#' leadtime_hour = seq(0, 24*30, by = 6), 
#' start_download = FALSE)
#' 
#' #run download
#' req <- download_seasonal_forecast(year = 1993:2000,
#' month = '11', 
#' area = c(51, 6.5, 50, 7.5),
#' leadtime_hour = seq(0, 24*30, by = 6), 
#' start_download = FALSE,
#' request_env = req)
#' 
#' #extract values
##' fname <- 'season-forecast_dwd21_2m_temperature_1996_11_1_0-168_51-6.5-50-7.5.nc'
#' 
#' extract_seasonal_forecast(file = fname, 
#' target_lat = 50.7,
#' target_lon = 7.1)
#' 
#' #download local observation
#' long <- 7.0871843
#' lat <- 50.7341602
#' weather_dwd <- chillR::handle_dwd(action = 'list_stations', 
#'                                   location = c(long, lat), 
#'                                   time_interval = c(19800101, 20251231))
#'                                   
#' data <- chillR::handle_dwd(action = "download_weather",
#'                            location = weather_dwd[1:3, "Station_ID"],
#'                            time_interval = c(19800101, 20241231),
#'                            stations_to_choose_from = 25,
#'                            station_list = weather_dwd,
#'                            drop_most = TRUE,
#'                            add.DATE = FALSE,
#'                             quiet = TRUE,
#'                             add_station_name = FALSE)
#'                             
#' data_clean <- chillR::handle_dwd(data)
#' 
#' observed <- data_clean$`Köln/Bonn`
#' 
#' predicted_bias_corrected <- mva_bias_correction_forecast(observed, predicted)
#' 
#' }
#' 
#' @importFrom lubridate ym
#' @importFrom stats aggregate
#'  
#' @export mva_bias_correction_forecast
#' 

mva_bias_correction_forecast <- function(observed, predicted,
                                         unit_observed = 'C'){
  
  #summarize both on a monthly basis
  if(all(c('Month', 'Year') %in% colnames(observed)) == FALSE){
    stop('Columns "Month" and "Year" need to be present in object "observed"')
  }
  
  
  fcst_sum <- stats::aggregate(predicted$temp, by = list(Month = predicted$Month, 
                                                  Year = predicted$Year,
                                                  model = predicted$model),FUN = mean,
                        na.action = na.pass, na.rm = TRUE)
  fcst_sum$mean_fcst <- round(fcst_sum$x, digits = 4)
  fcst_sum$year_mo <- lubridate::ym(paste(fcst_sum$Year, fcst_sum$Month))
  
  
  #select target column
  if('Temp' %in% colnames(observed)){
    observed$target_col = observed$Temp
  } else if('Tmean' %in% colnames(observed)){
    observed$target_col = observed$Tmean
  } else if(all(c("Tmin", "Tmax") %in% colnames(observed))){
    observed$target_col = (observed$Tmin + observed$Tmax)/2
  } else {
    stop('Temperature column with the name "Tmean", "Temp", "Tmin" together with "Tmax" need to be present in object "observed".')
  }
  
  observed$year_mo <- lubridate::ym(paste(observed$Year, observed$Month))
  #filter only relevant years and months
  observed_sub <- observed[observed$year_mo %in% fcst_sum$year_mo,]
  
  observed_sum <-  stats::aggregate(observed_sub$target_col, by = list(Month = observed_sub$Month, 
                                                               Year = observed_sub$Year),FUN = mean,
                            na.action = na.pass, na.rm = TRUE)
  observed_sum$mean_obs <- round(observed_sum$x, digits = 4)
  
  
  if(unit_observed == 'C'){
    observed_sum$mean_obs <- observed_sum$mean_obs + 273.15
  } else if(unit_observed == 'F'){
    observed_sum$mean_obs <- (observed_sum$mean_obs + 459.67) * 5/9 
  }
  
  #----------------#
  #correct monthly bias
  #----------------#
  #calculate mean temperature per month
  clim.obs = mean(observed_sum$mean_obs, na.rm = T)
  clim.fcst = mean(fcst_sum$mean_fcst, na.rm = T)
  
  #calculate sd per month
  sigma.e = sd(fcst_sum$mean_fcst, na.rm = T)
  sigma.ref = sd(observed_sum$mean_obs, na.rm = T)
  
  #corrected monthly temperature of forecast data
  fcst_call = ((fcst_sum$mean_fcst - clim.fcst) * (sigma.ref/sigma.e)) + clim.obs
  
  #add info to forecast summary
  fcst_sum$fcst_call = fcst_call
  
  #calc correction factor that is used to scale the daily observation of forecast
  fcst_sum$corr_fact <- (fcst_sum$fcst_call / fcst_sum$mean_fcst)
  
  
  #select appropriate columns
  fcst_sum <- fcst_sum[,c('Year', 'Month', 'model', 'mean_fcst', 'fcst_call', 'corr_fact')]
  
  combined <-  merge(predicted, fcst_sum, by = c('Year', 'Month', 'model')) 
  combined$temp_corrected = combined$temp * combined$corr_fact 
  return(combined)
}