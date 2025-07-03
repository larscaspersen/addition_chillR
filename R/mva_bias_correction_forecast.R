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
#' @param obs_cols character, indicates in which columns the observed weather variable is stored. Can be also a vector of column names, needs
#' to match the length and order of the columns indicated in the pred_cols argument. Columns of predicted and observed are matched in the same order as 
#' indicated in the vectors
#' 
#' @param pred_cols character, indicates in which columns the forecast weather variable is stored. Can be also a vector of column names, needs
#' to match the length and order of the columns indicated in the obs_cols argument. Columns of predicted and observed are matched in the same order as 
#' indicated in the vectors
#' 
#' @param unit_observed, character, by default "C". Indicates the unit of the input temperature. Bias correction is done in Kelvin.
#' To indicate Fahrenheit, use "F". To indicate Kelvin use "K"
#' 
#' @param unit_predicted, character, by default "K". Indicates the unit of the forecast temperature. Bias correction is done in Kelvin.
#' To indicate Fahrenheit, use "F". To indicate Celsius use "C"
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
#' @importFrom stats na.pass
#' @importFrom assertthat assert_that
#'  
#' @export mva_bias_correction_forecast
#' 
#' 

mva_bias_correction_forecast <- function(observed, predicted,
                                         obs_cols = 'Tmean', pred_cols = 'temp',
                                         unit_observed = 'C', unit_predicted = 'K'){
  
  #summarize both on a monthly basis
  if(all(c('Month', 'Year', obs_cols) %in% colnames(observed)) == FALSE){
    stop(paste0('Columns ', paste(c('Month', 'Year', obs_cols), collapse = ', '), ' need to be present in object "observed"'))
  }
  
  #summarize both on a monthly basis
  if(all(c('Month', 'Year', pred_cols) %in% colnames(predicted)) == FALSE){
    stop(paste0('Columns ', paste(c('Month', 'Year', pred_cols), collapse = ', '), ' need to be present in object "predicted"'))
  }
  
  if(length(obs_cols) != length(pred_cols)){
    stop('Targeted columns in predicted and observed need to be of same length')
  }
  
  assertthat::assert_that(unit_observed %in% c('C', 'K', 'F'),
                          unit_predicted %in% c('C', 'K','F'))
  
  res_df <- data.frame()
  
  
  observed$year_mo <- lubridate::ym(paste(observed$Year, observed$Month))
  #filter only relevant years and monthssss
  #if it coveres less than ione month, then take 15 years centered at year
  
  if(( max(predicted$Year)- min(predicted$Year))>15){
    years_add <- (15 - (max(predicted$Year)- min(predicted$Year))) / 2
    observed_sub <- observed[observed$Year >= min(predicted$Year) - years_add &
                   observed$Year <= max(predicted$Year) + years_add &
                   observed$Month %in% unique(predicted$Month), ]
  }
  
  
  res_df <- predicted
  
  #iterate over variables
  for(i in 1:length(obs_cols)){
    
    #monthly mean of forecast
    fcst_sum <- stats::aggregate(predicted[,pred_cols[i]], by = list(Month = predicted$Month, 
                                                           Year = predicted$Year,
                                                           model = predicted$model),FUN = mean,
                                 na.action = na.pass, na.rm = TRUE)
    fcst_sum$mean_fcst <- round(fcst_sum$x, digits = 4)
    fcst_sum$year_mo <- lubridate::ym(paste(fcst_sum$Year, fcst_sum$Month))
    
    observed_sum <-  stats::aggregate(observed_sub[,obs_cols[i]], by = list(Month = observed_sub$Month, 
                                                               Year = observed_sub$Year),FUN = mean,
                            na.action = stats::na.pass, na.rm = TRUE)
    observed_sum$mean_obs <- round(observed_sum$x, digits = 4)
    
    if(unit_observed == 'C'){
      observed_sum$mean_obs <- observed_sum$mean_obs + 273.15
      } else if(unit_observed == 'F'){
        observed_sum$mean_obs <- (observed_sum$mean_obs + 459.67) * 5/9 
      }
    
    if(unit_predicted == 'C'){
      fcst_sum$mean_fcst <- fcst_sum$mean_fcst + 273.15
    } else if(unit_predicted == 'F'){
      fcst_sum$mean_fcst <- (fcst_sum$mean_fcst + 459.67) * 5/9 
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
    if(is.na(sigma.ref)) sigma.ref <- sigma.e
    
    #corrected monthly temperature of forecast data
    fcst_call = ((fcst_sum$mean_fcst - clim.fcst) * (sigma.ref/sigma.e)) + clim.obs
    
    #add info to forecast summary
    fcst_sum$fcst_call = fcst_call
    
    #calc correction factor that is used to scale the daily observation of forecast
    fcst_sum[,paste0(pred_cols[i], '_corfac')] <- (fcst_sum$fcst_call / fcst_sum$mean_fcst)
    
    
    #select appropriate columns
    fcst_sum <- fcst_sum[,c('Year', 'Month', 'model', paste0(pred_cols[i], '_corfac'))]
    
    res_df <-  merge(res_df, fcst_sum, by = c('Year', 'Month', 'model')) 
    res_df[,paste0(pred_cols[i], '_corrected')] = res_df[, pred_cols[i]] * res_df[,paste0(pred_cols[i], '_corfac')] 
    
    #bring back to original format
    if(unit_predicted == 'C'){
      res_df[,paste0(pred_cols[i], '_corrected')] <- res_df[,paste0(pred_cols[i], '_corrected')] - 273.15
    } else if(unit_predicted == 'F'){
      res_df[,paste0(pred_cols[i], '_corrected')]  <- (res_df[,paste0(pred_cols[i], '_corrected')] *  (9/5)) - 459.67 
    }
  }
  
  #drop the correction factors
  col_drop <- which(grepl(pattern = '*_corfac', colnames(res_df)))

  return(res_df[,-col_drop])
}

