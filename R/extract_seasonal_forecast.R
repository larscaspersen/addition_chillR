#' Opens downloaded seasonal forecast from the C3S database
#' 
#' Opens a downloaded netcdf file (.nc) and brings it into a data.frame format. 
#' In case target latitude and longitude are provided, it extracts the values of the closest
#' pixel. Otherwise it will extract all pixel values. The function was developed for
#' precipitation but may also be compatible with other variables.
#' 
#' 
#' 
#' @param file character, is the name of the file that needs to be extracted.
#' In case the file is not in the working directory, the value needs to include either an
#' absolute or relative path to the file.
#' 
#' @param target_lat, numeric, latitude of the location you want to extract. Be default set to NULL. If both target_lat and target_lon are NULL, then all the pixel values are extracted.
#' 
#' @param target_lon numeric, longitude of the location you want to extract. Be default set to NULL. If both target_lat and target_lon are NULL, then all the pixel values are extracted.
#' 
#' @return data.frame containing the columns: Year, Month, Day, Hour, temp, unit, model, latitude, longitude, target_lat, target_lon.
#' Year, Month, Day, Hour indicate the date for the extracted value.
#' one column containing the variable value and another with the unit of the value
#' name of the variable columns are the same as in the nc file
#' model: indicates the individual model that provided the output. Usually, the seasonal forecast contain an
#' reference_time: start point of the forecast (Date)
#' ensemble of models making the predictions. The underlying model is that generated the observation is shared among models, they are just different instances, as the forecast involves randomness.
#' latitude: original latitude of the pixel
#' longitude: original longitude of the pixel
#' target_lat: latitude for the target point, that we wanted to extract
#' target_lon: longitude of the target point, that we wanted to extract
#' 
#' @author Lars Caspersen
#' 
#' @examples \dontrun{
#' 
#' #download a simple dataset for a one week forecast
#' #takes one or two minutes
#' download_seasonal_forecast(year = c('1996'),
#' month = '11', 
#' area = c(51, 6.5, 50, 7.5),
#' leadtime_hour = seq(0, 24*7, by = 6), 
#' start_download = TRUE)
#' 
#' fname <- 'season-forecast_dwd21_2m_temperature_1996_11_1_0-168_51-6.5-50-7.5.nc'
#' 
#' extract_seasonal_forecast(file = fname, 
#' target_lat = 50.7,
#' target_lon = 7.1)
#' 
#' }
#' 
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @importFrom CFtime CFtime
#' @importFrom CFtime as_timestamp
#' @importFrom CFtime parse_timestamps
#'  
#' @export extract_seasonal_forecast
#' 
#' 
extract_seasonal_forecast <- function(file, 
target_lat = NULL, 
target_lon = NULL){
  
  #open file
  nc <- ncdf4::nc_open(file)
  
  #check variables
  vars <- names(nc$var)
  #expect vars to be valid time and t2m
  if(any(vars %in% c("valid_time", "t2m", "mx2t24", "mn2t24") == FALSE)) {
    warning(paste('Expected ncdf file to contain "valid_time", and one of the following further variables "t2m", "mx2t24", or "mn2t24". However, this file contains:', paste(vars, collapse = ', '), 
                  '\nExtracting values might malfunction for the variables you downloaded'))
  }
  
  #check dimensions
  dims <- names(nc$dim)
  if(any(dims != c("number", "forecast_reference_time", "forecast_period", "latitude", "longitude"))) {
    stop(paste('Expected dimensions of ncdf file to contain: "number", "forecast_reference_time", "forecast_period", "latitude", "longitude". However, this file contains:', paste(dims, collapse = ', '), 
               '\nExtracting values might malfunction for the variables you downloaded'))
  }
  
  #extract dimensions
  #extract values stored in ncdf file
  lon <- ncdf4::ncvar_get(nc, "longitude")  # or "longitude"
  lat <- ncdf4::ncvar_get(nc, "latitude")  # or "latitude"
  number <- ncdf4::ncvar_get(nc,"number")
  fcst_ref_time <- ncdf4::ncvar_get(nc,"forecast_reference_time")
  fcst_period <- ncdf4::ncvar_get(nc,"forecast_period")
  
  #-----------------#
  #TIME
  #-----------------#
  
  #extract time
  time_array <- ncdf4::ncvar_get(nc,vars[1])
  time_units <- ncdf4::ncatt_get(nc,vars[1],"units")
  
  # decode time
  cf <- CFtime::CFtime(time_units$value, calendar = "proleptic_gregorian", time_array) # convert time to CFtime class
  timestamps <- CFtime::as_timestamp(cf) # get character-string times
  
  #bring into proper format
  time_cf <- CFtime::parse_timestamps(cf, timestamps)
  
  
  #process reference period (startpoint of forecast)
  ref_time_units <- ncdf4::ncatt_get(nc, "forecast_reference_time", "units")$value
  ref_time_cf <- CFtime::CFtime(ref_time_units, calendar = "proleptic_gregorian", fcst_ref_time)
  ref_timestamps <- CFtime::as_timestamp(ref_time_cf)
  
  
  #------------------#
  #LOCATION
  #------------------#
  
  grd <-expand.grid(1:length(lon), 1:length(lat))
  lon_idx <- grd$Var1
  lat_idx <- grd$Var2
  
  if(!(is.null(target_lat) & is.null(target_lon))){
    
    
    if(!all(is.numeric(target_lat)) & all(is.numeric(target_lon))) {
      stop('Target latitude and longitude need to be numeric')
    }
    
    #check if lat and lon are covered
    covered_lon <- min(lon) <= min(target_lon) & max(target_lon) <= max(lon)
    covered_lat <- min(lat) <= min(target_lat) & max(target_lon) <= max(lat)
    
    if(covered_lon == FALSE | covered_lat == FALSE){
      stop('Target latitude or longitude not covered by downloaded grid. Either check if you downloaded the right area or check if the target coordinates are right')
    }
    
    
    #position of the pixel(s) that we extract
    lat_idx <- c()
    lon_idx <- c()
    #find closest pixel to the target coordinates
    for(i in 1:length(target_lat)){
      lon_i <- which.min(abs(lon - target_lon[i]))
      lat_i <- which.min(abs(lat - target_lat[i]))
      
      lat_idx <- c(lat_idx, lat_i)
      lon_idx <- c(lon_idx, lon_i)
    }
    
    
    
  }
  
  
  #-----------------#
  #TEMPERATURE
  #-----------------#
  
  #contains the extracted values
  res_df <- data.frame()
  
  #iterate over further variables
  for(v in vars[2:length(vars)]){
    #extract data
    temp_array <- ncdf4::ncvar_get(nc,v, collapse_degen = FALSE)
    #extract unit
    temp_units <- ncdf4::ncatt_get(nc,v,"units")
    
    temp_df <- data.frame()
    for(i in 1:length(number)){
      for(j in 1:length(lon_idx)){
        temp <- as.vector(temp_array[lon_idx[j], lat_idx[j],,,i])  
        
        int_df <- time_cf[,c('year', 'month', 'day', 'hour')]
        
        int_df[,v] <- round(temp, digits = 4)
        int_df[,paste0('unit_', v)] <- temp_units$value
        int_df$latitude <- lat[lat_idx[j]]
        int_df$target_lat <- target_lat[j]
        int_df$longitude <- lon[lon_idx[j]]
        int_df$target_lon <- target_lon[j]
        int_df$model <- i
        int_df$reference_time <- rep(ref_timestamps, each = length(fcst_period))
        
        temp_df <- rbind(temp_df, int_df)
      }
      
      
    }
    
    if(nrow(res_df) == 0){
      res_df <- temp_df
    } else{
      #merge with final data.frame
      res_df <- merge(res_df, temp_df, by = c('year', 'month', 'day', 'hour', 'latitude', 'longitude', 'target_lat', 'target_lon', 'model', 'reference_time'))
    }

  }
  

  
  ncdf4::nc_close(nc)
  
  #rename some columns
  colnames(res_df)[which(colnames(res_df) == 'month')] <- 'Month'
  colnames(res_df)[which(colnames(res_df) == 'year')] <- 'Year'
  colnames(res_df)[which(colnames(res_df) == 'day')] <- 'Day'
  colnames(res_df)[which(colnames(res_df) == 'hour')] <- 'Hour'
  
  return(res_df)
  
}