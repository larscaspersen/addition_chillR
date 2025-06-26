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
#' temp: contains the extracted temperature values
#' unit: indicates in what unit the temperatue is supplied (usually Kelvin)
#' model: indicates the individual model that provided the output. Usually, the seasonal forecast contain an
#' ensemble of models making the predictions. The underlying model is that generated the observation is shared among models, they are just different instances, as the forecast involves randomness.
#' latitude: original latitude of the pixel
#' longtidue: original longitude of the pixel
#' target_lat: latitude for the target point, that we wanted to extract
#' target_lon: longitude of the taret point, that we wanted to extract
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
  if(any(vars != c("valid_time", "t2m"))) {
    warning(paste('Expected ncdf file to contain "valid_time", and "t2m" as variables. However, this file contains:', paste(vars, collapse = ', '), 
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
  
  #extract data
  temp_array <- ncdf4::ncvar_get(nc,vars[2], collapse_degen = FALSE)
  #extract unit
  temp_units <- ncdf4::ncatt_get(nc,vars[2],"units")
  
  #container for extracted data
  temp_df <- data.frame()
  
  for(i in 1:length(number)){
    for(j in 1:length(lon_idx)){
      temp <- as.vector(temp_array[lon_idx[j], lat_idx[j],,,i])  
      
      int_df <- time_cf
      
      int_df$temperature <- round(temp, digits = 4)
      int_df$unit <- temp_units$value
      int_df$latitude <- lat[lat_idx[j]]
      int_df$target_lat <- target_lat[j]
      int_df$longitude <- lon[lon_idx[j]]
      int_df$target_lon <- target_lon[j]
      int_df$model <- i
      
      temp_df <- rbind(temp_df, int_df)
    }
    
    
  }
  
  ncdf4::nc_close(nc)
  
  #
  temp_df$Month <-  temp_df$month
  temp_df$Year <-  temp_df$year
  temp_df$Day <-  temp_df$day
  temp_df$Hour <-  temp_df$hour 
  temp_df$temp <-  temp_df$temperature 
  
  temp_df <- temp_df[,c('Year', 'Month', 'Day', 'Hour', 'temp', 'unit', 'model', 'latitude', 'longitude', 'target_lat', 'target_lon')]
  
  return(temp_df)
  
}