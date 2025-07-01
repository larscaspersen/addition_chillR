#' Download shortterm seaonal weather forecast
#' 
#' Access the Weather Forecast API (https://open-meteo.com/en/docs) and 
#' downloads forecasted temperature (or any other variable you specify)
#' for a period up to 16 days.
#' 
#' The API collects forecasts from many weather agencies and weather models. 
#' You can find more information on their website.
#' 
#' You can eihter make pre-defined data requests by specifying latitude,
#' longitude, but you can also customize your request. Customized request
#' need to supplied in a named list format under the parameter: params. 
#' Check the website for the request element names and values. 
#' 
#' For example you can request other weather variables, e.g. precipitation, 
#' or you can request data to be in a daily format instead of hourly. You could 
#' also request data from a certain weather model.
#' 
#' 
#' 
#' @param latitude numeric, latitude in decimal format of the location of interest.
#' As the API mostly uses gridded data, it could be that it forwards data of a slightly
#' different coordinate
#' 
#' @param longitude numeric, longitude in decimal format of the location of interest.
#' As the API mostly uses gridded data, it could be that it forwards data of a slightly
#' different coordinate
#' 
#' @param var numeric, by default set to 'temperature_2m'. Specifies the weather variable
#' that should be downloaded.
#' 
#' @param forecast_days numeric, number of days the forecast should span. 16 is the maximum
#' value
#' 
#' @param base_url character, url of the API client
#' 
#' @param params list, by default NULL. This argument allows the user to make
#' customized requests. The format need to be named. Check the website for element
#' names and values. If specified, other arguements apart of base_url are ignored
#' 
#' @return data.frame containing the columns: 
#' Date, Year, Month, Day, Hour, Var, Unit, latitude, longitude.
#' Most columns are self-explanatory. Var contains the requested variable,
#' Unit specifies in what unit it is provided. latitude and longitude
#' specify the pixels coordinate the data is taken from, so it may differ
#' #from the originally requested coordinates
#' 
#' @author Lars Caspersen
#' 
#' @examples \dontrun{
#' 
#' #download temperature for Bonn
#' fcst_temp <- download_shortterm_forecast(latitude = 50.7,
#'                                          longitude = 7.1,
#'                                          var = 'temperature_2m',
#'                                          forecast_days = 16)
#' 
#' #download rain for Bonn
#' fcst_rain <- download_shortterm_forecast(latitude = 50.7,
#'                                          longitude = 7.1,
#'                                          var = 'rain',
#'                                          forecast_days = 16)
#' 
#' #specify model
#' #--> need to build params list
#' fcst_specific_model <- download_shortterm_forecast(params = list(
#'   latitude = 50.7, 
#'   longitude = 7.1,
#'   hourly = 'temperature_2m',
#'   forecast_days = 16,
#'  model = 'ecmwf_ifs025'
#' ))
#' 
#' #seamless combines forecast of several models
#' fcst_specific_model <- download_shortterm_forecast(params = list(
#'   latitude = 50.7, 
#'   longitude = 7.1,
#'   hourly = 'temperature_2m',
#'   forecast_days = 16,
#'   model = 'icon_seamless'
#' )) 
#' }
#' 
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom httr status_code
#' @importFrom purrr map_chr
#' @importFrom lubridate ymd_hm
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate hour
#'  
#' @export download_shortterm_forecast
#' 

download_shortterm_forecast <- function(latitude=NULL,
                                        longitude=NULL,
                                        var = 'temperature_2m',
                                        forecast_days = 16,
                                        base_url = 'https://api.open-meteo.com/v1/forecast?',
                                        params = NULL){
  
  #in case params are provided as a list ignore this,
  #otherwise build by function arguments
  if(is.null(params)){
    params <- list(
      latitude = latitude,
      longitude = longitude,
      hourly = var,
      forecast_days = forecast_days
    )
  } 

  # Send GET request
  response <- httr::GET(url = base_url, query = params)
  
  
  if (httr::status_code(response) == 200) {
    result <- jsonlite::fromJSON(httr::content(response, "text"))
    #print(names(result))  # View top-level keys
  } else {
    stop("Error: ", httr::status_code(response))
  }
  
  #process the data
  var_downloaded <- result$hourly[[var]]
  unit <- result$hourly_units[[var]]

  date_int <- strsplit(result$hourly$time, 'T')
  
  date <- purrr::map_chr(date_int, 1)
  hour <- purrr::map_chr(date_int, 2)
  date_ymdhm <- lubridate::ymd_hm(paste(date, hour))
  
  #generate data.frame with columns
  
  return(data.frame(Date = date_ymdhm,
             Year = lubridate::year(date),
             Month = lubridate::month(date),
             Day = lubridate::day(date),
             Hour = lubridate::hour(date),
             Var = var_downloaded,
             Unit = unit,
             latitude = result$latitude,
             longitude = result$longitude))
}
