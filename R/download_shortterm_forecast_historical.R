#' Download historical shortterm seaonal weather forecast
#' 
#' Access the Weather Forecast API (https://open-meteo.com/en/docs/historical-forecast-api) and 
#' downloads forecasted temperature (or any other variable you specify) for a period
#' up to sixteen days. This function makes great use of the 
#' download_shortterm_forecast function of the same package. Historical forecast
#' are available since 2022.
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
#' @param start_date character, date in a format 'yyyy-mm-dd'
#' 
#' @param end_date character, date in a format 'yyyy-mm-dd'. The total period shoul
#' no exceed 16 days
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
#' fcst_temp <- download_shortterm_forecast_historical(latitude = 50.7,
#'                                          longitude = 7.1,
#'                                          var = 'temperature_2m',
#'                                          start_date = '2024-11-01,
#'                                          end_date = '2024-11-10)
#' 
#' #download rain for Bonn
#' fcst_rain <- download_shortterm_forecast_historical(latitude = 50.7,
#'                                          longitude = 7.1,
#'                                          var = 'rain',
#'                                          start_date = '2024-11-01,
#'                                          end_date = '2024-11-10)
#' }
#' 
#' @export download_shortterm_forecast_historical
#' 

download_shortterm_forecast_historical <- function(latitude=NULL,
                                        longitude=NULL,
                                        var = 'temperature_2m',
                                        start_date = NULL,
                                        end_date = NULL,
                                        base_url = 'https://historical-forecast-api.open-meteo.com/v1/forecast?',
                                        params = NULL){
  
  if(is.null(params)){
    params <- list(
      latitude = latitude,
      longitude = longitude,
      hourly = var,
      start_date = start_date,
      end_date = end_date
    )
  } 
  out <- download_shortterm_forecast(params = params, base_url = base_url)
  return(out)
}
