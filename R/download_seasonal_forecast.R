#' Downloads seasonal forecast from the C3S database
#' 
#' Starts the download request for the seasonal forecast. The seasonal forecast data comes from the
#' Copernicus Climate Change Service (C3S). The full name of the dataset is: Seasonal
#' forecast daily and subdaily data on single levels. You can find more information on
#' the dataset online: https://cds.climate.copernicus.eu/datasets/seasonal-original-single-levels?tab=overview.
#' 
#' The communication with the API is done with the help of the ecmwfr package. 
#' Check out the ecmwfr package: https://cran.r-project.org/web/packages/ecmwfr/index.html
#' The package can be used to also access other datasets. For instance, the ecmwfr package is used
#' to download climate change projections from the Coupled Model Intercomparison Project Phase 6 (CMIP6)
#' in the chillR package. 
#' 
#' 
#' @param year numeric or character, indicates for which years forecast data 
#' should be downloaded. Can also be a vector of years.
#' For most data sources, earliest possible year is 1993, some also can go back to 1981. For
#' more details check the documentation of the dataset. 
#' 
#' @param month numeric or character, indicates for which months forecast data should be downloaded. 
#' This indicates the start_point of the forecast. Can also be a vector of months. 
#' 
#' @param area numeric, of length 4. Specifies the coordinates of the area for 
#' the downloaded forecast data. Coordinates are provided in the format:
#' max latitude, min longitude, min latitude, max longtidue  
#' 
#' @param leadtime_hours numeric or character, indicates for which timepoints 
#' data should be downloaded. By default is set to 'all', requesting full length of forecast.
#' Leadtime hour indicates timepoints (in hours since first day midnight). 
#' Data is provided every six hours, so provided values need to be divisible by six.
#' Can be a vector of hours. Maximum value depends on data source, but in most cases cover up to six months (=4416). 
#' 
#' @param fname character, indicates file name for the download. By default is set to NULL,
#' so that file name is decided automatically. Automated file naming indicates most relevant
#' parameter specified in the download call. For more infor of automated file naming, check the details. 
#' 
#' @param download_path character, specifies where the downloaded file should be saved. Is by default
#' the working directory of the R session. 
#' 
#' @param start_download logical, indicates if download should start after the request is sent. 
#' If set TRUE (default), the function will be busy until the file is downloaded from the
#' API. Usually it takes a while until the dataset is ready for download, so in larger requests
#' (several years, several months) it is adviced to set to FALSE. When set to FALSE,
#' the request is sent to API and the file can be downloaded later. See in example how the
#' download and request can be run independently from another.
#' 
#' @param request_env environment object, created by the download function when parameter start_download is set FALSE.
#' Allows the download of the requested forecast data independently from the request. Can be also used in 
#' a different R session, if the request_env is saved. 
#' 
#' @param data_format character, decides on file format. By default is set to 'netcdf'.
#' The API also allows to download .grib files (data_format = 'grib'). But it is adviced
#' to use 'netcdf', as the extraction function only works for netcdf files.
#' 
#' @param day, character or numeric, indicates the day of the month for the start of the forecast. Most sources only offer the first day of the month
#' for the start. By default set to 1, for first day of the month
#' 
#' @param originating_centre character, indicates the data source. By default set to 'dwd'. See details for more options.
#' 
#' @param variable character, indicates what variable to download. By default set to '2m_temperature' for temperature at
#' 2m height. Check the documentation of the dataset for more options.
#' 
#' @param system character, indicates the global circulation model used for the forecast. Check documentation of the dataset, 
#' or the details, to see what combination of originating_center and system are valid.
#' 
#' @param target_lon numeric, longitude of the location you want to extract. Be default set to NULL. If both target_lat and target_lon are NULL, then all the pixel values are extracted.
#' 
#' @return if start_download = TRUE, then nothing is returned. If start_download = FAlSE, then an environment object is returned,
#' that can be used to download the requested data later (once it is ready for download). 
#' 
#' @details
#' 
#' Combination of originating center, system and maximum leadtime_hour
#' 
#' \item{ecmwf}{system = c('4', '5', '51'), year_start = 1981, leadtime_hour_end = 5160, month = 1:12, day = '01'} 
#' \item{ukmo}{system = as.character(c(12:15, 600:604)), year_start = 1993, leadtime_hour_end = 5160, month = 1:12, day = c('01', '9', '17', '25')}
#' \item{meteo_france}{system = as.character(5:9),year_start = 1993, leadtime_hour_end = 5160, month = 1:12, day = c('01')}
#' \item{dwd}{system = as.character(c(2, 21,22)),year_start = 1993, leadtime_hour_end = 4416,month = 1:12, day = c('01')}
#' \item{cmcc}{system = as.character(c(3, 35)), year_start = 1993, leadtime_hour_end = 4416, month = 1:12, day = c('01')}
#' \item{ncep}{system = as.character(c(2)),year_start = 1993,leadtime_hour_end = 5160,month = 1:12,day = as.character(1:30)}
#' \item{jma}{system = as.character(c(2, 3)),year_start = 1981,leadtime_hour_end = 5160,month = 1:12,day = as.character(1:30)}
#' \item{eccc}{system = as.character(1:5),year_start = 1993, leadtime_hour_end = 5136,month = 1:12,day = c('01')}
#' \item{bom}{system = as.character(2),year_start = 1993,leadtime_hour_end = 5208,month = 2:7,day = c('01'))}

#' #' Structure of automated names
#' 
#' \item{'seasonal_forecast'} 
#' \item{organization (e.g. dwd) with system (e.g. 21) --> dwd21}
#' \item{variable (2m_temperature)}
#' \item{years (e.g. 1996), if a range is supplied it covers min year to max year (e.g. 1996-2000)}
#' \item{reference month (e.g. 11) this is the start point of the forecast, if a range is supplie it covers min month to max month (e.g. 1-12)}
#' \item{leadtime (e.g 6), this is the hours since the reference month (usually starts at day 1 of the month), if range it covers 
#' max leadtime_hour to min leadtime_hour (e.g. 0-168 for a week forecast)}
#' \item{area (e.g. 51-6.5-50-7.5), coordinates of map, follows the order: max latitude, min longitude, min latitude, max longtidue}
#' Example: season-forecast_dwd21_2m_temperature_1996_11_1_0-24_51-6.5-50-7.5
#' year: 1996, month: 11, leadtime: from 0 to 24 (from Nov 1 to Nov 2), area: 51, 6.5, 50, 7.5
#' 
#' How to run request and download indepently from another:
#' The API usually needs some time (some minutes to some hours) to process the request and make the data available for download.
#' So it can be convenient to handle data request and download seperately. If you set start_download = FALSE, 
#' then the function returns an object that allows to run the request later on. The object is an environment containing several variables and functions. 
#' To commence download, you can simply re-run the download function, this time supply the obtained object from the request to the variable 'request_env'
#' If the data is ready for download, the download will start. If it still being processed or if it is still queued, the function would inform you about it and
#' you can try it another time. You can also directly use the object obtained from the intial data-request to do the download manually. You can find an example
#' down below. 
#' 
#' @author Lars Caspersen
#' 
#' @examples \dontrun{
#' 
#' #download a simple dataset for a one week forecast
#' #takes one or two minutes
#' 
#' download_seasonal_forecast(year = c('1996'),
#' month = '11', 
#' area = c(51, 6.5, 50, 7.5),
#' leadtime_hour = seq(0, 24*2, by = 6), 
#' start_download = TRUE)
#' 
#' #use initial request and download indepently from another
#' req <- download_seasonal_forecast(year = c('1996'),
#' month = '11', 
#' area = c(51, 6.5, 50, 7.5),
#' leadtime_hour = seq(0, 24*2, by = 6), 
#' start_download = FALSE)
#' 
#' #run download
#' download_seasonal_forecast(year = c('1996'),
#' month = '11', 
#' area = c(51, 6.5, 50, 7.5),
#' leadtime_hour = seq(0, 24*2, by = 6), 
#' start_download = FALSE,
#' request_env = req)
#' 
#' #you can also save the req_environment and run the download later in another r session
#' saveRDS(req, file = 'request.rds')
#' req <- readRDS('request.rds')
#' 
#' }
#' 
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @importFrom assertthat is.dir
#' @importFrom ecmwfr wf_transfer
#' @importFrom ecmwfr wf_request
#'  
#' @export download_seasonal_forecast
#' 
download_seasonal_forecast <- function(year,
                                       month,
                                       area,
                                       leadtime_hour = 'all',
                                       fname = NULL,
                                       download_path = getwd(),
                                       start_download = TRUE,
                                       request_env = NULL,
                                       data_format = 'netcdf',
                                       day = NULL,
                                       originating_centre = 'dwd',
                                       variable = "2m_temperature",
                                       system = '21'){
  
  #in case request id is supplied, start the download
  if(is.null(request_env) == FALSE){
    
    request_env <- request_env$update_status()
    
    #not ready for download
    if(request_env$is_running()){
      message('The API is still preparing the data, it is not ready for download yet. Try a little bit later')
      return(NULL)
    }
    #something went wrong with the request
    if(request_env$is_failed()){
      error('The API indicated, that your request failed. Maybe you requeset was too large. Try reducing the number of requested years')
    }
    
    #download the request if it is pending
    if(request_env$is_pending()){
      ecmwfr::wf_transfer(url = request_env$get_url(),
                          path = req$.__enclos_env__$private$path,
                          filename = req$get_request()$target)
      return(NULL)
    }
  }
  
  #convert year, month, leadtime to characters, so it is easier to handle
  year <- as.character(year)
  month <- as.character(month)
  leadtime_hour <- as.character(leadtime_hour)
  
  #maybe also use check_request
  #ecmwfr::wf_check_request()
  
  
  #check input of request, send request (and download if start_download)
  
  #----------------------#
  #INPUT CONTROL
  #----------------------#
  
  #control inputs
  assertthat::assert_that(
    assertthat::is.string(system),
    assertthat::is.string(originating_centre),
    assertthat::is.string(data_format),
    length(area) == 4,
    is.numeric(area),
    area[1] >= -90 & area[1] <= 90,
    area[3] >= -90 & area[3] <= 90,
    area[2] >= -180 & area[2] <= 180,
    area[4] >= -180 & area[4] <= 180,
    area[1] > area[3],
    area[2] < area[4],
    month %in% as.character(1:12),
    data_format %in% c('netcdf', 'grib'),
    originating_centre %in% c('ecmwf', 'ukmo', 'meteo_france',
                              'dwd',
                              'cmcc', 'ncep', 'jma',
                              'eccc', 'bom'),
    assertthat::is.dir(download_path)
  )
  
  if(length(day) == 0){
    if(day == 1){
      day <- '01'
    }
  }
  
  #set the current year as highest possible end-year
  year_end = format(Sys.Date(), "%Y") %>% as.numeric()
  
  #master table what inputs per system are allowed
  master_list <- list('ecmwf' = list(system = c('4', '5', '51'),
                                     year_start = 1981,
                                     leadtime_hour_end = 5160,
                                     month = 1:12,
                                     day = '01'),
                      'ukmo' = list(system = as.character(c(12:15, 600:604)),
                                    year_start = 1993,
                                    leadtime_hour_end = 5160,
                                    month = 1:12,
                                    day = c('01', '9', '17', '25')),
                      'meteo_france' = list(system = as.character(5:9),
                                            year_start = 1993,
                                            leadtime_hour_end = 5160,
                                            month = 1:12,
                                            day = c('01')),
                      'dwd' = list(system = as.character(c(2, 21,22)),
                                   year_start = 1993,
                                   leadtime_hour_end = 4416,
                                   month = 1:12,
                                   day = c('01')),
                      'cmcc' = list(system = as.character(c(3, 35)),
                                    year_start = 1993,
                                    leadtime_hour_end = 4416,
                                    month = 1:12,
                                    day = c('01')),
                      'ncep' = list(system = as.character(c(2)),
                                    year_start = 1993,
                                    leadtime_hour_end = 5160,
                                    month = 1:12,
                                    day = as.character(1:30)),
                      'jma' = list(system = as.character(c(2, 3)),
                                   year_start = 1981,
                                   leadtime_hour_end = 5160,
                                   month = 1:12,
                                   day = as.character(1:30)),
                      'eccc' = list(system = as.character(1:5),
                                    year_start = 1993,
                                    leadtime_hour_end = 5136,
                                    month = 1:12,
                                    day = c('01')),
                      'bom' = list(system = as.character(2),
                                   year_start = 1993,
                                   leadtime_hour_end = 5208,
                                   month = 2:7,
                                   day = c('01')))
  
  #make sure that years are covered 
  if(all(year %in% as.character(master_list[[originating_centre]]$year_start:year_end)) == FALSE){
    stop(past0('The selected year is outside the valid range. You can only select years for the range: ', master_list[[originating_centre]]$year_start, ' - ', year_end))
  }
  
  if(length(leadtime_hour) == 1){
    if(leadtime_hour == 'all'){
      leadtime_hour <- as.character(seq(0, master_list[[originating_centre]]$leadtime_hour_end, by = 6))
    }
  }
  
  #make sure that leadtime is covered
  if(all(leadtime_hour %in% as.character(seq(0,master_list[[originating_centre]]$leadtime_hour_end, by = 6))) == FALSE){
    stop(paste0('The leadtime is outside the valid range. You can only select leadtime (hours) for the range: 0 - ',  master_list[[originating_centre]]$leadtime_hour_end,'.\ Hours need to be divisible by 6'))
  }
  
  if(all(as.character(day) %in% master_list[[originating_centre]]$day) == FALSE){
    stop(paste0('The selected day(s) is outside the valid range. You can only select:', master_list[[originating_centre]]$month))
  }
  
  assertthat::assert_that(
    system %in% master_list[[originating_centre]]$system
  )
  
  min_year <- min(as.numeric(year))
  if(min_year ==  max(as.numeric(year))){
    max_year <- NULL
  } else {
    max_year <- paste0('-', max(as.numeric(year)))
  }
  
  min_month <- min(as.numeric(month))
  if(min_month ==  max(as.numeric(month))){
    max_month <- NULL
  } else {
    max_month <- paste0('-', max(as.numeric(month)))
  }
  
  min_day <- min(as.numeric(day))
  if(min_day ==  max(as.numeric(day))){
    max_day <- NULL
  } else {
    max_day <- paste0('-', max(as.numeric(day)))
  }
  
  min_leadtime_hour <- min(as.numeric(leadtime_hour))
  if(min_leadtime_hour ==  max(as.numeric(leadtime_hour))){
    max_leadtime_hour <- NULL
  } else {
    max_leadtime_hour <- paste0('-', max(as.numeric(leadtime_hour)))
  }
  
  if(data_format == 'netcdf') fextension <- '.nc'
  if(data_format == 'grib') fextension <- '.grib'
  
  
  if(is.null(fname)){
  #create a filename based on the request
  fname <- paste0('season-forecast_',
                  originating_centre, system, '_',
                  variable, '_',
                  min_year, max_year, '_',
                  min_month, max_month, '_',
                  min_day, max_day, '_',
                  min_leadtime_hour, max_leadtime_hour, '_',
                  paste0(area, collapse = '-'), 
                  fextension)
  } else {
    #make sure it has the right extension
    stopifnot('The file format must match the file extension chosen in the file name.'= grepl(paste0('*', fextension), fname))
  }
  
  
  #bind the request
  request <- list(dataset_short_name = "seasonal-original-single-levels",
                  originating_centre = originating_centre,
                  variable = variable,
                  system = system,
                  year = year,
                  month = month,
                  day = day,
                  leadtime_hour = leadtime_hour,
                  data_format = data_format,
                  target = fname,
                  area = area)
  
  #submit request
  req <- ecmwfr::wf_request(request = request,
                            path = download_path, 
                            transfer = start_download)
  
  #return the environment of the request, needed to run the 
  #download later on
  if(start_download == FALSE){
    return(req)
  }
  
}