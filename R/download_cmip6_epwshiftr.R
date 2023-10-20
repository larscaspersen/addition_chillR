#' Download climate change data of CMIP6 and extract for specified location
#' 
#' This function allows the used to download climate changed of the ScenarioMIP
#' project of CMIP6. Data is downloaded from the "Metagrid Search Interface" provided by ESGF with the
#' help of the \code{\link{epwshiftr}} package.
#' 
#' At first data is queried using the function \link[epwshiftr]{init_cmip6_index}. If the 
#' user wants at first to get a feeling how much data files are found in the search, it
#' may be a good idea to run the query at first using directly the function. Terms for the search include:
#' 
#' \itemize{
#'  \item{variable: Which weather variable should be downloaded. Commonly used are 'Tmin' for
#'  daily minimum temperature, 'Tmax' for daily maximum temperature and 'Prec' for precipitation.} 
#'  \item{Scenarios: defines the root scenarios identifier. Commonly used Tier 1 scenarioss include: c("ssp126", "ssp245", "ssp370", "ssp585")}
#'  \item{Frequency: defines the timestep of the model output. Default is 'mon' for monthly}
#'  \item{Resolution: size of the pixels of the model output. Default is "100 km"}
#'  \item{...: there are more possible search terms which can include 'source' (name of model) or 'variant' (identifier for the individual run of the model output). For more
#'  details please consult the documentation of \link[epwshiftr]{init_cmip6_index}. Additional search terms can be specified at the end of the arguments via the '...' argument} 
#' }
#' 
#' @param variable character / vector  of characters indicating which variables should be downloaded. Common choices are "Tmax" maximum temperature (°C), "Tmin" for minimum temperature (°C) and "Prec" for precipitation sum (mm). 
#' You can also choose other parameters, For more details please check also the documenation of \link[epwshiftr]{init_cmip6_index}
#' @param year_start numeric, marks the start of the time series to be downloaded. Usually is 2015
#' @param year_end numeric, marks the end of the time series to be downloaded. Usually is 2100
#' @param scenarios character / vector or characters defining the root scenarios identifier. Commonly used Tier 1 scenarioss include: c("ssp126", "ssp245", "ssp370", "ssp585")
#' @param frequency defines the timestep of the model output. Default is 'monthly'. Further options include "hourly", "daily" and "yearly". For more options you can also refer to \link[epwshiftr]{init_cmip6_index}.
#' @param resolution size of the pixels of the model output. Default is "100 km"
#' @param activity character, allows to specify from which modelling activity data is downloaded. By default 'ScenarioMIP'
#' @param source by default NULL, allows to specify which models should be included. If set NULL, all available models will be used
#' @param path_download character, defining the path relative to working directory, 
#' in which the .nc files should be (temporarily) safed
#' @param ... allows the user to define further search terms for the query such as 
#' 'source' or 'variant'.
#' @return NULL, saves files in the specified path
#' 
#' @examples 
#' \dontrun{
#' download_cmip6_epwshiftr(year_start = 2015,
#' year_end = 2100,
#' variable =c('tasmin', 'tasmax'),
#' scenarios = c('ssp126'))
#' 
#' }
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @importFrom epwshiftr init_cmip6_index
#' @importFrom epwshiftr get_data_node
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom assertthat is.number 
#' @importFrom assertthat assert_that
#' @importFrom purrr walk
#'  
#' @export download_cmip6_epwshiftr

download_cmip6_epwshiftr <- function(variable = c('Tmin', 'Tmax'),
    
  year_start = 2015,
                                 year_end = 2100,
                                 scenarios = c('ssp126', 'ssp245', 'ssp370', 'ssp585'),
                                 frequency = 'monthly',
                                 resolution = '100 km',
                                 activity = 'ScenarioMIP', 
                                 source = NULL,
                                 path_download = 'cmip6_downloaded',
                                 ...){
  
  
  #assertthat::assert_that(all(variable %in% c('tasmin', 'tasmax', 'pr')), msg = "Only c('tasmin', 'tasmax', 'pr') are valid options for variable")
  assertthat::is.number(year_start)
  assertthat::is.number(year_end)
  assertthat::assert_that(all(is.character(variable)))
  assertthat::assert_that(is.character(scenarios))
  #assertthat::assert_that(all(is.character(scenario)))
  
  
  #if the variable is Tmin or Tmax or Prec then substitute it with the real name
  if('Tmin' %in% variable){
    variable[variable == 'Tmin'] <- 'tasmin'
  }
  if('Tmax' %in% variable){
    variable[variable == 'Tmax'] <- 'tasmax'
  }
  if('Prec' %in% variable){
    variable[variable == 'Prec'] <- 'pr'
  }

  if(frequency == 'monthly'){
    frequency <- 'mon'
  }
  if(frequency == 'daily'){
    frequency <- 'day'
  }
  if(frequency == 'yearly'){
    frequency <- 'yr'
  }
  if(frequency == 'houlry'){
    frequency <- '1hr'
  }
  
  #coordinates <- data.frame(id = 'id_1', Longitude = 9.2, Latitude = 54.6 )
  
  #query the data.base
  query <- epwshiftr::init_cmip6_index(variable = variable,
                                       frequency = frequency, 
                                       experiment = scenarios,
                                       resolution = resolution,
                                       latest = TRUE,
                                       activity = activity,
                                       years = c(year_start, year_end),
                                       ...)
  # query <- epwshiftr::init_cmip6_index(variable = variable,
  #                                      frequency = frequency,
  #                                      experiment = scenarios,
  #                                      resolution = resolution,
  #                                      latest = TRUE,
  #                                      activity = activity,
  #                                      years = c(year_start, year_end))
  
  #if query didn't reutrn anything stop the funciton
  if(length(query) == 0){
    stop('The search returned no valid variable. Try different search options')
  }
  
  #the date subset doesnt work so well, so make sure that only this period is covered
  #somehow the end date must be 1st of december 2100 and not 31st...
  query <- query[as.Date(query$datetime_start) <= as.Date(paste0(year_start,'-01-01')),]
  query <- query[as.Date(query$datetime_end) >= as.Date(paste0(year_end,'-12-01')),]
  
  if(length(query) == 0){
    stop('Subsetting the query for the desired year_start and year_end lead to no remaining entry. Try different start and end year')
  }
  

  ######
  #download data
  ######
  
  #create temporary directory
  if(dir.exists(path_download) == FALSE){
    dir.create(path_download) 
  }
  
  #if the directory already exists, check which files were already downloaded
  flist <- list.files(paste0(path_download,'/'))
  
  #check which nodes are available for download
  active_nodes <- epwshiftr::get_data_node()
  
  
  cat('Downloading CMIP6 data\n')
  purrr::walk(1:nrow(query), function(i){
    #check if the node we want to use active, if not skip it
    if(query$data_node[i] %in% active_nodes$data_node[active_nodes$status == 'DOWN']){
      #print('next')
      return(NULL)
    }
    
    
    #last part of url should be the file name
    fname <- strsplit(query$file_url[i],split='/', fixed=TRUE)[[1]]
    fname <- fname[length(fname)]
    
    #check if fname is in flist
    if(!(fname %in% flist)){
      #mode wb is very important
      # download.file(url = query$file_url[i], 
      #               destfile =  paste0(path_download,'/',fname),
      #               mode = "wb")
      
      httr::GET(query$file_url[i], httr::write_disk( paste0(path_download,'/',fname), overwrite = T))
    }
  }, .progress = TRUE)
  
}
