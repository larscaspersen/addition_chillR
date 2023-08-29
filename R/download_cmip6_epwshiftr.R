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
#'  \item{Metric: Which weather variable should be downloaded. Commonly used are 'tasmin' for
#'  daily minimum temperature, 'tasmax' for daily maximum temperature and 'pr' for precipitation.} 
#'  \item{Experiment: defines the root experiment identifier. Commonly used Tier 1 experiments include: c("ssp126", "ssp245", "ssp370", "ssp585")}
#'  \item{Frequency: defines the timestep of the model output. Default is 'mon' for monthly}
#'  \item{Resolution: size of the pixels of the model output. Default is "100 km"}
#'  \item{...: there are more possible search terms which can include 'source' (name of model) or 'variant' (identifier for the individual run of the model output). For more
#'  details please consult the documentation of \link[epwshiftr]{init_cmip6_index}. Additional search terms can be specified at the end of the arguments via the '...' argument} 
#' }
#' 
#' @param year_start numeric, marks the start of the time series to be downloaded. Usually is 2015
#' @param year_end numeric, marks the end of the time series to be downloaded. Usually is 2100
#' @param metric character / vector  of characters indicating which variables should be downloaded. 
#' Common are 'tasmax' for near-surface maximum temperature, 'tasmin' for near-surface minimum temperature and
#' 'pr' for precipitation. For more details please check also the documenation of \link[epwshiftr]{init_cmip6_index}
#' @param experiment character / vector or characters defining the root experiment identifier. Commonly used Tier 1 experiments include: c("ssp126", "ssp245", "ssp370", "ssp585")
#' @param frequency defines the timestep of the model output. Default is 'mon' for monthly
#' @param resolution size of the pixels of the model output. Default is "100 km"
#' @param activity character, allows to specify from which modelling activity data is downloaded. By default 'ScenarioMIP'
#' @param source by default NULL, allows to specify which models should be included. If set NULL, all available models will be used
#' @param path_download character, defining the path relative to working directory, 
#' in which the .nc files should be (temporarily) safed
#' @param ... allows the user to define further search terms for the query such as 
#' 'source' or 'variant'.
#' @return NULL, saves files in the specified path
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

download_cmip6_epwshiftr <- function(year_start = 2015,
                                 year_end = 2100,
                                 metric =c('tasmin', 'tasmax'),
                                 experiment = c('ssp126', 'ssp245', 'ssp370', 'ssp585'),
                                 frequency = 'mon',
                                 resolution = '100 km',
                                 activity = 'ScenarioMIP', 
                                 source = NULL,
                                 path_download = 'cmip6_downloaded',
                                 ...){
  
  
  #assertthat::assert_that(all(metric %in% c('tasmin', 'tasmax', 'pr')), msg = "Only c('tasmin', 'tasmax', 'pr') are valid options for metric")
  assertthat::is.number(year_start)
  assertthat::is.number(year_end)
  assertthat::assert_that(all(is.character(metric)))
  assertthat::assert_that(is.character(experiment))
  #assertthat::assert_that(all(is.character(scenario)))

  
  #coordinates <- data.frame(id = 'id_1', Longitude = 9.2, Latitude = 54.6 )
  
  #query the data.base
  query <- epwshiftr::init_cmip6_index(variable = metric,
                                       frequency = frequency, 
                                       experiment = experiment,
                                       resolution = resolution,
                                       latest = TRUE,
                                       activity = activity,
                                       years = c(year_start, year_end),
                                       ...)
  # query <- epwshiftr::init_cmip6_index(variable = metric,
  #                                      frequency = frequency,
  #                                      experiment = experiment,
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
  
  
  #extracting the data
  #extracted <- extract_cmip6_data(stations = stations, download_path = path_download)
  
  # #get names of downloaded files
  # fnames <- list.files(paste0(path_download,'/'))
  # fnames <- paste0(path_download,'/', fnames)
  # 
  # #only work with files which are not empty
  # fnames <- fnames[file.size(fnames) > 0L]
  # 
  # extracted_df <- purrr::map(fnames, function(x){
  #   
  #   #print(x)
  #   extract_df <- extract_cmip_data(fname = x, coords = coordinates)
  #   
  #   fragment_names <- strsplit(strsplit(x, '/')[[1]][2],'_')[[1]]
  #   
  #   extract_df$variable <- fragment_names[1]
  #   extract_df$model <- fragment_names[3]
  #   extract_df$ssp <- fragment_names[4]
  #   
  #   return(extract_df)
  #   
  # } )
  # 
  # 
  # extracted_df <- do.call(rbind, extracted_df)
  # 
  # #omit rows with NA
  # extracted_df <- stats::na.omit(extracted_df)
  # 
  # #library(tidyverse)
  # 
  # if('pr' %in% metric){
  #   pr_adj <- extracted_df %>% 
  #     reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
  #     dplyr::filter(.data$variable == 'pr') %>% 
  #     dplyr::mutate(value = round(.data$value * 60 * 60 * 24, digits = 2))
  # } else {
  #   pr_adj <- NULL
  # }
  # 
  # if('tasmin' %in% metric){
  #   tmin_adj <- extracted_df %>% 
  #     reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
  #     dplyr::filter(.data$variable == 'tasmin') %>% 
  #     dplyr::mutate(value = round(.data$value - 273.15, digits = 2))
  # } else {
  #   tmin_adj <- NULL
  # }
  # 
  # if('tasmax' %in% metric){
  #   tmax_adj <- extracted_df %>% 
  #     reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
  #     dplyr::filter(.data$variable == 'tasmax') %>% 
  #     dplyr::mutate(value = round(.data$value - 273.15, digits = 2))
  # } else {
  #   tmax_adj <- NULL
  # }
  # 
  # #in case the metric contains variable which are not tasmin, tasmax, pr
  # if(any(!metric %in% c('tasmin', 'tasmax', 'pr'))){
  #   other_adj <- extracted_df %>% 
  #     reshape2::melt(id.vars = c('Date', 'variable', 'model', 'ssp'), variable.name = 'id') %>% 
  #     filter(!.data$variable %in% c('tasmax', 'tasmin', 'pr'))
  # } else {
  #   other_adj <- NULL
  # }
  # 
  # #bind everything back together, bring back to long format
  # extracted_df <- rbind(tmin_adj, tmax_adj, pr_adj, other_adj) %>% 
  #   reshape2::dcast(formula = Date + variable + model + ssp ~ id)
  
  
  # if(keep_downloaded == FALSE){
  #   unlink(paste0(path_download,'/'), recursive = TRUE)
  # }
  
  # return(extracted)
#}


# extract_cmip_data <- function(fname, coords){
#   
#   #fname needs to be a character
#   assertthat::assert_that(is.character(fname))
#   
#   #coords needs to be a data.frame with column names
#   assertthat::assert_that(is.data.frame(coords))
#   assertthat::assert_that(all(c('Longitude', 'Latitude') %in% colnames(coords)))
#   
#   
#   #determine the name of the variable
#   #names can be pr, tasmin, and tasmax
#   fragment_fname <- strsplit(strsplit(fname, '/')[[1]][2], '_')[[1]]
#   #usual names in the file
#   weather_vars <- c('tasmin', 'tasmax', 'pr')
#   #findout which one is present here
#   dname <- weather_vars[weather_vars %in% fragment_fname]
#   
#   
#   Datatemp_raw <- purrr::map(1:nrow(coords), function(i){
#     metR::ReadNetCDF(fname, 
#                      vars = dname, 
#                      subset = list(lon = coords[i,"Longitude"], 
#                                    lat = coords[i,"Latitude"])) %>% 
#       dplyr::mutate(location = coords$id[i])
#   }) %>% 
#     dplyr::bind_rows() %>% 
#     stats::na.omit()
#   
#   
#   
#   
#   
#   
#   #load the file
#   b <- raster::brick(fname, dname)
#   
#   #get time data
#   time <- raster::getZ(b)
#   
#   #how to get info of NA
#   fillvalue <- ncdf4::ncatt_get(ncdf4::nc_open(fname), dname, "_FillValue")
#   #ncdf4::nc_close(fname)
#   raster::NAvalue(b) <- fillvalue$value
#   
#   #if longitude only defined as degree east
#   if(as.vector(raster::extent(b))[2] > 180){
#     
#     coords$Longitude[coords$Longitude < 0] <- coords$Longitude[coords$Longitude < 0] + 360
#   } 
#   
#   
#   extract.pts <- cbind(coords$Longitude,coords$Latitude)
#   ext <- raster::extract(b,extract.pts,method="bilinear")
#   
#   #transpose extracted data
#   ext <- t(ext)
#   
#   ext_df <- as.data.frame(ext, row.names = F)
#   
#   colnames(ext_df) <- 
#     
#     
#   colnames(ext_df) <- coords$id
#   ext_df$Date <- time
#   
#   return(ext_df)
# }
