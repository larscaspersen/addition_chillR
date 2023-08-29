#' Download CMIP6 Data via the ecwfr package
#' 
#' Accesses the CMIP6 data of the Coperincus API via the ecwfr package. Saves the downloaded files as
#' .zip objects in the specified path.
#' 
#' @param scenario character, specifying the shared socioeconomic pathways (SSP) 
#' to be downloaded. Currently the values 'ssp1_2_6', 'ssp2_4_5', 'ssp3_7_0' and 
#' 'ssp5_8_5' are the only legit options, which are also the standard scenarios 
#' of CMIP6
#' 
#' @param n_try numneric, number of repeated calls for the API. For more information see 'Details'
#' 
#' @param wait numeric, waiting in time between repeated function calls in seconds. For more
#' information see 'Details'
#' 
#' @param temporal_resolution character, can be either 'daily' or 'monthly'. Sets
#' if the downloaded CMIP6 data is in daily or monthly format
#' 
#' @param variable vector of characters, decides which variables get downloaded. 
#' Currently, the options "Tmin" (Daily minimum temperature in degree centigrade), 
#' "Tmax" (Daily maximum temperature in degree centigrade) 
#' and "Prec" (Daily sum of precipitation in mm) are the only valid options
#' 
#' @param year_start numeric, earliest year for downloaded CMIP6 data. By default set
#' to 2015
#' 
#' @param year_end numeric, latest year for downloaded CMIP6 data. By default set
#' to 2100
#' 
#' @param area numeric vector of length 4. Sets the spatial boundaries of the downloaded data.
#' Coordinates are supplied in the format:  c(maximum latitude, minimum longitude, minimum latitude, maximum longitude) which corresponds to the
#' northern extend, wester extend, southern extend and eastern extend of the area of interest
#' 
#' @param month numeric vector, sets which months should be downloaded. By default set to 1:12
#' 
#' @param update_everything logical, by default set to FALSE. When set FALSE already downloaded
#' scenarios with matching names are skipped. If set TRUE, then files are downloaded regardless if 
#' a file with the same name is already present
#' 
#' @param download_path character, sets the path for the download of the CMIP6 file. If not already present, then a 
#' new folder will be created. Path is relative to working directory
#' 
#' @return NULL, the downloaded files are saved in the stated directory
#' 
#' @details Sometimes the server is not responding in time and the download fails because of that.
#' To avoid that the same line of code needs to be executed several times until
#' all the needed scenarios are downloaded, the call will be repeated in these cases. The argument n_calls sets the
#' maximum number of repeated calls. The argument wait sets the waiting time between repeated function calls.
#' 
#' @author Lars Caspersen, Antonio Picornell
#' 
#' @examples 
#' \dontrun{
#' scenario<-c("ssp1_2_6", "ssp2_4_5", "ssp3_7_0", "ssp5_8_5")
#' purrr::map(scenario, download_cmip6_ecmwfr, area = c(52, -7, 33, 8), variable = c('Tmin', 'Tmax'))
#' }
#' @importFrom assertthat is.string
#' @importFrom assertthat is.number
#' @importFrom assertthat are_equal
#' @importFrom assertthat is.dir
#' @importFrom ecmwfr wf_set_key
#' @importFrom purrr compact
#' @importFrom ecmwfr wf_request_batch
#'  
#' @export download_cmip6_ecmwfr
#' 
download_cmip6_ecmwfr <- function(scenario, 
                               n_try = 20,
                               wait = 5,
                               temporal_resolution = 'monthly', 
                               variable = c('Tmin', 'Tmax'),
                               year_start = 2015, 
                               year_end = 2100, 
                               area = c(71, -15, 35, 50),
                               month = 1:12,
                               update_everything = FALSE,
                               download_path = 'cmip6_downloaded'){
  
  #check that arguments are sensible
  assertthat::is.string(scenario)
  assertthat::is.string(temporal_resolution)
  assertthat::is.number(n_try)
  assertthat::is.number(wait)
  assertthat::is.number(year_start)
  assertthat::is.number(year_end)
  assertthat::are_equal(length(area), 4)
  assertthat::is.dir(download_path)
  assertthat::assert_that(all(variable %in% c('Tmin', 'Tmax', 'Prec')))
  stopifnot(is.numeric((month)))
  stopifnot(is.numeric((area)))


  #here is the set of arguments that are only allowed for scnearios
  if(temporal_resolution == 'daily' & ! (scenario  %in% c('historical', 'ssp1_2_6', 'ssp2_4_5', 'ssp5_8_5'))){
    stop("When choosing daily resolution valid scenario are only c('historical', 'ssp1_2_6', 'ssp2_4_5', 'ssp5_8_5')")
  } else if(temporal_resolution == 'monthly' & !(scenario  %in% c('ssp1_2_6', 'ssp2_4_5','ssp3_7_0', 'ssp5_8_5'))){
    stop("When choosing monthly resolution valid scenario are only c('historical', 'ssp1_2_6', 'ssp2_4_5', 'ssp5_8_5')")
  }
  
  #counter fior 
  n <- 1 
  
  #the function is repeated because sometimes the api is not responding and we request has to be sent again
  repeat {
    tmp <- try(climate_extractor(scenario = scenario, 
                                 temporal_resolution = temporal_resolution,
                                 variable = variable,
                                 year_start = year_start,
                                 year_end = year_end,
                                 area = area,
                                 month = month,
                                 update_everything = update_everything,
                                 download_path = download_path))
    
    #error message when the wrong models were selected for the scenario
    #how should I handle that? make the models an argument? but usually we do not want to secify that...
    #grepl(pattern = 'No matching data for request', tmp)
    
    
    #if everything worked out then tmp is null, we can end the function
    if(is.null(tmp)){
      break
    }
    
    
    #if there is an error message that the api is not responding, try again, otherwise end function
    if(inherits(tmp, "try-error") & grepl('2100', tmp) | n < n_try ){
      
      #increase the counter
      n <- n + 1
      #wait a moment before repeating the download
      Sys.sleep(wait)
    } else {
      
      #in any other case: repeating the function won't help so we have to break anyways
      break
    }

      
    }
}


climate_extractor <- function(scenario, 
                              temporal_resolution = 'monthly', 
                              variable = c('Tmin', 'Tmax'),
                              year_start = 2015, 
                              year_end = 2100, 
                              area = c(71, -15, 35, 50),
                              month = 1:12,
                              update_everything = FALSE,
                              download_path = 'cmip6_downloaded'){
  
  ecmwfr::wf_set_key(user = '243306', key = 'cf909b0a-39cd-417f-89fa-198963d45ef7', service = 'cds')
  
  
  Models<-c()
  
  
  if(temporal_resolution == 'daily'){
    if(scenario == "historical"){
      Models<-c("access_cm2",       "awi_cm_1_1_mr" ,   "cmcc_esm2"     ,   "cnrm_cm6_1_hr"   ,
                "cnrm_cm6_1" ,      "cnrm_esm2_1"    ,  "fgoals_g3"    ,    "gfdl_esm4"    ,  "inm_cm4_8"       ,
                "inm_cm5_0"   ,     "kiost_esm"       , "miroc6"        ,   "miroc_es2l"   ,    "mpi_esm1_2_lr"   , "mri_esm2_0"      ,
                "noresm2_mm"      ,  "ec_earth3_cc"  ,   "ec_earth3_veg_lr"   ,
                "ipsl_cm6a_lr"  ,   "kace_1_0_g"       ,"ukesm1_0_ll"     , "canesm5"        ,  "nesm3"    )
    }
    if(scenario == "ssp1_2_6"){
      Models<-c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cmcc_esm2", "cnrm_cm6_1_hr",
                "cnrm_cm6_1", "cnrm_esm2_1", "fgoals_g3", "gfdl_esm4", "iitm_esm", "inm_cm4_8",
                "inm_cm5_0", "kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr", "mri_esm2_0",
                "noresm2_lm", "noresm2_mm")
    }
    if( scenario =="ssp2_4_5"){
      Models<-c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cesm2_waccm", "cmcc_esm2",
                "cnrm_cm6_1", "cnrm_esm2_1", "ec_earth3_cc", "ec_earth3_veg_lr", "giss_e2_1_g", "inm_cm4_8",
                "inm_cm5_0","ipsl_cm6a_lr", "kace_1_0_g","kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr",
                "mri_esm2_0","noresm2_lm", "noresm2_mm", "ukesm1_0_ll")
      
    }
    if(scenario =="ssp5_8_5"){
      Models<-c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "canesm5","cesm2", "cmcc_esm2",
                "cnrm_cm6_1", "cnrm_esm2_1", "ec_earth3_cc", "ec_earth3_veg_lr","gfdl_esm4", "inm_cm4_8",
                "inm_cm5_0", "kace_1_0_g","kiost_esm",
                "miroc6",
                "miroc_es2l",
                "mpi_esm1_2_lr",
                "mri_esm2_0","nesm3",
                "noresm2_lm",
                "noresm2_mm"
      )
    }
  }
  
  
  if(temporal_resolution == 'monthly'){
    
    if(scenario == 'ssp1_2_6'){
      Models <-  c('access_cm2', 'awi_cm_1_1_mr', 'bcc_csm2_mr', 'canesm5', 'cmcc_esm2',
                   'cnrm_cm6_1_hr', 'fio_esm_2_0', 
                   'inm_cm5_0', 'ipsl_cm6a_lr', 'miroc6', 'miroc_es2l', 'mri_esm2_0',
                   'cesm2','cnrm_cm6_1','cnrm_esm2_1','ec_earth3_veg_lr',
                   'fgoals_g3','gfdl_esm4','inm_cm4_8', 'mpi_esm1_2_lr',
                   'nesm3','ukesm1_0_ll')
    } else if(scenario == 'ssp2_4_5'){
      Models <-   c('access_cm2','awi_cm_1_1_mr','bcc_csm2_mr','cmcc_esm2',
                    'cnrm_cm6_1_hr','fio_esm_2_0', 
                    'inm_cm5_0','ipsl_cm6a_lr',  'miroc6','miroc_es2l', 'mri_esm2_0',
                    'cesm2','cnrm_cm6_1','cnrm_esm2_1',
                    'ec_earth3_cc', 'ec_earth3_veg_lr','fgoals_g3','gfdl_esm4',
                    'inm_cm4_8','mpi_esm1_2_lr','nesm3','ukesm1_0_ll')
    } else if(scenario == 'ssp3_7_0'){
      
      Models <- c('access_cm2','awi_cm_1_1_mr', 'bcc_csm2_mr',  'cnrm_cm6_1_hr',
                  'ec_earth3_aerchem',  'inm_cm5_0','ipsl_cm6a_lr',  'miroc6',
                  'miroc_es2l',    'mri_esm2_0','cesm2','cnrm_cm6_1',    'cnrm_esm2_1',
                  'ec_earth3_veg_lr',   'fgoals_g3','gfdl_esm4',  'inm_cm4_8',
                  'mpi_esm1_2_lr',    'ukesm1_0_ll')
    } else if(scenario == 'ssp5_8_5'){
      Models <- c('access_cm2', 'awi_cm_1_1_mr','bcc_csm2_mr',
                  'cmcc_esm2',  'cnrm_cm6_1_hr','fio_esm_2_0',
                  'inm_cm5_0',    'ipsl_cm6a_lr',  'miroc6',  'miroc_es2l',
                  'mri_esm2_0',   'cesm2','ciesm','cnrm_cm6_1',
                  'cnrm_esm2_1',    'ec_earth3_cc',    'ec_earth3_veg_lr',
                  'fgoals_g3',    'gfdl_esm4',    'inm_cm4_8',
                  'mpi_esm1_2_lr',    'nesm3',      'ukesm1_0_ll')
    }
    
    
    
  }
  
  
  #make sure the folder exists
  if(dir.exists(download_path) == FALSE){
    dir.create(download_path)
  }
  
  #make a batch of requests..........
  prec_request <- tmax_request <- tmin_request <- NULL
  
  
  if('Tmax' %in% variable){
    tmax_request <-   purrr::map(Models, function(mod){
      
      #file name
      #maybe add the area to the file name so that you can download for different areas but same scenario. this would not be possible right now
      fname <- paste0("tmax_",scenario,"_",mod, '_', paste0(area, collapse = '_'),".zip")
      
      #construct request
      request_max <- list(
        temporal_resolution = temporal_resolution,
        experiment = scenario,
        level = "single_levels",
        variable = "daily_maximum_near_surface_air_temperature",
        model = mod,
        year = as.character(year_start:year_end),
        month = as.character(month),
        area = area,
        format = "zip",
        dataset_short_name = "projections-cmip6",
        target = fname)
      
      #if already present return nothing
      if(fname %in% list.files(download_path) & update_everything == FALSE){
        cat(paste0('File ', fname, ' is already downloaded\n'))
        return(NULL)
      } else {
        #otherwise return the request
        return(request_max)
      }
      
    }) %>% 
      purrr::compact()
  }
  
  if('Tmin' %in% variable){
    tmin_request <-   purrr::map(Models, function(mod){
      
      
      
      #file name
      fname <- paste0("tmin_",scenario,"_",mod, '_',   paste0(area, collapse = '_'),".zip")
      
      #construct request
      request_min <- list(
        temporal_resolution = temporal_resolution,
        experiment = scenario,
        level = "single_levels",
        variable = "daily_minimum_near_surface_air_temperature",
        model = mod,
        year = as.character(year_start:year_end),
        month = as.character(month),
        area = area,
        format = "zip",
        dataset_short_name = "projections-cmip6",
        target = fname)
      
      #if already present return nothing
      if(fname %in% list.files(download_path) & update_everything == FALSE){
        cat(paste0('File ', fname, ' is already downloaded\n'))
        return(NULL)
      } else {
        #otherwise return the request
        return(request_min)
      }
      
    }) %>% 
      purrr::compact()
  }
  
  if('Prec' %in% variable){
    prec_request <-   purrr::map(Models, function(mod){
      
      #file name
      #maybe add the area to the file name so that you can download for different areas but same scenario. this would not be possible right now
      fname <- paste0("prec_",scenario,"_",mod, '_', paste0(area, collapse = '_'),".zip")
      
      #construct request
      request_prec <- list(
        temporal_resolution = temporal_resolution,
        experiment = scenario,
        level = "single_levels",
        variable = "precipitation",
        model = mod,
        year = as.character(year_start:year_end),
        month = as.character(month),
        area = area,
        format = "zip",
        dataset_short_name = "projections-cmip6",
        target = fname)
      
      #if already present return nothing
      if(fname %in% list.files(download_path) & update_everything == FALSE){
        cat(paste0('File ', fname, ' is already downloaded\n'))
        return(NULL)
      } else {
        #otherwise return the request
        return(request_prec)
      }
      
    }) %>% 
      purrr::compact()
  }
  
  #combine the requests
  request <- c(tmax_request, tmin_request, prec_request)
  
  #download the files
  ecmwfr::wf_request_batch(request,path = download_path)
  
}



