#' Download CMIP6 Data via the ecwfr package
#' 
#' Accesses the CMIP6 data of the Coperincus API via the \code{\link[ecmwfr:wf_request_batch]{ecmwfr}} package. Saves the downloaded files as
#' .zip objects in the specified path.
#' 
#' @param scenario character, specifying the shared socioeconomic pathways (SSP) 
#' to be downloaded. Currently the values 'ssp1_2_6', 'ssp2_4_5', 'ssp3_7_0' and 
#' 'ssp5_8_5' are the only legit options, which are also the standard scenarios 
#' of CMIP6
#' 
#' @param area numeric vector of length 4. Sets the spatial boundaries of the downloaded data.
#' Coordinates are supplied in the format:  c(maximum latitude, minimum longitude, minimum latitude, maximum longitude) which corresponds to the
#' northern extend, wester extend, southern extend and eastern extend of the area of interest
#' 
#' @param user a character, user name from the copernicus climate data store See the 'Details' for more 
#' 
#' @param key a character. Can be found just beneath the user id on the profile when registering 
#' for the copernicus website. Should be provided as a character (so in quotation marks)
#' 
#' @param model character, by default "default". Decides which global climate models are requested. If set to "default" then depening on the scenario and temporal 
#' resolution around 20 models are selected for which we know that combination of scenario and variables data is available. Can be also set to "all", then
#' all potential models are requested. You can also hand pick the models you want to download as a vector of the model names. You can check \url{https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip6?tab=form} for the list of models. 
#' In case a certain request fails because either the model name is wrong or the requested combination of SSP, time  period and variable is not available, then the model
#' is dropped from the requests and the function carries on with the remaining requests. The user will get a warning in these cases. 
#' 
#' @param serivce character, by default cds. Decides which database is used. For more details see in the documentation of \code{\link[ecmwfr:wf_set_key]{ecmwfr::wf_set_key()}}.
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
#' @param month numeric vector, sets which months should be downloaded. By default set to 1:12
#' 
#' @param n_try numeric, number of repeated calls for the API. For more information see 'Details'
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
#' @details Registering to cds.climate.coperincus.eu:
#' \url{https://cds.climate.copernicus.eu/cdsapp#!/home}
#' 
#' Finding the user id and the key:
#' 
#' On the website of the copernicus climate data store, navigate to the user profile and scroll to the bottom to "API key". There
#' you can find the item "UID". The user id should be provided as character (within quotation marks). Just below you can also find the key, which is also need when using this function.
#' 
#' After successful registration some extra steps are needed in order to be able to download CMIP6 data. 
#' Additional to the "Terms of use of the Copernicus Climate Store" and the "Data Protection
#' and privacy agreement", you also need to agree to the "CMIP6 - Data Access - Terms of Use". 
#' This needs to be done after registering. You can agree to the terms via the following link 
#' \url{https://cds.climate.copernicus.eu/cdsapp/#!/terms/cmip6-wps}. 
#' 
#' Alternatively,
#' you can navigate to the term within the copernicus webpage. Go to "Datasets", you can find it in the upper ribbon of the main page. 
#' There you need to search for "CMIP6" with the search field
#' and choose the first result, which is named "CMIP6 climate projections". 
#' There you need to click on "Download data" and scroll to the very bottom of the page to the
#' field "Terms of Use". There you need to click on the button saying "Accept Terms".
#' If you do not accept the terms the download via the API (and consequently via this
#' function) will not be possible!
#' 
#' 
#' Sometimes the server is not responding in time and the download fails because of that. In these cases after a short 
#' waiting time of 5 seconds, the request is started again. If the error repeats several times the requested model
#' will be dropped from the list of requests. By default the number of allowed repeated requestes is 10. The user 
#' will get a warning if the model is dropped from the requests. 
#' 
#' @author Lars Caspersen, Antonio Picornell
#' 
#' @examples 
#' \dontrun{
#' download_cmip6_ecmwfr(scenario = 'ssp1_2_6', 
#' area = c(55, 5.5, 47, 15.1),
#' user = 'write user id here'
#' key = 'write key here',
#' model = 'AWI-CM-1-1-MR',
#' temporal_resolution = 'monthly', 
#' variable = c('Tmin', 'Tmax'),
#' year_start = 2015, 
#' year_end = 2100)
#' }
#' @importFrom assertthat is.string
#' @importFrom assertthat is.number
#' @importFrom assertthat are_equal
#' @importFrom assertthat is.dir
#' @importFrom assertthat assert_that
#' @importFrom ecmwfr wf_set_key
#' @importFrom purrr compact
#' @importFrom ecmwfr wf_request_batch
#'  
#' @export download_cmip6_ecmwfr
#' 
download_cmip6_ecmwfr <- function(scenario, 
                                  area,
                                  user,
                                  key,
                                  model =  'default',
                                  service = 'cds',
                                  temporal_resolution = 'monthly', 
                                  variable = c('Tmin', 'Tmax'),
                                  year_start = 2015, 
                                  year_end = 2100, 
                                  month = 1:12,
                                  n_try = 10,
                                  update_everything = FALSE,
                                  download_path = 'cmip6_downloaded'){
  
  #---------------------------#
  #check inputs
  #---------------------------#
  
  
  #check that arguments are sensible
  assertthat::assert_that(is.character(scenario))
  assertthat::is.string(user)
  assertthat::is.string(key)
  assertthat::is.string(temporal_resolution)
  assertthat::is.number(n_try)
  assertthat::is.number(year_start)
  assertthat::is.number(year_end)
  assertthat::are_equal(length(area), 4)
  assertthat::assert_that(is.character(download_path))
  
  #make sure the folder exists
  if(dir.exists(download_path) == FALSE){
    dir.create(download_path)
  }
  assertthat::is.dir(download_path)
  
  
  assertthat::assert_that(all(variable %in% c('Tmin', 'Tmax', 'Prec')))
  stopifnot(is.numeric((month)))
  stopifnot(is.numeric((area)))
  assertthat::assert_that(is.character(model))
  
  #need to check the area on certain things
  #first and third entry must be in -90, and 90
  assertthat::assert_that(area[1] <= 90 & area[1] >= -90)
  assertthat::assert_that(area[3] <= 90 & area[3] >= -90)
  #first entry must be larger than third
  assertthat::assert_that(area[1] > area[3])
  #second and fourht entry must be in -180 to 180
  assertthat::assert_that(area[2] <= 180 & area[2] >= -180)
  assertthat::assert_that(area[4] <= 180 & area[4] >= -180)
  #second entry must be smaller than fourth
  assertthat::assert_that(area[2] < area[4])
  
  
  
  #set key for download
  ecmwfr::wf_set_key(user = user, key = key, service = service)
  
  
  #------------------------------------
  #select gcms
  #------------------------------------
  
  #placeholder
  Models <- NULL
  
  #waiting time in seconds when api does not respond
  wait <- 5
  
  #standard selection
  default_gcm_list <- list('daily' = list('historical' = c("access_cm2",       "awi_cm_1_1_mr" ,   "cmcc_esm2"     ,   "cnrm_cm6_1_hr"   ,
                                                           "cnrm_cm6_1" ,      "cnrm_esm2_1"    ,  "fgoals_g3"    ,    "gfdl_esm4"    ,  "inm_cm4_8"       ,
                                                           "inm_cm5_0"   ,     "kiost_esm"       , "miroc6"        ,   "miroc_es2l"   ,    "mpi_esm1_2_lr"   , "mri_esm2_0"      ,
                                                           "noresm2_mm"      ,  "ec_earth3_cc"  ,   "ec_earth3_veg_lr"   ,
                                                           "ipsl_cm6a_lr"  ,   "kace_1_0_g"       ,"ukesm1_0_ll"     , "canesm5"        ,  "nesm3"    ),
                                          'ssp1_2_6' = c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cmcc_esm2", "cnrm_cm6_1_hr",
                                                         "cnrm_cm6_1", "cnrm_esm2_1", "fgoals_g3", "gfdl_esm4", "iitm_esm", "inm_cm4_8",
                                                         "inm_cm5_0", "kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr", "mri_esm2_0",
                                                         "noresm2_lm", "noresm2_mm"),
                                          'ssp2_4_5' = c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cesm2_waccm", "cmcc_esm2",
                                                         "cnrm_cm6_1", "cnrm_esm2_1", "ec_earth3_cc", "ec_earth3_veg_lr", "giss_e2_1_g", "inm_cm4_8",
                                                         "inm_cm5_0","ipsl_cm6a_lr", "kace_1_0_g","kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr",
                                                         "mri_esm2_0","noresm2_lm", "noresm2_mm", "ukesm1_0_ll"),
                                          'ssp5_8_5' = c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "canesm5","cesm2", "cmcc_esm2",
                                                         "cnrm_cm6_1", "cnrm_esm2_1", "ec_earth3_cc", "ec_earth3_veg_lr","gfdl_esm4", "inm_cm4_8",
                                                         "inm_cm5_0", "kace_1_0_g","kiost_esm",
                                                         "miroc6", "miroc_es2l", "mpi_esm1_2_lr", "mri_esm2_0","nesm3", "noresm2_lm", "noresm2_mm")
  ),
  'monthly' = list('ssp1_2_6' = c('access_cm2', 'awi_cm_1_1_mr', 'bcc_csm2_mr', 'canesm5', 'cmcc_esm2',
                                  'cnrm_cm6_1_hr', 'fio_esm_2_0', 
                                  'inm_cm5_0', 'ipsl_cm6a_lr', 'miroc6', 'miroc_es2l', 'mri_esm2_0',
                                  'cesm2','cnrm_cm6_1','cnrm_esm2_1','ec_earth3_veg_lr',
                                  'fgoals_g3','gfdl_esm4','inm_cm4_8', 'mpi_esm1_2_lr',
                                  'nesm3','ukesm1_0_ll'),
                   'ssp2_4_5' = c('access_cm2','awi_cm_1_1_mr','bcc_csm2_mr','cmcc_esm2',
                                  'cnrm_cm6_1_hr','fio_esm_2_0', 
                                  'inm_cm5_0','ipsl_cm6a_lr',  'miroc6','miroc_es2l', 'mri_esm2_0',
                                  'cesm2','cnrm_cm6_1','cnrm_esm2_1',
                                  'ec_earth3_cc', 'ec_earth3_veg_lr','fgoals_g3','gfdl_esm4',
                                  'inm_cm4_8','mpi_esm1_2_lr','nesm3','ukesm1_0_ll'),
                   'sp3_7_0' = c('access_cm2','awi_cm_1_1_mr', 'bcc_csm2_mr',  'cnrm_cm6_1_hr',
                                 'ec_earth3_aerchem',  'inm_cm5_0','ipsl_cm6a_lr',  'miroc6',
                                 'miroc_es2l',    'mri_esm2_0','cesm2','cnrm_cm6_1',    'cnrm_esm2_1',
                                 'ec_earth3_veg_lr',   'fgoals_g3','gfdl_esm4',  'inm_cm4_8',
                                 'mpi_esm1_2_lr',    'ukesm1_0_ll'),
                   'ssp5_8_5' = c('access_cm2', 'awi_cm_1_1_mr','bcc_csm2_mr',
                                  'cmcc_esm2',  'cnrm_cm6_1_hr','fio_esm_2_0',
                                  'inm_cm5_0',    'ipsl_cm6a_lr',  'miroc6',  'miroc_es2l',
                                  'mri_esm2_0',   'cesm2','ciesm','cnrm_cm6_1',
                                  'cnrm_esm2_1',    'ec_earth3_cc',    'ec_earth3_veg_lr',
                                  'fgoals_g3',    'gfdl_esm4',    'inm_cm4_8',
                                  'mpi_esm1_2_lr',    'nesm3',      'ukesm1_0_ll'))
  )
  
  #in case it is default
  if(length(model) == 1){
    if(model == 'default'){
      
      Models <- default_gcm_list[[temporal_resolution]][[scenario]] 
    }
    
    #last checked 1st sept 2023
    #maybe also have a colum with the complete name as on the website, because people probably do not
    #know the api name
    if(model == 'all'){
      Models <- c('access_cm2', 'awi_cm_1_1_mr', 'bcc_csm2_mr', 'cams_csm1_0',
                  'canesm5_canoe', 'cesm2_fv2', 'cesm2_waccm_fv2', 'cmcc_cm2_hr4',
                  'cmcc_esm2', 'cnrm_cm6_1_hr', 'e3sm_1_0', 'e3sm_1_1_eca',
                  'ec_earth3_aerchem', 'ec_earth3_veg', 'fgoals_f3_l',
                  'fio_esm_2_0', 'giss_e2_1_g', 'hadgem3_gc31_ll',
                  'iitm_esm', 'inm_cm5_0', 'ipsl_cm6a_lr', 'kiost_esm',
                  'miroc6', 'miroc_es2l', 'mpi_esm1_2_hr', 'mri_esm2_0',
                  
                  'norcpm1', 'noresm2_mm', 'taiesm1', 
                  'access_esm1_5', 'awi_esm_1_1_lr', 'bcc_esm1', 'canesm5',
                  'cesm2', 'cesm2_waccm', 'ciesm', 'cmcc_cm2_sr5',
                  'cnrm_cm6_1', 'cnrm_esm2_1', 'e3sm-1_1', 'ec_earth3',
                  'ec_earth3_cc', 'ec_earth3_veg_lr', 'fgoals_g3', 'gfdl_esm4',
                  'giss_e2_1_h', 'hadgem3_gc31_mm', 'inm_cm4_8', 'ipsl_cm5a2_inca',
                  'kace_1_0_g', 'mcm_ua_1_0', 'miroc_es2h', 'mpi_esm_1_2_ham', 
                  'mpi_esm1_2_lr', 'nesm3', 'noresm2_lm', 'sam0_unicon',
                  'ukesm1_0_ll')
      
    }
  }
  
  #match the model, then take the api name
  gcm_lookup_df <- data.frame(model_name =       c('ACCESS-CM2','ACCESS-ESM1-5', 'AWI-CM-1-1-MR',      'AWI-ESM-1-1-LR',
                                                   'BCC-CSM2-MR',      'BCC-ESM1',      'CAMS-CSM1-0',      'CanESM5',
                                                   'CanESM5-CanOE',      'CESM2',      'CESM2-FV2',      'CESM2-WACCM',
                                                   'CESM2-WACCM-FV2',      'CIESM',      'CMCC-CM2-HR4',      'CMCC-CM2-SR5',
                                                   'CMCC-ESM2',      'CNRM-CM6-1',      'CNRM-CM6-1-HR',      'CNRM-ESM2-1',
                                                   'E3SM-1-0',      'E3SM-1-1',      'E3SM-1-1-ECA',      'EC-Earth3',
                                                   'EC-Earth3-AerChem',      'EC-Earth3-CC',      'EC-Earth3-Veg',
                                                   'EC-Earth3-Veg-LR',      'FGOALS-f3-L',      'FGOALS-g3',
                                                   'FIO-ESM-2-0',      'GFDL-ESM4',      'GISS-E2-1-G',      'GISS-E2-1-H',
                                                   'HadGEM3-GC31-LL',      'HadGEM3-GC31-MM',      'IITM-ESM',
                                                   'INM-CM4-8',      'INM-CM5-0',      'IPSL-CM5A2-INCA',      'IPSL-CM6A-LR',
                                                   'KACE-1-0-G',      'KIOST-ESM',      'MCM-UA-1-0',      'MIROC6',
                                                   'MIROC-ES2H',      'MIROC-ES2L',      'MPI-ESM-1-2-HAM',      'MPI-ESM1-2-HR',
                                                   'MPI-ESM1-2-LR',      'MRI-ESM2-0',      'NESM3',      'NorCPM1',
                                                   'NorESM2-LM',      'NorESM2-MM',      'SAM0-UNICON',      'TaiESM1',
                                                   'UKESM1-0-LL' ),
                              api_name = c('access_cm2', 'access_esm1_5', 
                                           'awi_cm_1_1_mr', 'awi_esm_1_1_lr', 
                                           'bcc_csm2_mr', 'bcc_esm1',
                                           'cams_csm1_0','canesm5',
                                           'canesm5_canoe', 'cesm2', 
                                           'cesm2_fv2',  'cesm2_waccm', 
                                           'cesm2_waccm_fv2', 'ciesm', 
                                           'cmcc_cm2_hr4','cmcc_cm2_sr5',
                                           'cmcc_esm2', 'cnrm_cm6_1',
                                           'cnrm_cm6_1_hr',   'cnrm_esm2_1',
                                           'e3sm_1_0',  'e3sm-1_1', 
                                           'e3sm_1_1_eca', 'ec_earth3',
                                           'ec_earth3_aerchem',   'ec_earth3_cc',
                                           'ec_earth3_veg',  'ec_earth3_veg_lr',
                                           'fgoals_f3_l',  'fgoals_g3',
                                           'fio_esm_2_0',  'gfdl_esm4',
                                           'giss_e2_1_g',  'giss_e2_1_h',
                                           'hadgem3_gc31_ll', 'hadgem3_gc31_mm', 
                                           'iitm_esm',  'inm_cm4_8', 
                                           'inm_cm5_0',   'ipsl_cm5a2_inca',
                                           'ipsl_cm6a_lr',  'kace_1_0_g', 
                                           'kiost_esm', 'mcm_ua_1_0', 
                                           'miroc6',   'miroc_es2h', 
                                           'miroc_es2l',  'mpi_esm_1_2_ham', 
                                           'mpi_esm1_2_hr', 'mpi_esm1_2_lr', 
                                           'mri_esm2_0', 'nesm3', 
                                           'norcpm1',  'noresm2_lm', 
                                           'noresm2_mm',  'sam0_unicon',
                                           'taiesm1',   'ukesm1_0_ll'))
  
  #in all other cases take the user selection
  if(is.null(Models)){
    
    #user selection is probably the names on the website, but I need to tranlsate the name to the api request
    match_row <- match(model, gcm_lookup_df$model_name)
    
    Models <- gcm_lookup_df$api_name[match_row]
    
    #if the match was not successfull, then take the original entry and try it with that one
    if(any(is.na(Models))){
      warning(paste('At least one of the provided model name did match any of the known GCM model names. The function will try to download it anyway, but it is likely to fail. Please check if there are no typos in the GCMs name.\nThis affects the provided model names:', 
                    paste0(model[is.na(match_row)], collapse = ' ') )
      )
      
      #in case there are no matches in the lookup, then use for these the raw user input
      Models[is.na(match_row)] <- model[is.na(match_row)]
    }
    
    
  }
  
  

  #-----------------------------------
  #generate the request for the api
  #-----------------------------------
  
  #make a batch of requests..........
  prec_request <- tmax_request <- tmin_request <- NULL
  
  
  if('Tmax' %in% variable){
    tmax_request <-   purrr::map(Models, function(mod){
      
      #file name
      #maybe add the area to the file name so that you can download for different areas but same scenario. this would not be possible right now
      fname <- paste0("tmax_",scenario,"_",mod, '_',  temporal_resolution, '_' ,paste0(area, collapse = '_'),".zip")
      
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
      fname <- paste0("tmin_",scenario,"_",mod, '_', temporal_resolution, '_' ,  paste0(area, collapse = '_'),".zip")
      
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
      fname <- paste0("prec_",scenario,"_",mod, '_', temporal_resolution, '_' ,paste0(area, collapse = '_'),".zip")
      
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
  
  
  
  #-------------------------------------#
  #run the download
  #-------------------------------------#
  
  #cycle through the requests, if a request was failed for a model, then drop the model from the requests
  
  
  #check for wrong weather stations
  run_request <- TRUE
  send_warning_message_missing_model <- send_warning_message_2100 <- send_warning_message_no_points <- FALSE
  dropped_model_names <- c()
  dropped_model_names_no_points <- c()
  dropped_model_names_2100 <- c()
  n <- 1
  model_2100_before <- ''
  model_2100_current <- ''
  
  
  while(run_request){
    tmp <- try(  ecmwfr::wf_request_batch(request_list = request,
                                          path = download_path, 
                                          user = user), silent = TRUE)
    
    #allows to end the while loop
    run_request <- FALSE
    
    #check if there is an error
    if(inherits(tmp, "try-error")){
      
      #users did not agree to terms and condtions of cmip6 dataset
      if(grepl("Client has not agreed to the required terms and conditions", tmp)){
        stop("User did not agree to the required terms and conditions to download CMIP6 data.\nPlease go to 'https://cds.climate.copernicus.eu/cdsapp/#!/terms/cmip6-wps' or see the Details section of the download_cmip6_ecmwfr function")
      }
      
      #if it contains the error message that the No matching data for request
      else if(grepl('No matching data for request', tmp)){
        
        #send warning message at the end
        send_warning_message_missing_model <- TRUE
        
        #------#
        #extract model name from error message   #
        #------#
        
        #then kick this station out and try again
        split_error <- strsplit(tmp, '\\{')
        
        #take second option
        split_again <- strsplit(split_error[[1]][2], ',')
        
        model_fragment <- split_again[[1]][grep('model', split_again[[1]])]
        
        model_name <- gsub(pattern = " 'model': ", replacement = '', model_fragment)
        model_name <- gsub("'", '', model_name)
        
        
        dropped_model_names <- c(dropped_model_names, model_name)
        
        #---------#
        #remove the model name from the Models argument   #
        #---------#
        
        #get the names of the requested models
        request_models <- purrr::map_chr(request, 'model')
        
        #translate the name if no match
        
        #if name is not in the requested model names, then use the lookup table
        if((model_name %in% request_models) == FALSE){
          drop_request <- which(request_models %in% gcm_lookup_df$api_name[gcm_lookup_df$model_name == model_name])
        } else {
          drop_request <- which(request_models %in% model_name)
        }
        
        request <- request[-drop_request]
        
        #check which files are already downloaded and drop them from the list, too
        
        already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(download_path))
        if(length(already_downloaded) != 0){
          request <- request[-already_downloaded]
        }
        
        #if there are remaining requests, then run it again
        if(length(request) > 0){
          run_request <- TRUE
        }
        
        
      }
      
      
      #in case of this error message, also remove the stations
      else if(grepl('There were no valid data points found in the requested subset', tmp)){
        
        #flag to send later on warning message
        send_warning_message_no_points <- TRUE
        
        #extract the model which caused the problem
        dropped_model_names_no_points <- c(dropped_model_names_no_points, request[[1]]$model)
        
        #drop the model from the requests
        drop_request <- which(purrr::map_chr(request, 'model') %in% dropped_model_names_no_points)
        request <- request[-drop_request]
        
        #also drop already downloaded files from the request
        already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(download_path))
        if(length(already_downloaded) != 0){
          request <- request[-already_downloaded]
        }
        
        #if there are remaining requests, then run it again
        if(length(request) > 0){
          run_request <- TRUE
        }
      }
      
      #in case of failure of website to answer, try again after some seconds
      #if it fails to answer too often for the same model, then skip it
      else if(grepl('Process error: 2100', tmp)){
        
        already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(download_path))
        if(length(already_downloaded) != 0){
          request <- request[-already_downloaded]
        }
        
        
        #this controls a counter, if we fail to download a station because of that error for too often, then we will drop the station
        model_2100_before <- model_2100_current
        model_2100_current <- request[[1]]$model
        
        
        if(model_2100_before == model_2100_current){
          n <- n + 1
        } else {
          n <- 1
        }
        
        if(n < n_try){
          #wait a moment before repeating the download
          Sys.sleep(wait)
          run_request <- TRUE
        } else {
          
          #flag to send later warning message
          send_warning_message_2100 <- TRUE
          
          dropped_model_names_2100 <- c(dropped_model_names_2100, request[[1]]$model)
          
          #drop the model from the requests
          drop_request <- which(purrr::map_chr(request, 'model') %in% dropped_model_names_2100)
          request <- request[-drop_request]
          
          #only run again if there is something remaining
          if(length(request) > 0){
            run_request <- TRUE
          }
        }
        
        
        
      } else {
        #drop already downloaded files from the request
        already_downloaded <- which(purrr::map_chr(request, 'target') %in% list.files(download_path))
        if(length(already_downloaded) != 0){
          request <- request[-already_downloaded]
        }
        
        modelname_warning_message <- gcm_lookup_df$model_name[gcm_lookup_df$api_name %in% request[[1]]$model]
        
        #error message which I have not encountered yet
        stop(paste0('Unknown error when downloading the CMIP6 data. Error happended for model ', modelname_warning_message,'. \nConsider dropping that model from the vector of models and try running the function again.\nHere is the original error message:\n \n', tmp))
      }
      
    }
  }
  
  if(send_warning_message_missing_model){
    warning(paste0('Dropped model: ', paste0(dropped_model_names, collapse = ', '), ' from the request, because the requested combination of\n SSP: ', scenario, ', Model: ', paste0(dropped_model_names, collapse = ', '), ', and Variables: ', paste0(variable, collapse = ', '),
                   ' does not exist.'))
  }
  if(send_warning_message_no_points){
    
    modelname_warning_message <- gcm_lookup_df$model_name[gcm_lookup_df$api_name %in% dropped_model_names_no_points]
    
    warning(paste0('Dropped model: ', paste0(modelname_warning_message, collapse = ', '), ' from the request, because the requested area and / or time period was not covered by the models'))
  }
  if(send_warning_message_2100){
    
    modelname_warning_message <- gcm_lookup_df$model_name[gcm_lookup_df$api_name %in% dropped_model_names_2100]
    
    warning(paste0('Dropped model: ', paste0(modelname_warning_message, collapse = ', '), ' from the request, the server failed to answer to that request too often.'))
  }
  
}


?download_cmip6_ecmwfr
library(tidyverse)
scenario <- 'ssp5_8_5'
download_cmip6_ecmwfr(scenario = scenario,
                      area = c(55, 5.5, 47, 15.1),
                      user = '243306',
                      key = 'cf909b0a-39cd-417f-89fa-198963d45ef7',
                      model = 'AWI-CM-1-1-MR',
                      temporal_resolution = 'monthly',
                      variable = c('Tmin', 'Tmax'),
                      year_start = 2015,
                      year_end = 2100)


