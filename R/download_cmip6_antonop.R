# library(tidyverse)
#download cmip6 the way antonio does it


climate_extractor <- function(Scenarios, 
                              temporal_resolution = 'daily', 
                              year_start = 2015, 
                              year_end = 2100, 
                              area = c(71, -15, 35, 50),
                              month = 1:12,
                              update_everything = FALSE,
                              download_path = 'cmip6_downloaded'){
  
  ecmwfr::wf_set_key(user = '243306', key = 'cf909b0a-39cd-417f-89fa-198963d45ef7', service = 'cds')
  
  
  Models<-c()
  
  if(temporal_resolution == 'daily' & ! (Scenarios  %in% c('historical', 'ssp1_2_6', 'ssp2_4_5', 'ssp5_8_5'))){
    stop("When choosing daily resolution valid Scenarios are only c('historical', 'ssp1_2_6', 'ssp2_4_5', 'ssp5_8_5')")
  } else if(temporal_resolution == 'monthly' & !(Scenarios  %in% c('ssp1_2_6', 'ssp2_4_5','ssp_3_7_0', 'ssp5_8_5'))){
    stop("When choosing monthly resolution valid Scenarios are only c('historical', 'ssp1_2_6', 'ssp2_4_5', 'ssp5_8_5')")
  }
  
  if(temporal_resolution == 'daily'){
    if(Scenarios == "historical"){
      Models<-c("access_cm2",       "awi_cm_1_1_mr" ,   "cmcc_esm2"     ,   "cnrm_cm6_1_hr"   ,
                "cnrm_cm6_1" ,      "cnrm_esm2_1"    ,  "fgoals_g3"    ,    "gfdl_esm4"    ,  "inm_cm4_8"       ,
                "inm_cm5_0"   ,     "kiost_esm"       , "miroc6"        ,   "miroc_es2l"   ,    "mpi_esm1_2_lr"   , "mri_esm2_0"      ,
                "noresm2_mm"      ,  "ec_earth3_cc"  ,   "ec_earth3_veg_lr"   ,
                "ipsl_cm6a_lr"  ,   "kace_1_0_g"       ,"ukesm1_0_ll"     , "canesm5"        ,  "nesm3"    )
    }
    if(Scenarios == "ssp1_2_6"){
      Models<-c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cmcc_esm2", "cnrm_cm6_1_hr",
                "cnrm_cm6_1", "cnrm_esm2_1", "fgoals_g3", "gfdl_esm4", "iitm_esm", "inm_cm4_8",
                "inm_cm5_0", "kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr", "mri_esm2_0",
                "noresm2_lm", "noresm2_mm")
    }
    if( Scenarios =="ssp2_4_5"){
      Models<-c("access_cm2", "awi_cm_1_1_mr", "bcc_csm2_mr", "cesm2", "cesm2_waccm", "cmcc_esm2",
                "cnrm_cm6_1", "cnrm_esm2_1", "ec_earth3_cc", "ec_earth3_veg_lr", "giss_e2_1_g", "inm_cm4_8",
                "inm_cm5_0","ipsl_cm6a_lr", "kace_1_0_g","kiost_esm","miroc6", "miroc_es2l", "mpi_esm1_2_lr",
                "mri_esm2_0","noresm2_lm", "noresm2_mm", "ukesm1_0_ll")
      
    }
    if(Scenarios =="ssp5_8_5"){
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
    
    if(Scenarios == 'ssp1_2_6'){
      Models <-  c('access_cm2', 'awi_cm_1_1_mr', 'bcc_csm2_mr', 'canesm5', 'cmcc_esm2',
                   'cnrm_cm6_1_hr', 'fio_esm_2_0', 
                   'inm_cm5_0', 'ipsl_cm6a_lr', 'miroc6', 'miroc_es2l', 'mri_esm2_0',
                    'cesm2','ciesm','cnrm_cm6_1','cnrm_esm2_1','ec_earth3_veg_lr',
                   'fgoals_g3','gfdl_esm4','inm_cm4_8', 'mpi_esm1_2_lr',
                   'nesm3','ukesm1_0_ll')
    } else if(Scenarios == 'ssp2_4_5'){
      Models <-   c('access_cm2','awi_cm_1_1_mr','bcc_csm2_mr','cmcc_esm2',
                    'cnrm_cm6_1_hr','fio_esm_2_0', 
                    'inm_cm5_0','ipsl_cm6a_lr',  'miroc6','miroc_es2l', 'mri_esm2_0',
                    'cesm2','cesm2_waccm', 'ciesm','cnrm_cm6_1','cnrm_esm2_1',
                    'ec_earth3_cc', 'ec_earth3_veg_lr','fgoals_g3','gfdl_esm4',
                    'inm_cm4_8','mpi_esm1_2_lr','nesm3','ukesm1_0_ll')
    } else if(Scenarios == 'ssp3_7_0'){
      
      Models <- c('access_cm2','awi_cm_1_1_mr', 'bcc_csm2_mr',  'canesm5',  'cnrm_cm6_1_hr',
                  'ec_earth3_aerchem',  'inm_cm5_0','ipsl_cm6a_lr',  'miroc6',
                  'miroc_es2l',    'mri_esm2_0','cesm2','cnrm_cm6_1',    'cnrm_esm2_1',
                  'ec_earth3_veg_lr',   'fgoals_g3','gfdl_esm4',  'inm_cm4_8',
                  'mpi_esm1_2_lr',    'ukesm1_0_ll')
    } else if(Models == 'ssp5_8_5'){
      Models <- c('access_cm2', 'awi_cm_1_1_mr','bcc_csm2_mr',
                  'cmcc_esm2',  'cnrm_cm6_1_hr','fio_esm_2_0',
                  'inm_cm5_0',    'ipsl_cm6a_lr',  'miroc6',  'miroc_es2l',
                  'mri_esm2_0',   'canesm5',  'cesm2','ciesm','cnrm_cm6_1',
                  'cnrm_esm2_1',    'ec_earth3_cc',    'ec_earth3_veg_lr',
                  'fgoals_g3',    'gfdl_esm4',    'hadgem3_gc31_11',    'inm_cm4_8',
                  'mpi_esm1_2_lr',    'nesm3',      'ukesm1_0_ll')
    }
    
    
    
  }
  
  
  #make sure the folder exists
  if(dir.exists(download_path) == FALSE){
    dir.create(download_path)
  }
  
  #make a batch of requests..........
  
  
  tmax_request <-   purrr::map(Models, function(mod){
    
    #file name
    fname <- paste0("tmax_",Scenarios,"_",mod,".zip")
    
    #construct request
    request_max <- list(
      temporal_resolution = temporal_resolution,
      experiment = Scenarios,
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
    compact()
  
  tmin_request <-   purrr::map(Models, function(mod){
    
    #file name
    fname <- paste0("tmin_",Scenarios,"_",mod,".zip")
    
    #construct request
    request_min <- list(
      temporal_resolution = temporal_resolution,
      experiment = Scenarios,
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
    compact()
  
  request <- c(tmax_request, tmin_request)
  
  ecmwfr::wf_request_batch(request,path = download_path, )
  
}

run_download_cmip6 <- function(Scenarios, 
                               n_try = 20,
                               wait = 5,
                               temporal_resolution = 'daily', 
                               year_start = 2015, 
                               year_end = 2100, 
                               area = c(71, -15, 35, 50),
                               month = 1:12,
                               update_everything = FALSE,
                               download_path = 'cmip6_downloaded'){
  n <- 1 
  
  repeat {
    tmp <- try(climate_extractor(Scenarios = Scenarios, 
                                 temporal_resolution = temporal_resolution,
                                 year_start = year_start,
                                 year_end = year_end,
                                 area = area,
                                 month = month,
                                 update_everything = update_everything,
                                 download_path = download_path))
    #increase the counter
    n <- n + 1
    
    #options to break the repetitions
    if(!inherits(tmp, "try-error") | n >= n_try){
      break
    } else {
      
      #wait some seconds before douing the next request
      Sys.sleep(wait)
        
      }
      
    }
  }
  

extract_cmip6_data_antonio <- function(stations,
                                       download_path = 'cmip6_downloaded'){
  
  #need to transform the latitude because netcdf latitude only goes from 0 to 360
  stations$longitude<-stations$longitude+360
  

  fnames <- list.files(download_path, full.names = TRUE)
  
  all_weather <- purrr::map(fnames, function(f){
    
    #look at the zipped files, find the ncdf
    take_f <- grep(pattern =  '\\.nc$', unzip(f, list = TRUE)$Name) 
    unzip_f <- unzip(f, list = TRUE)$Name[take_f]
    
    #extract the information from the file name
    split_name <-   unzip_f %>% 
      str_split('_')
    
    variable <- split_name[[1]][1]
    gcm <- split_name[[1]][3]
    ssp <- split_name[[1]][4]
    
    #read for all stations
    Datatemp_REM <- purrr::map(1:length(stations), function(i){
      metR::ReadNetCDF(unzip(f[1], unzip_f), 
                       vars = variable, 
                       subset = list(lon = stations[i,"longitude"], 
                                     lat=stations[i,"latitude"])) %>% 
        mutate(location = stations$station_name[i])
    }) %>% 
      bind_rows()

    #rename the file extracted value column
    colnames(Datatemp_REM)[4]<-"value"
    

    Datatemp <- Datatemp_REM %>% 
      as.data.frame() %>% 
      mutate(value = value - 273.15,
             variable = variable,
             model = gcm,
             ssp = ssp,
             Month = lubridate::month(time),
             Year = lubridate::year(time),
             Date = lubridate::date(time),
             Day = lubridate::day(time)) %>% 
      dplyr::select(Date, Year, Month, Day, lat, lon, location, variable, model, ssp, value)
    
    if(nrow(Datatemp)<1){print(paste("WARNING!!!!!!:", ssp, variable, model))}
    
    return(Datatemp)
  })
  
  
  return(bind_rows(all_weather) %>% 
    reshape2::dcast(Date + Year + Month + Day + lon  + lat + location + model + ssp ~ variable, value.var = 'value') %>% 
    rename(Tmin = tasmin,
           Tmax = tasmax) %>% 
    split(~ location) %>% 
    purrr::map(.f = function(x) split(x, list(x$ssp, x$model))))
  
}



#higher-level function to create temperature scenarios
gen_rel_change_scenario <- function(downloaded_list, 
                                    weather_list,
                                    years_local_weather = NULL,
                                    times = c(2050, 2085), 
                                    baseline_year_relative_change = 2022,
                                    baseline_window_width = 15, 
                                    future_window_width = 31){
  
  
  #iterate over the different weather stations (locally observed data and climate change data)
  purrr::map2(downloaded_list, weather_list, function(x, y){
    
    # x <- downloaded_list[[1]]
    # y <- weather_list[[1]]
    # 
    if(is.null(years_local_weather)){
      #calculate the mean year of observation
      reference_year <- mean(c(min(y$Year), max(y$Year)))
    } else {
      reference_year <- mean(years_local_weather)
    }
    
    
    #run the function on the list of scenarios for one station
    out <- create_scenario_list(cmip6_one_station = x, 
                                reference_year = reference_year,
                                times = times,
                                baseline_year_relative_change = baseline_year_relative_change,
                                baseline_window_width = baseline_window_width, 
                                future_window_width = future_window_width)
  })
  
}

#lower level function to generate temperature scenario for one weather station
create_scenario_list <- function(cmip6_one_station, 
                                 reference_year,
                                 times = c(2050, 2085), 
                                 baseline_year_relative_change = 2022,
                                 baseline_window_width = 15, 
                                 future_window_width = 31){
  
  
  #iterate over the different ssp scenarios
  listoflists <- purrr::map(cmip6_one_station, function(x){
    
    #x <- cmip6_one_station[[1]]
    
    #iterate over the different points of time we are interested in (2050, 2085)
    int <- purrr::map(times, function(time){
      
      #create baseline scenario (usually 2015)
      clim_senc <- chillR::temperature_scenario_from_records(weather = x, 
                                                             runn_mean = baseline_window_width,
                                                             year = baseline_year_relative_change)
      
      #create scenario for future point in time we are interested in (usually 2050 or 2085)
      clim_senc_later <- chillR::temperature_scenario_from_records(weather = x, 
                                                           runn_mean = future_window_width,
                                                           year = time)
      
      #calculate the realtive change and put that into a list, mimicking the structure in chillR
      clim_scen_adjusted <- list(data =  clim_senc_later[[1]]$data - clim_senc[[1]]$data,
                                 scenario = unique(x$ssp),
                                 start_year = time - baseline_window_width,
                                 end_year = time + baseline_window_width,
                                 scenario_year = time,
                                 reference_year = reference_year,
                                 scenario_type = 'relative',
                                 labels = unique(x$model))
      
    })
    
    #adjust names
    names(int) <- times
    
    return(int)
  })
  
  return(unlist(listoflists, recursive = FALSE))
}


# Scenarios<-c("historical","ssp1_2_6", "ssp2_4_5", "ssp3_7_0", "ssp5_8_5")
# Scenarios <- Scenarios[3]
# 
# run_download_cmip6(Scenarios = Scenarios,
#                    temporal_resolution = 'monthly')

#it should work now from download to scenario data
#haven't tried the function for other ssp scenarios
#function gets hung up on loop if there is a mistake
#make the loop more specific to the error message

