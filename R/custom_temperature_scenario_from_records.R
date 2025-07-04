#' Make monthly temperature scenario from historic records
#' 
#' Produces a list of scenarios containing monthly means for Tmin and Tmax that are representative
#' of particular years. Can also handle precipitation now. These scenario are computed by applying linear regression to a
#' file containing Tmin and Tmax records, and using the regression model to calculate typical
#' values for the user-specified years.
#' 
#' This function produces outputs that can be used as input for the temperature_generation
#' function. Sample applications are the use of the temperature_generation function for
#' making replicate weather records for a given year for risk assessment purposes, or
#' the generation of a weather scenario that can be compared with other datasets (e.g. climate
#' scenarios based on the WorldClim dataset refer to a 1951-2000 baseline, so that meaningful
#' use of such scenarios for local contexts requires consideration of a scenario that corresponds
#' to temperatures in 1975, the central year of this period).
#' 
#' 
#' @param weather daily weather, as produced with the fix_weather
#' function. Can also be generated by other means, but should contain the columns
#' c("Month", "Day", "Year"). Also needs to contain variables specified in argument
#' 'variable'
#' @param variable, vector with characters. Specifies for which columns the
#' temperature scenario is generated. By default set to c('Tmin', 'Tmax') for 
#' minimum and maximum temperature. Also allows 'Prec' for precipitation.
#' @param year numeric vector of years, for which the scenario is to be produced.
#' @param weather_start start year of the period to be considered in calculating the regression.
#' Defaults to NA, which means the first year of the record is used as start year.
#' @param weather_end end year of the period to be considered in calculating the regression.
#' Defaults to NA, which means the last year of the record is used as end year.
#' @param scen_type character string, either "regression" or "running_mean", specifying how the
#' scenario should be produced. "regression" computed the scenario based on an assumed linear trend
#' in the data; "running_mean" uses a running mean function instead, with the length of the
#' running mean window determined by the runn_mean parameter. The default is a running mean function,
#' since the assumption of a linear trend often does not hold.
#' @param runn_mean number of vector elements to use for calculating the
#' running mean; this is reduced, if the time series is not long enough to accommodate the specified
#' window. Defaults to 15.
#' @return list of climate scenario objects, consisting of the following elements: 'data' =
#' a data frame with n_intervals elements containing the absolute temperature information.
#' 'scenario_year' = the year the scenario is representative of, i.e. the specified 'year' parameter.
#' 'reference_year' = NA (because this is an absolute temperature scenarios, not a 
#' relative one); 'scenario_type' = 'absolute' (because this is an absolute temperature
#' scenario, not a relative one); 'labels' = 'regression-based scenario'.
#' 
#' @author Eike Luedeling, Lars Caspersen
#' @keywords utility
#' @importFrom stats lm
#' @examples
#' \dontrun{
#' custom_temperature_scenario_from_records(weather=chillR::KA_weather,
#' year=2001,
#' weather_start=2000,
#' weather_end=2005)
#' }
#'  
#' @export custom_temperature_scenario_from_records

custom_temperature_scenario_from_records <-function (weather, year, variable = c('Tmin', 'Tmax'), weather_start = NA, weather_end = NA, 
                                                     scen_type = "running_mean", runn_mean = 15) 
{
  if (!is.data.frame(weather)) 
    return(warning("Error - weather object is not a data.frame"))
  if (!"Day" %in% colnames(weather) | !"Month" %in% colnames(weather) | 
      !"Year" %in% colnames(weather)) 
    return(warning("Error - required columns missing ('Day','Month','Year','Tmin' or 'Tmax')"))
  if(any(variable %in% colnames(weather) == FALSE)) stop('Specified variables missing in weather data')
  if (is.na(weather_start)) 
    weather_start <- min(weather$Year)
  if (is.na(weather_end)) 
    weather_end <- max(weather$Year)
  if (weather_start == weather_end) 
    return(warning("Error - only one data year selected"))
  if (weather_start > weather_end) 
    return(warning("Error - End year before start year"))
  weather <- weather[which(weather$Year >= weather_start), 
  ]
  weather <- weather[which(weather$Year <= weather_end), ]
  if (nrow(weather) == 0) 
    return(warning("Error - no weather records in specified interval"))
  past_means <- list()
  baseclim_baseline_duration_adjustment <- data.frame(test = rep(NA, 12))
  for(var in variable){
    past_means[[var]] <- aggregate(weather[, var], by = list(weather$Year, 
                                                             weather$Month), FUN = "mean")
    colnames(past_means[[var]]) <- c("Year", "Month", "Temp")
    
    df_add <- data.frame(var = rep(NA, 12))
    colnames(df_add) <- var
    baseclim_baseline_duration_adjustment <- cbind(baseclim_baseline_duration_adjustment,
                                                   df_add)
  }
  baseclim_baseline_duration_adjustment <- baseclim_baseline_duration_adjustment[,-1]
  
  rownames(baseclim_baseline_duration_adjustment) <- 1:12
  out_scenarios <- list()
  for (y in 1:length(year)) {
    for (v in variable) {
      for (i in 1:12) {
        monthly <- past_means[[v]][which(past_means[[v]]$Month == 
                                           i), ]
        if (scen_type == "regression") {
          model <- lm(monthly$Temp ~ monthly$Year)
          baseclim_baseline_duration_adjustment[i, v] <- as.numeric(model$coefficients[1] + 
                                                                      model$coefficients[2] * year[y])
          if (is.na(baseclim_baseline_duration_adjustment[i, 
                                                          v])) 
            return(warning(paste("Error - unable to calculate regression for", 
                                 v, "in month", i)))
        }
        if (scen_type == "running_mean") {
          baseclim_baseline_duration_adjustment[i, v] <- runn_mean_pred(indep = monthly$Year, 
                                                                        dep = monthly$Temp, pred = year[y], runn_mean = runn_mean)$predicted
          if (is.na(baseclim_baseline_duration_adjustment[i, 
                                                          v])) 
            return(warning(paste("Error - cannot calculate value for", 
                                 v, "in month", i)))
        }
      }
    }
    out_scenarios[[y]] <- list(data = baseclim_baseline_duration_adjustment, 
                               scenario_year = year[y], reference_year = NA, scenario_type = "absolute", 
                               labels = "regression-based scenario")
    if (scen_type == "running_mean") 
      out_scenarios[[y]]$labels <- "running mean scenario"
  }
  names(out_scenarios) <- as.character(year)
  return(out_scenarios)
}