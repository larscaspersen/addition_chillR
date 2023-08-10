#' Function to return bloom dates
#' 
#' Takes hourly weather time series and PhenoFlex model parameters and returns day of the year with bloom. 
#' 
#' This function is mainly used by the global optimization objective functions
#' (\code{\link{evaluation_function_meigo}, \link{evaluation_function_meigo_nonlinear}, \link{evaluation_function_meigo_vns}}) 
#' to fit the PhenoFlex parameters. For a single season hourly temperature data time series and a set of model
#' parameters it returns the predicted day of the year of bloom.
#' 
#' @param x data.frame with at least the column "Temp" (hourly temperature data) and "JDay" (day of the year)
#' @param par vector of length 12 with the PhenoFlex model parameters in the following order:
#' yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope. If the set of parameters include 
#' theta_star, theta_c, tau and pie_c instead of E0, E1, A0, and A1, then the function \code{\link{convert_parameters}}
#' should be run before inserting the parameters
#' @param constraints boolean, set FALSE by default. If set TRUE, the function will perform
#' some checks on the model parameter prior to the prediction of bloom day. For more information 
#' see "Details"
#' @return single numeric value with the day of the year, for which the model predicts bloom
#' with given temperature data and model parameters. 
#' @details In case constraints is TRUE, logic checks will be performed for some parameters. If one of the checks 
#' is violated, then the function will return "NA" instead of the predicted bloom day. Checks involve parameters
#' of the dynamic chill model and the growing degree hour (GDH) model. Tests include:
#' \itemize{
#'  \item{Parameters of GDH model:}
#'     \itemize{
#'     \item{Tf > Tb}
#'     \item{Tc > Tb}
#'     }
#'  \item{Dynamic model}
#'  \itemize{
#'     \item{exp((10 * E0)/(297 * 279)) > 1.5 & exp((10 * E0)/(297 * 279)) < 3.5}
#'     \item{exp((10 * E1)/(297 * 279)) > 1.5 | exp((10 * E0)/(297 * 279)) < 3.5}
#'     }
#' }
#' 
#' The parameter test of the dynamic model follows the approach suggested by Egea et al (2021)
#' in which the Q10 parameter was used to constraint the E0 and E1 parameters. The Q10
#' parameter how much a chemical process increases by increasing the temperature by 10 degree 
#' Celsius. Biological reactions typically a Q10 value ranging from 2 to 3. Here, ranges of 1.5 to 3.5 were used as a threshold.
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @import chillR
#' @examples 
#' \dontrun{
#' #          yc,  zc,  s1, Tu,    E0,      E1,     A0,         A1,   Tf, Tc, Tb,  slope
#' par <-   c(40, 190, 0.5, 25, 3372.8,  9900.3, 6319.5, 5.939917e13,  4, 36,  4,  1.60)
#' 
#' #prepare weather data
#' weather<-fix_weather(KA_weather[which(KA_weather$Year>2004),])
#' hourtemps<-stack_hourly_temps(weather, latitude=50.4)
#' seasonList <- genSeasonList(hourtemps$hourtemps, year = 2006)
#' 
#' custom_PhenoFlex_GDHwrapper(x = seasonList[[1]], par = par)
#' 
#' 
#' }
#' @export custom_PhenoFlex_GDHwrapper
custom_PhenoFlex_GDHwrapper <- function (x, par, constraints = FALSE){
  #x is one of the elements in season List
  #par are the parameters of the model
  
  #make explicit what which parameters is
  yc = par[1]
  zc = par[2]
  s1 = par[3]
  Tu = par[4]
  E0 = par[5]
  E1 = par[6]
  A0 = par[7]
  A1 = par[8]
  Tf = par[9]
  Tc = par[10] 
  Tb = par[11]
  slope = par[12]
  
  
  #in case the parameters do not make sense, return NA
  if(constraints){
    
    t1 <- Tu <= Tb
    t2 <- Tc <= Tb
    t3 <- exp((10 * E0)/(297 * 279)) < 1.5 | exp((10 * E0)/(297 * 279)) > 3.5
    t4 <- exp((10 * E1)/(297 * 279)) < 1.5 | exp((10 * E0)/(297 * 279)) > 3.5
    
    if(any(c(t1, t2, t3, t4))){
      return(NA)
    }
    
  }
  
  #calculat the bloom day
  bloomindex <- chillR::PhenoFlex(temp = x$Temp, 
                          times = seq_along(x$Temp), 
                          yc = yc, 
                          zc = zc, 
                          s1 = s1, 
                          Tu = Tu, 
                          E0 = E0, 
                          E1 = E1, 
                          A0 = A0, 
                          A1 = A1, 
                          Tf = Tf, 
                          Tc = Tc, 
                          Tb = Tb, 
                          slope = slope, 
                          Imodel = 0L, 
                          basic_output = TRUE)$bloomindex
  
  

  #return values
  if (bloomindex == 0){
    return(NA)
  } 
  
  JDay <- x$JDay[bloomindex]
  JDaylist <- which(x$JDay == JDay)
  
  #if we are in the norhtern hemisphere and the year corresponding to the index is the smaller one, return negative numbers relative to Jan-1 being 1
  if(length(unique(x$Year)) == 2 & x$Year[bloomindex] == min(x$Year)){

    JDay <- JDay - 365
    
  } 
  
  n <- length(JDaylist)
  if (n == 1){
    return(JDay)
  } 
  return(JDay + which(JDaylist == bloomindex)/n - 1/(n/ceiling(n/2)))
  
  
}