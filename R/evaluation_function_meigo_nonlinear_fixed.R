#' Function used in the optimization process to generate bloom data
#' 
#' This is the evaluation function for the fitting process. It takes the "new" parameters
#' (theta_star, theta_c, tau, pie_c) instead of the old ones (E0, E1, A0, A1). This function
#' allows combined fitting of cultivars. It will only estimate cultivar-specific parameters of yc, zc and s1. Furthermore
#' it has fixed values of theta_star, pie_c, Tc, Tu and slope. 
#' 
#' This function will be best used in combination with the \link[MEIGOR]{MEIGO} function 
#' to estimate PhenoFlex parameter values. The order of bloomJDays and SeasonList should be the same. 
#' The order will correspond to estimated cultivar-specific parameters 
#' 
#' 
#' @param x traditional model parameters of PhenoFlex in the order yc, zc, s1, Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope. The parameters
#' yc, zc and s1 need to be replicated as often as cultivars are fitted. 
#' @param modelfn function used within the evaluation function to calculate the actual bloomday, often we use
#' the 'custom_GDH_wrapper' function for that
#' @param bloomJDays list of numeric vectors containing the observed bloom dates in day of the year format
#' @param SeasonList list of lists, containing the hourly temperatures for the individual phenological seasons. One element per cultivar.
#' The list of each cultivar should contain data.frames for eachs season
#' with the columns "Temp" (for the hourly temperature) and "JDay" for the corresponding Julian day. Is usually
#' generated using \link[chillR]{genSeasonList}
#' @param n_cult Numeric, specifying the numbers of cultivars fitted
#' @param na_penalty numeric, value which is used when the model fails to generate a prediction
#' for the bloom date. By default 365
#' @param theta_star numeric, by default 286. Is the optimal temperature for chill accumulation measured in Kelvin
#' @param pie_c numeric, ny default 28. Is the interval length in hours of a cycled two-temperature experiment which lead 
#' to absolute chill negation. 
#' @param Tc numeric, by default 36. Is the upper limit of temperature in degree Celsius, above which no heat accumulation happens.
#' @param Tb numeric, by default 2. Is the lower limit of temperature in degree Celsius, below which no heat accumulation happens.
#' @param slope numeric, by default 1.6. Defines the slope of the transition process converting labile to stabile chill portions. 
#' @return list with two elements. First is called 'f' and contains the residual sum of squares of the model. The 
#' second is 'g' which is the values of the additional model constraints defined in the function.
#' 
#' @author Lars Caspersen
#' @keywords utility
#' @importFrom nleqslv nleqslv
#'  
#' @export evaluation_function_meigo_nonlinear_fixed

evaluation_function_meigo_nonlinear_fixed <- function(x, 
                                                      modelfn,
                                                      bloomJDays,
                                                      SeasonList,
                                                      na_penalty = 365,
                                                      n_cult,
                                                      theta_star = 286,
                                                      pie_c = 28,
                                                      Tc = 36,
                                                      Tb  = 2,
                                                      slope = 1.6){
  
  
  #bring x to another variable
  x_supplied <- x
  
  x <- c(x_supplied[1:((n_cult*3)+1)], #repeated yv, zc, s1 and lastly single Tu
         theta_star, #fixed
         x_supplied[c(length(x_supplied)-2, length(x_supplied)-1)], #theta_c and tau
         pie_c, #fixed
         x_supplied[length(x_supplied)], #Tf
         Tc,
         Tb,
         slope) 
  
  
  params<-numeric(4)
  
  params[1] <- theta_star  #theta*
  params[2] <- x[6+((n_cult -1) * 3)]    #theta_c
  params[3] <- x[7+((n_cult -1) * 3)]    #Tau(thetha*)
  params[4] <- pie_c     #pi_c
  
  
  output<-nleqslv::nleqslv(c(500, 15000), solve_nle, jac=NULL, params, xscalm="auto", method="Newton",
                           control=list(trace=0,allowSingular=TRUE))
  
  
  #This is a numerical method which can produce non-convergence. Check this
  if (output$termcd >= 3){
    #if the nle algorithm has stalled just discard this solution
    E0<-NA; E1<-NA; A0<-NA; A1<-NA
    return(list(F=10^6, g=rep(10^6,5)))
    
    #You would add here a flag to let your optimization procedure know
    #That this solution should be ignored by lack of convergence
    
  } else {
    
    E0 <- output$x[1]
    E1 <- output$x[2]
    
    #A1 and A0 can be calculated through Equations 36 and 37
    
    q=1/params[1]-1/params[2]
    
    A1 <- -exp(E1/params[1])/params[3]*log(1-exp((E0-E1)*q))
    A0 <- A1*exp((E0-E1)/params[2])
  }
  
  
  #change the name of the parameters, dont know if necessary
  par <- x
  par[(5:8)+((n_cult -1) * 3)] <- c(E0, E1, A0, A1)
  
  
  
  #loop over the cultivars, calculate predicted days for each cultivar
  rss <- purrr::map_dbl(1:length(SeasonList), function(i){
    
    #extract the parameters
    par_cult <- par[c(i, 1+((n_cult -1))+i, 2+((n_cult -1)*2)+i, (length(par)-8):length(par))]
    #predict the bloom
    pred_bloom <- unlist(lapply(X = SeasonList[[i]], FUN = modelfn, par = par_cult))
    pred_bloom <- ifelse(is.na(pred_bloom), yes = na_penalty, no = pred_bloom)
    #calculate for each cultivar the rss
    rss <- sum((pred_bloom - bloomJDays[[i]]$pheno)^2)
    return(rss)
    
  })
  
  
  #calculate the model performance value
  F <- sum(rss)
  
  
  
  #####
  #inequality constraints
  #####
  
  #this is the vector containing the values for the inequality constraints
  #at first initialize the vector
  g <- rep(0,5)
  
  
  #equality constraints should be always stated before inequality constraints, according to meigo vignette
  #(but we dont have any in this case)
  
  
  #inequality constraints are reformulated as differences
  
  #Tu >= Tb
  g[1] <- par[length(par)-8] - par[length(par)-1]
  #Tx >= Tb
  g[2] <- par[length(par)-2] - par[length(par)-1]
  #Tc >= Tu
  g[3] <- par[length(par)-2] - par[length(par)-8]
  
  
  #the q10 value should be between 1.5 and 3.5
  #q10 formation = exp((10*E0)/(T2-T1))
  #q10 destruction = exp((10*E1)/(T2-T1))
  #T1 = 297
  #T2 = 279
  #parameters T1 and T2 chosen after Egea 2020
  #ranges for q10 are provided in c_L and c_U
  g[4] <- exp((10 * par[length(par)-7]) / (297 * 279))
  
  g[5] <- exp((10 * par[length(par)-6]) / (297 * 279))
  
  
  
  return(list(F=F, g=g))
  
}