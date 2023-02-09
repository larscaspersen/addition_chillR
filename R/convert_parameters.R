#' Converts parameters to standard PhenoFlex format
#' 
#' Takes parameter used in  \code{\link{evaluation_function_meigo_nonlinear}} and transform them 
#' to the typical PhenoFlex format.
#' 
#' The changed parameters are theta_star, theta_c, tau and pie_c, which get converted
#' to E0, E1, A0 and A1. In case of A0 and A1 two conversion functions are used. 
#' For E0 and E1 a nonlinear system is solved using the function \link[nleqslv]{nleqslv}.
#' 
#' @param par vector of length 12 with the parameters in the following order:
#' yc, zc, s1, Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope
#' @param failure_return character, by default set to "MEIGO". Decides what should be returned if the conversion failed.
#' In case of "MEIGO" it returns a MEIGO-compatible output, indicating that the 
#' current parameters are not suitable. If set equal "Ignore Error" it will return the
#' best estimate. If set equal "NA" it will return NA instead.
#' @return vector of length 12, with the PhenoFlex parameters yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope
#' with given temperature data and model parameters. 
#' @details The conversion follows the approach documented in Fishman et al (1987) and Egea et al. (2021).
#' For more details consult equations 5 - 8 in Egea et al (2021)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom nleqslv nleqslv
#' @examples 
#' \dontrun{

#' par_old <-   c(40, 190, 0.5, 25, 279, 286.1, 47.7, 28, 4, 36, 4, 1.60)
#' par_new <- convert_parameters(par)
#' }
#' @export convert_parameters
convert_parameters <- function(par, failure_return = 'MEIGO'){
  params<-numeric(4)
  
  params[1] <- par[5]   #theta*
  params[2] <- par[6]    #theta_c
  params[3] <- par[7]    #Tau(thetha*)
  params[4] <- par[8]     #pi_c
  
  
  output<-nleqslv::nleqslv(c(500, 15000), solve_nle, jac=NULL, params, xscalm="auto", method="Newton",
                  control=list(trace=0,allowSingular=TRUE))
  
  
  #This is a numerical method which can produce non-convergence. Check this
  if (output$termcd >= 3 & failure_return != "Ignore Error"){
    #if the nle algorithm has stalled just discard this solution

    if(failure_return == 'MEIGO'){
      return(list(F=10^6, g=rep(10^6,5)))
    } else if (failure_return == "NA"){
      E0<-NA; E1<-NA; A0<-NA; A1<-NA
    } else{
      stop('Wrong option selected for "failure_return"')
    }
    

    
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
  par[5:8] <- c(E0, E1, A0, A1)
  
  return(par)
}
