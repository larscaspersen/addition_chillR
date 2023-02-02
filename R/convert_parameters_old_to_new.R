#' Converts parameters from old to new format
#' 
#' Takes the parameters used in  \link[chillR]{phenologyFitter} and brings it to
#' the format used in \code{\link{evaluation_function_meigo_nonlinear}}.
#' 
#' Original parameters include E0, E1, A0, A1 and convert those to theta_star, theta_c, tau and pie_c.
#' Parameters theta_star and tau depend on approximated intermediate variable sigma. Pie_c is
#' approximated using theta_star, theta_c and tau. Since the conversion
#' is an numerical approximation, the final results differ slightly from the "true" value.
#' This can be seen when first using \code{\link{convert_parameters}} and then convert the 
#' parameter again to the original format using this function. The error should  be
#' neglectible. Approximation is done using the function \link[nleqslv]{nleqslv}.
#' 
#' @param par vector of length 12 with the parameters in the following order:
#' yc, zc, s1, Tu, E0, E1, A0, A1, Tf, Tc, Tb, slope
#' @return vector of length 12, with the PhenoFlex parameters yc, zc, s1, Tu, theta_star, theta_c, tau, pie_c, Tf, Tc, Tb, slope
#' with given temperature data and model parameters. 
#' @details The conversion follows the approach documented in Fishman et al (1987) and Egea et al. (2021).
#' For more details consult equations 5 - 8 in Egea et al (2021)
#' @author Lars Caspersen, \email{lars.caspersen@@uni-bonn.de}
#' @importFrom nleqslv nleqslv
#' @examples 
#' \dontrun{

#' par_old <-   c(40, 190, 0.5, 25, 279, 286.1, 47.7, 28, 4, 36, 4, 1.60)
#' par_new <- convert_parameters(par)
#' par_old_again <- convert_parameters_old_to_new(par)
#' }
#' @export convert_parameters_old_to_new

convert_parameters_old_to_new <- function(par){
  
  output<-nleqslv::nleqslv(c(10), solve_sigma, jac=NULL, par, xscalm="auto", method="Newton",
                           control=list(trace=0,allowSingular=TRUE))
  
  E0 <- par[5]
  E1 <- par[6]
  A0 <- par[7]
  A1 <- par[8]
  
  p <- (E1 -E0) / E1
  
  if((1/output$x) < p){
    sigma <- output$x
  } else {
    stop('Conversion failed')
  }
  theta_star <- (E1-E0)/log((A1*sigma)/(A0*(sigma - 1)))
  
  #fishman 1987 eq33
  theta_c <- (E1-E0)/log(A1/A0)
  
  
  
  #mentioned in Fishman 1987 in between eq4 to eq6
  #but I need to calculate it with theta_star!
  k0 <- A0 * exp(-E0/theta_star)
  k1 <- A1 * exp(-E1/theta_star)
  xs <- k0 / k1
  
  tau <- (1/k1) * log(xs / (xs -1))
  
  
  params_old <- c(par[5:8],sigma)
  
  
  output<-nleqslv::nleqslv(c(28), solve_theta_star, jac=NULL, params_old, xscalm="auto", method="Newton",
                           control=list(trace=0,allowSingular=TRUE))
  
  
  #This is a numerical method which can produce non-convergence. Check this
  if (output$termcd >= 3){
    #if the nle algorithm has stalled just discard this solution
    pie_c<-NA;
    warning('Approximation of pie_c failed')
    
    
  } else {
    
    pie_c <- output$x[1]
    
  }
  
  
  #change the name of the parameters, dont know if necessary
  par[5:8] <- c(theta_star, theta_c, tau, pie_c)
  
  return(par)
}

solve_sigma <- function(x, par_old){
  E0 <- par_old[5]
  E1 <- par_old[6]
  
  p <- ((E1-E0) / E1)
  
  lhs <- log(x)
  rhs <- p * (x -1)
  
  f <- abs(lhs - rhs)
  
  return(f)
  
}

solve_theta_star <- function(x,params_old){
  
  pie_c <- x[1]
  
  #Initialize the vector of residuals
  y<-numeric(1)
  
  
  #Introduce the values of the parameters actually optimized
  E0<-params_old[1]    #theta*
  E1<-params_old[2]    #theta_c
  A0<-params_old[3]    #Tau(thetha*)
  A1<-params_old[4]    #pi_c
  sigma <- params_old[5]
  
  T1<-297
  T2<-279
  eta <- 1/3
  
  
  #fishman 1987 eq33
  theta_c <- (E1-E0)/log(A1/A0)
  
  #this section is needed because I couldn't find the reference value of sigma
  #instead I tried to calculate what sigma must have been in the fishman publication
  #based on the reported inputs and outputs
  # E0_org <- 2874.364
  # E1_org <- 11441.415
  # A0_org <- 1.386e3
  # A1_org <- 1.275e16
  # theta_star_org <- 279
  # 
  # b <- exp((E1_org - E0_org)/theta_star_org)*(A0_org/A1_org)
  # sigma <- b/(b-1)
  
  #use eq19
  theta_star <-(E1-E0)/log((A1*sigma)/(A0*(sigma - 1)))
  
  #intermediate object
  q <- (1/theta_star) - (1/theta_c)
  
  #eq 34
  tau <- 1/(-A1) * exp(E1/theta_star) * log(1 - exp((E0-E1)*q))
  
  #intermediate objects
  k1T1<-A1*exp(-E1/T1)
  k1T2<-A1*exp(-E1/T2)
  
  #eq 38 left hand side can be calculate based on what we know
  lhs<-(exp((E1-E0)/theta_c)-exp((E1-E0)/T1))/(exp((E1-E0)/T2)-exp((E1-E0)/T1))
  
  #for right hand site we need an approximated value of pie_c
  rhs<-(1-exp(-k1T2*(1-eta)*pie_c))/(1-exp(-(k1T1*eta+k1T2*(1-eta))*pie_c))
  
  y <- log(lhs)-log(rhs)   #Taking logs the problems is much easily solved
  
  return(y)
}


