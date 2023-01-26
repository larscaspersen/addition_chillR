#' Function used to calculate E0, E1, A0 and A1 of Phenoflex
#' 
#' This function is used with the nlseq packages to to convert
#' the parameters theta_star, theta_c, tau and pie_c to E0, E1, A0 and A1.
#' Parameters E0 and E1 need to be numerically approximated. The function returns the 
#' error of the approximation for the given set of parameters. 
#' 
#' 
#' 
#' 
#' @param x numeric vector, containing values of E0 and E1
#' @param params numeric vector, containing values of parameters theta_star, theta_c, tau and pie_c
#' the 'custom_GDH_wrapper' function for that
#' @return error, for the current set of E0 and E1, which gets minimized during the approximation
#' 
#' @author Lars Caspersen
#' @keywords utility
#'  
#' @export solve_nle

solve_nle <- function(x,params){
  
  #Initialize the vector of residuals
  y<-numeric(2)
  
  
  #Introduce the values of the parameters actually optimized
  par1<-params[1]    #theta*
  par2<-params[2]    #theta_c
  par3<-params[3]    #Tau(thetha*)
  par4<-params[4]    #pi_c
  
  T1<-297
  T2<-279
  
  eta <- 1/3
  q=1/par1-1/par2
  
  
  #Equation 35 Fishman et al 87. In the paper it seems that the log is multiplying, but it is not.
  #y[1] <- (x[1]-x[2])/(exp((x[2]-x[1])*q)-1)/log(1-exp((x[1]-x[2])*q))-x[2]
  y[1] <- log((x[1]-x[2])/(exp((x[2]-x[1])*q)-1)/log(1-exp((x[1]-x[2])*q)))-log(x[2])
  
  
  #Calculate these terms needed for the next equation
  #From Eq 36, we get A1 depending only in E0 and E1. Here the log is indeed multiplying!!.
  A1 <- -exp(x[2]/par1)/par3*log(1-exp((x[1]-x[2])*q))
  k1T1<-A1*exp(-x[2]/T1)
  k1T2<-A1*exp(-x[2]/T2)
  
  # Equation 38. It only depends on E0 and E1. Also in A1 but we had its expresion in terms of E0 and E1
  lhs<-(exp((x[2]-x[1])/par2)-exp((x[2]-x[1])/T1))/(exp((x[2]-x[1])/T2)-exp((x[2]-x[1])/T1))
  rhs<-(1-exp(-k1T2*(1-eta)*par4))/(1-exp(-(k1T1*eta+k1T2*(1-eta))*par4))
  
  
  y[2] <- log(lhs)-log(rhs)   #Taking logs the problems is much easily solved
  
  return(y)
}