% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solve_nle.R
\name{solve_nle}
\alias{solve_nle}
\title{Function used to calculate E0, E1, A0 and A1 of Phenoflex}
\usage{
solve_nle(x, params)
}
\arguments{
\item{x}{numeric vector, containing values of E0 and E1}

\item{params}{numeric vector, containing values of parameters theta_star, theta_c, tau and pie_c
the 'custom_GDH_wrapper' function for that}
}
\value{
error, for the current set of E0 and E1, which gets minimized during the approximation
}
\description{
This function is used with the nlseq packages to to convert
the parameters theta_star, theta_c, tau and pie_c to E0, E1, A0 and A1.
Parameters E0 and E1 need to be numerically approximated. The function returns the
error of the approximation for the given set of parameters.
}
\author{
Lars Caspersen
}
\keyword{utility}
