#' Stochastic Cox Ingersol Ross Process
#' 
#' 
#' @param n number of random observations
#' @param r starting short term interest rate
#' @param b mean of long term interest rate
#' @param a constant that determines rate of reversion of r
#' to the long term mean b
#' @param s volatility of the interest rate process
#' 
#' @export
#' @examples 
#' rcir(n = 10, r = 0.01, b = 0.04, a = 0.5, s = 0.07)
#' rcir(n = 10, r = 0.01, b = 0.04, a = 1, s = 0.05)
rcir <- function(n, r, b, a, s) {  
  output <- vector(mode = "numeric", length = n)
  output[1] <- cir(r, b, a, s)
  for (i in 2:n) {
    output[i] <- cir(output[i - 1], b, a, s)
  }
  output
}

cir <- function(r, b, a, s) {
  stopifnot(r >= 0) # CIR walk does not support negative interest rates
  max(r + a*(b - r) + s * sqrt(r) * rnorm(1, 0, 1), 0)
}

#' discount
#'
#' discount single benefit to present value
#' 
#' @param interest vector of annual interest rates
#' @param benefit_time the time (from x_) of death
#' 
#' @export
#' @examples
#' discount(0.04, death_time = 1.01)
#' discount(0.04, death_time = 0.8)
discount <- function(interest, benefit_time = NA) {
  if (is.na(benefit_time)) return(NA_real_)
  
  # repeat interest vector until it has length == ceiling(benefit_time)
  # if the interest vector is not long enough
  if (length(interest) <= benefit_time) {
    interest <- rep(interest, length.out = ceiling(benefit_time))
  }
  
  # find applicable trend factors
  trend <- 1 + interest[1:ceiling(benefit_time)]
  trend[length(trend)] <- trend[length(trend)] ^ (benefit_time %% 1)
  # calculate trend for given benefit time
  1 / prod(trend)
}