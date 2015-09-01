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
#' @examples 
#' rcir(n = 10, r = 0.01, b = 0.04, a = 1, s = 0.05)
rcir <- function(n, r, b, a, s) {  
  output <- c()
  output[1] <- cir(r, b, a, s)
  for (i in 2:n) {
    output[i] <- cir(output[i - 1], b, a, s)
  }
  output
}

cir <- function(r, b, a, s) {
  r + a*(b - r) + s * sqrt(r) * rnorm(1, 0, 1)
}