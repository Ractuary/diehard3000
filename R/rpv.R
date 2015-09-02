#' rpv_life
#' 
#' present value of \code{rdeath} simulation
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' @param interest vector of annual interest rates
#' 
#' @export
setGeneric("rpv_life", 
           #valueClass = "numeric",
           function(object, n, interest) {
             standardGeneric("rpv_life")
           }
)



#' rpv_life
#' 
#' Simulates the present value of the life insurance benefit for
#' an object of class \code{Insuree}
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' @param interest vector of annual interest rates.  Can use the \code{CIR()}
#' funtion to simulate interest rates in accordance with the Cox Ingersoll Ross
#' process.
#' 
#' @export
#' @examples
#' rpv_life(object = Insuree(x_ = 2, 
#'                           t_ = 3, 
#'                           benefit_t = c(1, 1, 1), 
#'                           benefit_value = c(3, 2, 1), 
#'                           m_ = 0.3), 
#'          n = 5,
#'          interest = 0.04)
#' rpv_life(object = Insuree(x_ = 2.48, 
#'                           t_ = 3.57, 
#'                           benefit_t = c(1, 1, 1, 0.57), 
#'                           benefit_value = c(2, 2, 4, 2), 
#'                           m_ = 3), 
#'          n = 5, 
#'          interest = rcir(n = 10, r = 0.01, b = 0.04, a = 1, s = 0.05))
setMethod("rpv_life", signature("Insuree"), function(object, n, interest) {
  
  # simulate deaths
  deaths <- rdeath(object, n = n)
  tod <- deaths[["death_t"]]
  
  # find applicable discount amount
  .discount <- lapply(tod, function(k) discount(interest, death_time = k))
  .discount <- unlist(.discount)
  
  # find applicable benefit amount
  tod[tod < object@m_] <- NA
  benefit <- object@benefit_value[findInterval(tod, cumsum(c(object@m_, object@benefit_t)))]

  # set all deaths in term period equal to the applicable benefit value
  # function output
  pv = .discount * benefit
  pv[is.na(pv)] <- 0
  list(deaths,
       discount = .discount,
       benefit = benefit,
       pv = pv
  )
})