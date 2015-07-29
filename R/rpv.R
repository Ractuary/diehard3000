#' rpv_life
#' 
#' present value of \code{rdeath} simulation
#' 
#' @export
setGeneric("rpv_life", 
           #valueClass = "numeric",
           function(object, n) {
             standardGeneric("rpv_life")
           }
)



#' rpv_life
#' 
#' Simulates the present value of a single benefit life insurance payment
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' 
#' @export
#' @examples
#' rpv_life(object = Insuree(x_ = 2, t_ = 3, benefit_t = c(1, 1, 1), benefit_value = c(3, 2, 1), m_ = 0.3), n = 5)
#' rpv_life(object = Insuree(x_ = 2.48, t_ = 3.57, benefit_t = c(1, 1, 1, 0.57), benefit_value = c(2, 2, 2, 2), m_ = 3), n = 5)
setMethod("rpv_life", signature("Insuree"), function(object, n) {
  
  # simulate deaths
  deaths <- rdeath(object, n = n)
  tod <- deaths[["death_t"]]
  
  # returns vector of discount factors
  discount_table <- data.frame(
    t_cume = cumsum(deaths$t),
    discount = discount(object, x_ = object@x_, t_ = object@t_, m_ = object@m_)
  )
  # find applicable discount amount
  discount <- sapply(tod, function(x) ifelse(is.na(x), NA, discount_table$discount[discount_table$t_cume == x]))
  # find applicable benefit amount
  tod[tod < object@m_] <- NA
  benefit <- object@benefit_value[findInterval(tod, cumsum(c(object@m_, object@benefit_t)))]

  # set all deaths in term period equal to the applicable benefit value
  # function output
  list(deaths,
       discount = discount,
       benefit = benefit,
       pv = discount * benefit
  )
})

#' rpv_annuity
#'
#' simulate the present value of life contingent
#' annuity payments for one Insuree
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' 
#' @export
#' @examples
#' rpv_annuity(object = Insuree(m_ = .5, benefit = c(1,1, 1, 1)), n = 5)
#' 
#' 
#setMethod("rpv_life", signature("Insuree"), function(object, n) {
#  
#  # simulate deaths
#  deaths <- rdeath(object, n = n)
#  pv <- deaths[["death_table"]]
#  # convert 1s to 0s if annuity
#  # set insuree time of death to t_ + m_ if insuree did not die
#  tod <- deaths[["death_t"]]
#  tod[is.na(tod)] <- ceiling(object@x_ %% 1 + object@t_ + object@m_) + 1
#  pv[1:ceiling((object@x_ %% 1) + object@m_), ] <- 0
#  # change death_table to 1s for years insuree survives
#  for (j in seq_along(tod)) {
#    pv[1:tod[j], j] <- 1
#  }
#  
#  pv[ceiling((object@x_ %% 1) + object@m_ + 1):nrow(pv), ] <- 
#    pv[ceiling((object@x_ %% 1) + object@m_ + 1):nrow(pv), ] * 
#    object@benefit * deaths$t[ceiling((object@x_ %% 1) + object@m_ + 1):nrow(pv)]
#})