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
#' rpv_life(object = Insuree(x_ = 2.48, t_ = 3.57, benefit_t = c(1, 1, 1, 0.57), benefit_value = c(2, 2, 4, 2), m_ = 3), n = 5)
setMethod("rpv_life", signature("Insuree"), function(object, n) {
  
  # simulate deaths
  deaths <- rdeath(object, n = n)
  tod <- deaths[["death_t"]]
  
  # find applicable discount amount
  discount <- c()
  for (j in seq_along(tod)) {
    if(is.na(tod[j])) {
      discount[j] <- NA
    } else {
      discount[j] <- discount_death(object, death_time = tod[j]) 
    }
  }
  # find applicable benefit amount
  tod[tod < object@m_] <- NA
  benefit <- object@benefit_value[findInterval(tod, cumsum(c(object@m_, object@benefit_t)))]

  # set all deaths in term period equal to the applicable benefit value
  # function output
  pv = discount * benefit
  pv[is.na(pv)] <- 0
  list(deaths,
       discount = discount,
       benefit = benefit,
       pv = pv
  )
})

#' rpv_anniuty
#' 
#' present value of \code{rdeath} simulation for life contingent
#' annuities
#' 
#' @export
setGeneric("rpv_annuity", 
           #valueClass = "numeric",
           function(object, n) {
             standardGeneric("rpv_annuity")
           }
)

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
setMethod("rpv_annuity", signature("Insuree"), function(object, n) {
  
  # simulate deaths
  deaths <- rdeath(object, n = n)

  tod <- deaths[["death_t"]]
  # find all possible death and benefit intervals
  inters <- sort(unique(c(cumsum(deaths$t), cumsum(c(object@m_, object@benefit_t)))))
  # identify benefit amount in each interval
  benefit_annual <- object@benefit_value[findInterval(inters, cumsum(c(object@m_, object@benefit_t)))]
  benefit_pro_rata <- c(diff(inters), NA) * benefit
  #benefit_final <- benefit[findInterval(tod, inters)]
  #TODO: make new life table with prorate benefits and new t and x values for discount function
})