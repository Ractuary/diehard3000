#' rpv_life
#' 
#' present value of \code{rdeath} simulation
#' 
#' @param object object of class Insuree
#' @param n number of observations
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
#' Simulates the present value of the life insurance benefit for
#' an object of class \code{Insuree}
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' 
#' @export
#' @examples
#' rpv_life(object = Insuree(x_ = 2, 
#'                           t_ = 3, 
#'                           benefit_t = c(1, 1, 1), 
#'                           benefit_value = c(3, 2, 1), 
#'                           m_ = 0.3), 
#'          n = 5)
#' rpv_life(object = Insuree(x_ = 2.48, 
#'                           t_ = 3.57, 
#'                           benefit_t = c(1, 1, 1, 0.57), 
#'                           benefit_value = c(2, 2, 4, 2), 
#'                           m_ = 3), 
#'          n = 5)
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
#setGeneric("rpv_annuity", 
#           #valueClass = "numeric",
#           function(object, n) {
#             standardGeneric("rpv_annuity")
#           }
#)

#' rpv_annuity
#'
#' simulates the present value of life contingent
#' annuity payments for one \code{Insuree}
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' 
#' @export
#' @examples
#' rpv_annuity(
#' object = Insuree(x_ = 2.48, 
#'                              t_ = 3.57, 
#'                              benefit_t = c(1, 1, 1, 0.57), 
#'                              benefit_value = c(2, 2, 4, 2), 
#'                              m_ = 3)#, 
#'             n = 5)
#setMethod("rpv_annuity", signature("Insuree"), function(object, n) {
#  
#  # simulate deaths
#  deaths <- rdeath(object, n = n)
#  tod <- deaths[["death_t"]]
#  
#  # cumulative t values for the benefit_t
#  benefit_time <- cumsum(c(object@m_, object@benefit_t))
#  # x value values for the benefit_t
#  benefit_x <- object@x_ + benefit_time
#  
#  # identify all possible death times that occur during
#  # at times when annuity benefits are being paid
#  deaths_t <- cumsum(deaths$t)
#  deaths_t <- deaths_t[deaths_t > object@m_]
#  # find all possible death and benefit t intervals
#  # we need all possible intervals so we can discount each interval for
#  # the appropriate time and benefit amount.
#  #intervals_t <- sort(
#  #                 unique(
#  #                   round(
#  #                     c(
#  #                       deaths_t,
#  #                       benefit_time
#  #                     ),
#  #                    2
#  #                   )
#  #                 )
#  #               )
#  
#  # identify annual benefit amount correspinding to each t interval 
#  # in the interval_t vector
#  #benefit_annual <- object@benefit_value[findInterval(intervals_t[-1], benefit_time)]
#  #benefit_annual <- benefit_annual[-length(benefit_annual)]
#  
#  #TODO: need to rethink how benefit is handled for annuities
#  
#  #benefit_pro_rata <- c(diff(intervals_t[intervals_t > object@m_])) * benefit_annual
#  # apply benefit amounts only to time survived for each simulated life
#  #benefit_pro_rata_n <- lapply(tod, function(j) if(is.na(j)) {NA} else {benefit_pro_rata[intervals_t < j]})
#  
#  # create new LifeTable segmented by inters
#  #inters_x <- object@x_ + intervals_t
#  #inters_x <- inters_x[-length(inters_x)]
#  #i_new <- object@i[findInterval(inters_x, object@x)]
#  # Probably need to redefine LifeTable object where t is set by default
#  #annuity_at <- ActuarialTable(x = inters_x,
#  #                             t = t_new,
#  #                             q_x = q_x_new,
#  #                             i = i_new)
#  #
#  #discount <- discount(annuity_at, 
#  #                     x_ = object@x_,
#  #                     t_ = object@t_,
#  #                     m_ = object@m_)
#  #
#  # set all deaths in term period equal to the applicable benefit value
#  # function output
#  # pv = discount * benefit
#  # pv[is.na(pv)] <- 0
#  # list(deaths,
#  #      discount = discount,
#  #      benefit = benefit,
#  #      pv = pv
#  # )
#})