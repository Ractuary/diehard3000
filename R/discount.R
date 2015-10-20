#' discount
#' 
#' discounts a life contingent benefit
#' 
#' @param object discount objects defining the benefit payment values and times
#' @param tod time of death
#' 
#' @export
setGeneric("discount", 
           #valueClass = "numeric",
           function(object, tod) {
             standardGeneric("discount")
           }
)



#' discount
#' 
#' Discounts a \code{DeathBenefit} object
#' 
#' @param object object of class DeathBenefit
#' @param tod time of death
#' 
#' @export
#' @examples
#' discount(object = BenefitDeath(), tod = 2)
#' discount(object = BenefitDeath(), tod = 10) 
setMethod("discount", signature(benefit = "BenefitDeath", 
                                interest = "Interest"), 
          function(benefit, interest, tod) {
  if (is.na(tod)) return(NA_real_)
  
  # repeat interest vector until it has length == ceiling(benefit_time)
  # if the interest vector is not long enough
  if (length(interest) <= benefit_time) {
    interest <- rep(interest@rate, length.out = ceiling(benefit$t))
  }
  
  top_discount <- sum(discount@t)
  top_interest <- sum(interest@t)
            
  discount@t <- discount@t[discount@t < tod]          
  interest@t <- interest@t[interest@t < tod]
  
  # find applicable trend factors
  trend <- 1 + interest@rate[1:ceiling(t)]
  discount_time <- interest@t[1:ceiling(t)]
  discount_time[length(discount_time)] <- benefit@t %% 1
  
  # find fractional length of period in which death occured
  trend[length(trend)] <- trend[length(trend)] ^ (benefit@t %% 1)
  
  # calculate trend for given benefit time
  1 / prod(trend)
})

##' discount
##'
##' discount single benefit to present value
##' 
##' @param interest vector of annual interest rates
##' @param benefit_time the time (from x_) of death
##' 
##' @export
##' @examples
##' discount(0.04, benefit_time = 1.01)
##' discount(0.04, benefit_time = 0.8)
#discount <- function(interest, benefit_time = NA) {
#  if (is.na(benefit_time)) return(NA_real_)
#  
#  # repeat interest vector until it has length == ceiling(benefit_time)
#  # if the interest vector is not long enough
#  if (length(interest) <= benefit_time) {
#    interest <- rep(interest, length.out = ceiling(benefit_time))
#  }
#  
#  # find applicable trend factors
#  trend <- 1 + interest[1:ceiling(benefit_time)]
#  trend[length(trend)] <- trend[length(trend)] ^ (benefit_time %% 1)
#  # calculate trend for given benefit time
#  1 / prod(trend)
#}