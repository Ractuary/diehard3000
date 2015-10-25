#' discount
#' 
#' discounts a life contingent benefit
#' 
#' @param object discount objects defining the benefit payment values and times
#' @param tod time of death
#' 
#' @export
setGeneric("discount", 
           valueClass = "numeric",
           function(object, ...) {
             standardGeneric("discount")
           }
)

#' discount
#' 
#' Finds the discount applicable to a future time of death
#' 
#' @param object object of class \code{Interest}
#' @param benefit_t time of benefit payment
#' 
#' @export
#' @examples
#' discount(Interest(), benefit_t = 2.1)
setMethod("discount", signature("Interest"), 
          function(object, benefit_t) {
            if (is.na(benefit_t)) return(NA_real_)
            if (is.null(object)) return(1)
            
            # finds all t values from current age
            tx <- cumsum(object@t)
            
            # return the t length of all interest intervals over which the individual
            # survived
            t <- object@t[tx < benefit_t]
            
            # return the length t length of the interest interval in which the
            # individual died
            beg_interval <- max(tx[tx < benefit_t])
            benefit_interval <- benefit_t - beg_interval
            time <- c(t, benefit_interval)
            
            # remove rate after death
            tx <- c(0, tx[-length(tx)])
            rate <- object@rate[tx < benefit_t]
            
            # find applicable trend factors
            trend <- (1 + rate) ^ (time)
            
            # calculate trend for given benefit time
            1 / prod(trend)
          })

#' discount
#' 
#' Discounts a \code{DeathBenefit} object
#' 
#' @param benefit object of class \code{DeathBenefit}
#' @param interest object of class \code{Interest}
#' @param tod time of death
#' 
#' @export
#' @examples
#' discount(benefit = BenefitDeath(), interest = Interest(), tod = 2)
#' discount(benefit = BenefitDeath(t = 10,
#'                                 value = 1000), 
#'          interest = Interest(t = rep(1, times = 10),
#'                              rate = rep(c(0.03, 0.04), times = 5)), 
#'          tod = 8) 
setMethod("discount", signature(benefit = "BenefitDeath", 
                                interest = "Interest"), 
          function(benefit, interest, tod) {
## TODO: figure out how to properly implement multiple dispatch  
  trend <- discount(interest, 
                    benefit_t = tod)
  
  # find applicable death benefit at time of death (tod)
  tx <- c(0, cumsum(benefit@t))
  benefit_value <- benefit@value[findInterval(tod, t)]
  
  benefit_value * trend
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