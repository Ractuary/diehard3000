#' discount
#' 
#' discounts a life contingent benefit
#' 
#' @param interest object of class \code{Interest}
#' @param benefit object of class \code{Benefit}
#' @param benefit_t time benefit is paid
#' 
#' @export
setGeneric("discount", 
           valueClass = "numeric",
           function(interest, 
                    benefit,
                    ...
                    ) {
             standardGeneric("discount")
           }
)

#' discount
#' 
#' Finds the discount applicable to a future time of death
#' 
#' @param interest object of class \code{Interest}
#' @param benefit_t time of benefit payment
#' 
#' @export
#' @examples
#' discount(Interest(), benefit_t = 2.1)
#' discount(Interest(), benefit_t = 1.5)
#' discount(Interest(), benefit_t = 0.5)
#' 
#' i <- Interest(t = rep(1, times = 5), rate = c(0.03, 0.06, 0.05, 0.07, 0.08))
setMethod("discount", signature("Interest"), 
          function(interest, benefit = NULL, benefit_t) {
            if (is.na(benefit_t)) return(NA_real_)
            if (is.null(interest)) return(1)
            
            # finds all t values from current age
            tx <- cumsum(interest@t)
            
            # return the t length of all interest intervals over which the individual
            # survived
            t <- interest@t[tx < benefit_t]
            
            # return the length t length of the interest interval in which the
            # individual died
            beg_interval <- if (all(tx >= benefit_t)) {
                              0
                            } else {
                              tx[tx < benefit_t]
                            }
            beg_interval <- max(beg_interval)
            benefit_interval <- benefit_t - beg_interval
            time <- c(t, benefit_interval)
            
            # remove rate after death
            tx <- c(0, tx[-length(tx)])
            rate <- interest@rate[tx < benefit_t]
            
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
setMethod("discount", signature(interest = "Interest",
                                benefit = "BenefitDeath" 
                                ), 
          function(interest, benefit, tod) {

  trend <- discount(interest, 
                    benefit_t = tod)
  
  # find applicable death benefit at time of death (tod)
  tx <- c(0, cumsum(benefit@t))
  benefit_value <- benefit@value[findInterval(tod, tx)]
  
  benefit_value * trend
})

#' discount
#'
#' discount a \code{BenefitAnnuity} object
#' 
#' @param interest object of class \code{Interest}
#' @param benefit object of class \code{Benefit}
#' @param tod the time of the benefit payment
#' 
#' @export
#' @examples
#' discount(Interest(), BenefitAnnuity(), tod = 2.1)
setMethod("discount", signature(interest = "Interest",
                                benefit = "BenefitAnnuity"), 
          function(interest, benefit, tod) {
            
            # find time to annuity payment for all survived annuity payments
            
            tx <- cumsum(benefit@t)
            benefit_index <- tx[tx < tod] 
            
            trend <- sapply(benefit_index,
                            function(x) discount(interest, benefit_t = x)
                            )
  
            # find applicable benefits and discount
            benefit_value <- benefit@value[tx < tod]
  
            benefit_value * trend
            }
        )