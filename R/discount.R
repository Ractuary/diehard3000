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
#' discount(i, benefit_t = 1.1)
#' discount(Interest(t = 100, rate = 0.04), benefit_t = 1.1)
setMethod("discount", signature("Interest"), 
          function(interest, benefit = NA, benefit_t) {
            if (is.na(benefit_t)) return(NA_real_) # individua did not receive benefit
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
#' discount(benefit = BenefitDeath(), interest = Interest(), tod = NA)
#' discount(benefit = BenefitDeath(t = 10,
#'                                 value = 1000), 
#'          interest = Interest(t = rep(1, times = 10),
#'                              rate = rep(c(0.03, 0.04), times = 5)), 
#'          tod = 8)
setMethod("discount", signature(interest = "Interest",
                                benefit = "BenefitDeath" 
                                ), 
          function(interest, benefit, tod) {
  if (is.na(tod)) return(0) # Life survives
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
#' discount(Interest(), BenefitAnnuity(), tod = NA)
#' discount(Interest(), BenefitAnnuity(), tod = 10)
#' discount(Interest(), BenefitAnnuity(t = c(1, 1, 1), value = c(3, 2, 2)), tod = 3)
#' discount(Interest(), BenefitAnnuity(t = c(1, 1, 1), value = c(3, 2, 2)), tod = 0.5)
setMethod("discount", signature(interest = "Interest",
                                benefit = "BenefitAnnuity"), 
          function(interest, benefit, tod) {
            if (tod < benefit@t[1] && !is.na(tod)) return(0) 
            
            # find time to annuity payment for all survived annuity payments
            # find applicable benefits and discount
            tx <- cumsum(benefit@t)
            if (is.na(tod)) {
              benefit_index <- tx
              benefit_value <- benefit@value
            } else {
              benefit_index <- tx[tx <= tod] 
              benefit_value <- benefit@value[tx <= tod]
            }
            
            trend <- sapply(benefit_index,
                            function(x) discount(interest, benefit_t = x)
                            )
  
            sum(benefit_value * trend)
            }
        )