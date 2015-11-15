#' rpv_life
#' 
#' present value of \code{rdeath} simulation
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' @param interest vector of annual interest rates
#' 
#' @export
setGeneric("rpv", 
           #valueClass = "numeric",
           function(object, n, interest) {
             standardGeneric("rpv")
           }
)



#' rpv
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
#' rpv(object = Life(x_ = 2.48, 
#'                   t_ = 3,
#'                   m_ = 3,
#'                   benefit = list(BenefitDeath(t = c(3, 3), value = c(0, 5)))), 
#'     n = 5,
#'     interest = Interest(t = 10, rate = 0.04))
#'     
#' rpv(object = Life(x_ = 2.48, 
#'                   t_ = 3,
#'                   m_ = 0,
#'                   benefit = list(BenefitAnnuity(t = c(1, 1, 1), value = c(3, 2, 2)))), 
#'     n = 5,
#'     interest = Interest(t = 10, rate = 0.04))
#'     
#' rpv(object = Life(x_ = 2.48, 
#'                   t_ = 3,
#'                   m_ = 0,
#'                   benefit = list(BenefitAnnuity(t = c(0, 1, 1), value = c(-2, -2, -2)),
#'                                  BenefitDeath(t = 3, value = 10)
#'                                  )
#'                   ), 
#'     n = 5,
#'     interest = Interest(t = 10, rate = 0.04))   
setMethod("rpv", signature("Life"), function(object, n, interest) {
  
  # simulate deaths
  deaths <- rdeath(object, n = n)
  tod <- deaths[["death_t"]]
  
  # find applicable benefits discounted to present value
  pv <- vector("list", length = length(tod))
  for (i in seq_along(pv)) {
    pv[[i]] <- vector("numeric", length = length(object@benefit))
    for (j in seq_along(pv[[i]])) { 
      pv[[i]][j] <- discount(interest = interest, 
                                        benefit = object@benefit[[j]], 
                                        tod = tod[i])
                        
    }
    pv[[i]] <- sum(pv[[i]])
  }
  
  pv <- unlist(pv)

  # set all deaths in term period equal to the applicable benefit value
  # function output
  #pv[is.na(pv)] <- 0 # move this to BenefitDeath discount method
  out <- list(deaths,
              pv = pv
         )
  class(out) <- "rpv_Life"
  out
})


#' rpv
#' 
#' Simulates the present value of the life insurance benefit for
#' each \code{Life} in the \code{Pool} object.
#' 
#' @param object object of class \code{Pool}
#' @param n number of observations
#' @param interest vector of annual interest rates.  e.g. you can use the \code{CIR()}
#' funtion to simulate interest rates in accordance with the Cox Ingersoll Ross
#' process.
#' 
#' @export
#' @examples
#' rpv(object = Pool(),
#'     n = 5,
#'     interest = Interest(t = 10, rate = 0.04))
#' rpv(object = Pool(),
#'     n = 5, 
#'     interest = Interest(t = 1:10, rate = rcir(n = 10, r = 0.01, b = 0.04, a = 1, s = 0.05))
#'     )
setMethod("rpv", signature("Pool"), function(object, n, interest) {
  
  # run rpv() simulation for each Insuree object
  out <- lapply(object@lives, rpv, n = n, interest = interest)
  class(out) <- "rpv_Pool"
  out
})