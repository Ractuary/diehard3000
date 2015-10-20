#' check_Interest
#' 
#' @param object object of class \code{Interest}
#' 
#' function to check validity of DeathBenefit S4 class constructor
check_BenefitDeath <- function(object) {
  errors <- character()
  if (!identical(length(object@t), 
                 length(object@rate),
                 1L)) { 
    errors <- c(errors, "Error! DeathBenefit t and value must all be of length 1")
  }
  
  if (any(object@t) < 0) {
    errors <- c(errors, "t values can not be less than 0")
  }
  
  if (identical(length(errors), 0)) {
    TRUE
  } else {
    errors
  }
}

#' Interest
#' 
#' This is to be used with the \code{Insuree} class to discount and
#' object from the \code{benefit} class to present value
#' 
#' @slot t time over which the annualized intereste rate is applicable
#' @slot rate annualized interest rate
#' 
#' @name Interest-class
#' @rdname Interest-class
#' @export Interest
BenefitDeath <- setClass("Interest",
                         slots = list(t = "numeric",
                                      value = "numeric"),
                         prototype = prototype(t = c(1, 1, 1),
                                               value = c(.04, .05, .03)),
                         validity = check_BenefitDeath
)