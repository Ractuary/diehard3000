#' Benefit
#' 
#' virtual class inherited by all types of benefits. (e.g. BenefitDeath and
#' BenefitAnnuity inherit the Benefit class)
#' 
#' 
#' @name Benefit-class
#' @rdname Benefit-class
#' @export Benefit
Benefit <- setClass("Benefit",
                    slots = list(type = "character")
)


#' check_BenefitDeath
#' 
#' @param object object of class \code{BenefitDeath}
#' 
#' function to check validity of DeathBenefit S4 class constructor
check_BenefitDeath <- function(object) {
  errors <- character()
  if (!identical(length(object@t), 
                 length(object@value))) { 
  errors <- c(errors, "Error! DeathBenefit t and value must have same length")
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

#' BenefitDeath
#' 
#' Death Benefit values.  This is meant to be used with the \code{Insuree}
#' class
#' 
#' @slot t time over which death benefit value is applicable
#' @slot value numeric vector defining value of death benefit over t time
#' 
#' @name BenefitDeath-class
#' @rdname BenefitDeath-class
#' @export BenefitDeath
BenefitDeath <- setClass("BenefitDeath",
                     contains = "Benefit",
                     slots = list(t = "numeric",
                                  value = "numeric"),
                     prototype = prototype(t = c(1, 1, 1),
                                           value = c(5, 4, 8)
                                           ),
                     validity = check_BenefitDeath
 )
#benefit_death <- function(t = c(1, 1, 1), 
#                          value = c(5, 4, 8)) {
#  hold <- list("t" = t, 
#               "value" = value)
#  class(hold) <- c("death", "benefit")
#  hold
#} 

#' BenefitAnnuity
#' 
#' Annuity Benefit values.  This is meant to be used with the \code{Insuree}
#' class
#' 
#' @slot t time (from present moment) that the benefit will be payed
#' @slot value numeric vector defining value of over the t time
#' 
#' @name BenefitAnnuity-class
#' @rdname BenefitAnnuity-class
#' @export BenefitAnnuity
BenefitAnnuity <- setClass("BenefitAnnuity",
                         slots = list(t = "numeric",
                                      value = "numeric"),
                         prototype = prototype(t = c(1, 1, 1),
                                               value = c(5, 4, 8)),
                         validity = check_BenefitDeath
)