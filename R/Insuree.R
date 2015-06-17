#' check_Insuree
#' 
#' function to check validity of Insuree S4 class constructor
check_Insuree <- function(object) {
  errors <- character()
  if (!identical(length(object@x_), 
                length(object@t_),
                length(object@m_),
                1L)) { 
    errors <- c(errors, "Error! x_, t_, and m_ must all be of length 1")
  }
  # benefit
  # there must be a benefit value for each x in which the term insurance
  # is active.  We use the ceiling function to the right to account for
  # partial years
  if (length(object@benefit) != ceiling(object@t_ + ifelse(object@x_ %% 1 == 0, 0, 1 - object@x_ %% 1))) {
    errors <- c(errors, "Error! benefit must have length equal to t_")
  }
  
  # validate x_
  if (object@x_ <= min(object@x) || object@x_ >= (max(object@x) + 1)) {
    errors <- c(errors, "Error! x_ must be in range of x on ActuarialTable")
  }
  
  # validate t_
  if (object@t_ <= 0) {
    errors <- c(errors, "Error! t_ must be >= 0")
  }
  if (object@x_ + object@t_ > (max(object@x) + 1L)) {
    errors <- c(errors, "Error! x_ + t_ > (max(x) + 1)")
  }
  
  # validate m_
  if (object@m_ < 0) {
    errors <- c(errors, "Error! m_ must be >= 0")
  }
  if (object@x_ + object@t_ + object@m_ > (max(object@x) + 1L)) {
    errors <- c(errors, "Error! x_ + t_ + m_ > (max(x) + 1)")
  }

  if (identical(length(errors), 0)) {
    TRUE
  } else {
    errors
  }
}


#' Insuree
#' 
#' An individual with some kind of life contingent insurance
#' 
#' @include ActuarialTable.R
#' @slot x_ x value for individual
#' @slot t_ t value for individual
#' @slot m_ m value for individual
#' @slot benefit life contingent benefits.  e.g. For ordinary life insurance
#' payable at the end of the year of death, the benefit would be a single payment.
#' For annuity benefits it could be a variable stream of payments.
#' 
#' @name Insuree-class
#' @rdname Insuree-class
#' @export Insuree
Insuree <- setClass("Insuree",
                contains = "ActuarialTable",
                slots = list(x_ = "numeric",
                             t_ = "numeric",
                             m_ = "numeric",
                             benefit = "numeric"),
                prototype = prototype(x_ = 2,
                                      t_ = 3,
                                      m_ = 0,
                                      benefit = c(1, 1, 1)),
                validity = check_Insuree
)