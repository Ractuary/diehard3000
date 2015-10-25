#' check_Interest
#' 
#' @param object object of class \code{Interest}
#' 
#' function to check validity of DeathBenefit S4 class constructor
check_Interest <- function(object) {
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
#' intrest rate class for discounting benefits
#' 
#' @slot t time over which the annualized intereste rate is applicable
#' @slot rate annualized interest rate
#' 
#' @name Interest-class
#' @rdname Interest-class
#' @export Interest
Interest <- setClass("Interest",
                     slots = list(t = "numeric",
                                  rate = "numeric"),
                     prototype = prototype(t = c(1, 1, 1),
                                           rate = c(.04, .05, .03)),
                     validity = check_Interest
)