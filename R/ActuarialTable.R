#' check_ActuarialTable
#' 
#' @param object object of class ActuarialTable
#' 
#' function to check validity of LifeTable S4 class constructor
check_ActuarialTable <- function(object) {
  errors <- character()
  if (length(object@x) != length(object@i)) { 
    errors <- c(errors, "Error! i must have length equal to length x")
  }
  
  if (identical(length(errors), 0)) {
    TRUE
  } else {
    errors
  }
}

#' ActuarialTable
#' 
#' An S4 class to represent an actuarial table
#' 
#' @include LifeTable.R
#' @slot i a length n numeric vector of interest rates
#' 
#' @name ActuarialTable-class
#' @rdname ActuarialTable-class
#' @export ActuarialTable
ActuarialTable <- setClass("ActuarialTable",
  contains = "LifeTable",
  slots = list(i = "numeric"),
  prototype = prototype(i = c(rep(0.04, 10), NA)),
  validity = check_ActuarialTable
)







