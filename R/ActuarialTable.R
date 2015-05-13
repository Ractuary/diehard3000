#' check_ActuarialTable
#' 
#' function to check validity of LifeTable S4 class constructor
check_ActuarialTable <- function(object) {
  errors <- character()
  if (length(object@x) != length(object@i) || length(object@i) == 1) { 
    errors <- c(errors, "Error! i must have length 1 length equal to x")
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
#' @exportClass ActuarialTable
ActuarialTable <- setClass("ActuarialTable",
  contains = "LifeTable",
  slots = list(i = "numeric"),
  prototype = prototype(i = rep(0.04, 10)),
  validity = check_ActuarialTable
)


#' Z_x
#' 
#' An S4 class to represent a life contingent random variable
#' 
#' @include ActuarialTable.R
#' @slot x_ x value for individual
#' 
#' @name Z_x-class
#' @rdname Z_x-class
#' @exportClass Z_x
Z_x <- setClass("Z_x",
         contains = "ActuarialTable",
         slots = list(x_ = "numeric"),
         prototype = prototype(x_ = 0),
         validity = function(object) identical(length(object@x_), 1L)
       )