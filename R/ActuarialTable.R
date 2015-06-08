#' check_ActuarialTable
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
  prototype = prototype(i = rep(0.04, 10)),
  validity = check_ActuarialTable
)


#' Z_x
#' 
#' The present value random variable for a life contingent 
#' insurance payment payable at the end of the year of death
#' 
#' @include ActuarialTable.R
#' @slot x_ x value for individual
#' @slot benefit the amount payable at the end of the year of death
#' 
#' @name Z_x-class
#' @rdname Z_x-class
#' @export Z_x
Z_x <- setClass("Z_x",
         contains = "ActuarialTable",
         slots = list(x_ = "numeric",
                      benefit = "numeric"),
         prototype = prototype(x_ = 2, benefit = 10),
         validity = function(object) identical(length(object@x_), 1L)
       )


#' Y_x
#'
#' present value random variable for life contingent
#' annuity payments
#'
#' @include ActuarialTable.R
#' @slot x_ x value for individual
#' @slot benefit the annuity benefit
#' 
#' @name Y_x-class
#' @rdname Y_x-class
#' @export Y_x
Y_x <- setClass("Y_x",
         contains = "Z_x",
         slots = list(annuity = "numeric"),
         prototype = prototype(annuity = 1),
         validity = function(object) identical(length(object@annuity), 1L)
)