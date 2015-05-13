#' check_LifeTable
#' 
#' function to check validity of LifeTable S4 class constructor
check_LifeTable <- function(object) {
  errors <- character()
  if (length(object@x) != length(object@q_x)) { 
    errors <- c(errors, "Error! x and q_x are not the same length")
  }
  if (any(object@x) | any(object@q_x) < 0) {
    errors <- c(errors, "Error! all elements of x and q_x must be >= 0")
  }
  
  if (identical(length(errors), 0)) {
    TRUE
  } else {
    errors
  }
}

#' LifeTable
#' 
#' An S4 class to represent a life table
#'
#' @slot x a length n ascending numeric vector
#' @slot q_x a length n numeric vector for probability of death at
#' each element of x
#' 
#' @name LifeTable-class
#' @rdname LifeTable-class
#' @exportClass LifeTable
LifeTable <- setClass("LifeTable",
  slots = list(x = "numeric", q_x = "numeric"),
  prototype = prototype(x = 0:9, 
                        q_x = seq(0.05, 0.14, by = 0.01)),
  validity = check_LifeTable
)

#' T_x
#' 
#' An S4 class to represent a life contingent random variable
#' 
#' @include LifeTable.R
#' @slot x_ x value for individual
#' 
#' @name T_x-class
#' @rdname T_x-class
#' @exportClass T_x
T_x <- setClass("T_x",
                contains = "LifeTable",
                slots = list(x_ = "numeric"),
                prototype = prototype(x_ = 0),
                validity = function(object) identical(length(object@x_), 1L)
)