#' check_LifeTable
#' 
#' @param object object of class \code{Insuree}
#' 
#' function to check validity of LifeTable S4 class constructor
check_LifeTable <- function(object) {
  errors <- character()
  # check lengths
  if (length(object@x) != length(object@q_x) || length(object@x) != length(object@t)) { 
    errors <- c(errors, "Error! x, t, and q_x must be the same length")
  }
  # chack that all values are >= 0
  if (any(object@x < 0) || any(object@q_x < 0) || any(object@t < 0)) {
    errors <- c(errors, "Error! all elements of x, t, and q_x must be >= 0")
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
#' @slot t a length n numeric vecotr indicating the t_ value
#' for each x appropriate that corresponds with the q_x element in
#' the table.  Important for last element and for deferral insurance.
#' @slot q_x a length n numeric vector for probability of death at
#' each element of x
#' 
#' @name LifeTable-class
#' @rdname LifeTable-class
#' @export LifeTable
LifeTable <- setClass("LifeTable",
  slots = list(x = "numeric",
               t = "numeric",
               q_x = "numeric"),
  prototype = prototype(x = 0:9,
                        t = rep(1, times = 10),
                        q_x = seq(0.05, 0.14, by = 0.01)),
  validity = check_LifeTable
)