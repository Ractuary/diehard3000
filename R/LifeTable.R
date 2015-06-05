#' check_LifeTable
#' 
#' function to check validity of LifeTable S4 class constructor
check_LifeTable <- function(object) {
  errors <- character()
  if (length(object@x) != length(object@q_x)) { 
    errors <- c(errors, "Error! x and q_x are not the same length")
  }
  if (any(object@x < 0) || any(object@q_x < 0)) {
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
#' @export LifeTable
LifeTable <- setClass("LifeTable",
  slots = list(x = "numeric", q_x = "numeric"),
  prototype = prototype(x = 0:9, 
                        q_x = seq(0.05, 0.14, by = 0.01)),
  validity = check_LifeTable
)