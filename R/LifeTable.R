#' check_LifeTable
#' 
#' @param object object of class \code{Insuree}
#' 
#' function to check validity of LifeTable S4 class constructor
check_LifeTable <- function(object) {
  errors <- character()
  # check lengths
  if (length(object@x) != length(object@q_x)) { 
    errors <- c(errors, "Error! x and q_x must be the same length")
  }
  # chack that all values are >= 0
  if (any(object@x < 0) || any(object@q_x < 0, na.rm = TRUE)) {
    errors <- c(errors, "Error! all elements of x and q_x must be >= 0")
  }
  if (!is.na(object@q_x[length(object@q_x)])) {
    errors <- c(errors, "Error.  Please change table so last q_x value is set to NA")
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
#' each element of x.  The final q_x should be set to NA.  This is
#' done to remove ambiguity associated with not having the length of
#' the last x value on most actuarial tables having a specified
#' duration. 
#' 
#' @name LifeTable-class
#' @rdname LifeTable-class
#' @export LifeTable
LifeTable <- setClass("LifeTable",
  slots = list(x = "numeric",
               q_x = "numeric"),
  prototype = prototype(x = 0:10,
                        q_x = c(seq(0.05, 0.14, by = 0.01), NA)
              ),
  validity = check_LifeTable
)