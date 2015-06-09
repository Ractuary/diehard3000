#' check_Z_x
#' 
#' function to check validity of LifeTable S4 class constructor
check_Z_x <- function(object) {
  errors <- character()
  if (!identical(length(object@x_), 
                length(object@t_),
                length(object@m_),
                length(object@benefit),
                1L)) { 
    errors <- c(errors, "Error! x_, t_, m_ and benefit must all be of length 1")
  }
  #' validate_x_
  if (length(object@x_ %in% object@x) != 1) {
    errors <- c(errors, "Error! x_ must be in x of ActuarialTable")
  }
  
  #' validate_t_
  if (object@t_ <= 0) {
    errors <- c(errors, "Error! t_ must be >= 0")
  }
  if (object@x_ + object@t_ > max(object@x)) {
    errors <- c(errors, "Error! x_ + t_ > max(x)")
  }
  
  #' validate_m_
  if (object@m_ < 0) {
    errors <- c(errors, "Error!, x_ + t_ + m_ > max(x)")
  }

  if (identical(length(errors), 0)) {
    TRUE
  } else {
    errors
  }
}


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
                             t_ = "numeric",
                             m_ = "numeric",
                             benefit = "numeric"),
                prototype = prototype(x_ = 2,
                                      t_ = 1,
                                      m_ = 0,
                                      benefit = 1),
                validity = check_Z_x
)