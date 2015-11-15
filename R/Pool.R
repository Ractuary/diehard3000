#' check_Pool
#' 
#' @param object object of class \code{Pool}
#' 
#' function to check validity of \code{Pool} S4 class
check_Pool <- function(object) {
  errors <- character()
  all_insuree_class <- unlist(lapply(object@lives, inherits, "Life"))
  if (!all(all_insuree_class)) {
    errors <- c(errors, "All list elements supplied to Pool() must be
                of class 'Life'")
  }
  if (identical(length(errors), 0)) {
    TRUE
  } else {
    errors
  }
}


#' Pool
#' 
#' S4 class for a group of \code{Life} objects
#' 
#' @slot lives list of objects of class \code{Life}
#' 
#' @name Pool-class
#' @rdname Pool-class
#' @export Pool
Pool <- setClass("Pool",
          slots = list("lives" = "ANY"),
          prototype = prototype(lives = list(Life(benefit = list(BenefitDeath())), 
                                             Life(benefit = list(BenefitDeath()))
                                           )
          ),
          validity = check_Pool
)