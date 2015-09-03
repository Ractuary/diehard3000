#' check_Pool
#' 
#' @param object object of class \code{Pool}
#' 
#' function to check validity of \code{Pool} S4 class
check_Pool <- function(object) {
  errors <- character()
  all_insuree_class <- unlist(lapply(object@insurees, inherits, "Insuree"))
  if (!all(all_insuree_class)) { 
    errors <- c(errors, "All list elements supplied to Pool() must be
                of class Insuree")
  }
  if (identical(length(errors), 0)) {
    TRUE
  } else {
    errors
  }
}


#' Pool
#' 
#' S4 class for a group of \code{Insuree} objects
#' 
#' @slot insurees list of objects of class \code{Insuree}
#' 
#' @name Pool-class
#' @rdname Pool-class
#' @export Pool
Pool <- setClass("Pool",
          slots = list(insurees = "ANY"),
          prototype = prototype(insurees = list(Insuree(), 
                                             Insuree()
                                           )
          ),
          validity = check_Pool
)