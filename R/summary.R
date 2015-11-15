#' summary.rpv_Life
#' 
#' summary of rpv() simulation of class \code{Life}
#' 
#' @param object \code{rpv_Life} object
#' @param ... other arguments
#' 
#' @export
#' @examples
#' test <- rpv(object = Life(benefit = list(BenefitDeath())), 
#'             n = 500,
#'             interest = Interest(t = 10, rate = 0.04))
#' summary(test)
summary.rpv_Life <- function(object, ...) {
  object
}

#' summary.rpv_Pool
#' 
#' summary of rpv() simulation of the \code{Pool} class
#' 
#' @param object \code{rpv_Pool} object
#' @param ... other arguments
#' 
#' @export
#' @examples
#' test <- rpv(object = Pool(), 
#'             n = 500,
#'             interest = Interest(t = 10, rate = 0.04))
#' summary(test)
summary.rpv_Pool <- function(object, ...) {
  pv <- lapply(object, function(x) x$pv)
  pv <- as.data.frame(pv)
  
  names2 <- vector(mode = "character", length = length(object))
  for (k in seq_along(object)) {
    names2[k] <- paste0("Insuree", k)
  }
  names(pv) <- names2
  pv
}