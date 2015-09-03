#' summary.rpv_life
#' 
#' summary of rpv_life() simulation
#' 
#' @param object rpv_life object
#' @param ... other arguments
#' 
#' @export
#' @examples
#' test <- rpv_life(object = Insuree(), 
#'                  n = 500,
#'                  interest = 0.04)
#' summary(test)
summary.rpv_life <- function(object, ...) {
  object
}