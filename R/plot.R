#' hist.rpv_Insuree
#' 
#' plot a histogram of the present value from a rpv_life() simulation
#' 
#' @param object rpv_life object
#' @param ... other arguments
#' 
#' @import ggplot2
#' @import scales
#' 
#' @export
#' @examples
#' test <- rpv(object = Insuree(), 
#'             n = 500,
#'             interest = 0.04)
#' hist(test)
hist.rpv_Insuree <- function(object, ...) {
  ggplot2::ggplot(data.frame(pv = object$pv), aes(x = pv)) +
    geom_histogram(fill = "white", colour = "black") +
    scale_x_continuous(labels = dollar) +
    xlab("Present Value of Death Benefit") +
    ylab("# of Observations") +
    ggtitle(paste0("rpv_life() Histogram"))
}

#' plot.rpv_Insuree
#' 
#' plot the empirical cumulative distribution of 
#' the present value of death benefits from a rpv_life() 
#' simulation
#' 
#' @param object rpv_life object
#' @param ... other arguments
#' 
#' @import ggplot2
#' @import scales
#' 
#' @export
#' @examples
#' test <- rpv(object = Insuree(), 
#'             n = 500,
#'             interest = 0.04)
#' plot(test)
plot.rpv_Insuree <- function(object, ...) {
  ggplot2::ggplot(data.frame(pv = object$pv), aes(x = pv)) +
    stat_ecdf() +
    scale_x_continuous(labels = dollar) +
    xlab("Present Value of Death Benefit") +
    ylab("F(x)") +
    ggtitle("rpv_life() Empirical Cumulative Distribution")
}