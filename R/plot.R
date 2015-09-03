#' hist.rpv_Insuree
#' 
#' plot a histogram of the present value from a rpv() simulation of the
#' \code{Insuree} class.
#' 
#' @param object rpv_Insuree object
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
#' @param object rpv_Insuree object
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

#' hist.rpv_Pool
#' 
#' plot a histogram of the present value from a rpv() simulation of the
#' \code{Pool} class
#' 
#' @param object rpv_Pool object
#' @param ... other arguments
#' 
#' @import ggplot2
#' @import scales
#' 
#' @export
#' @examples
#' test <- rpv(object = Pool(), 
#'             n = 500,
#'             interest = 0.04)
#' hist(test)
hist.rpv_Pool <- function(object, ...) {
  data <- summary.rpv_Pool(object)
  losses <- apply(data, 1, sum)
  losses <- as.data.frame(pv = losses)
  ggplot2::ggplot(losses, aes(x = pv)) +
    geom_histogram(fill = "white", colour = "black") +
    scale_x_continuous(labels = dollar) +
    xlab("Present Value of Death Benefits") +
    ylab("# of Observations") +
    ggtitle(paste0("rpv() Histogram"))
}

#' plot.rpv_Pool
#' 
#' plot the empirical cumulative distribution of 
#' the present value of death benefits from a rpv() 
#' simulation of the \code{Pool} class.
#' 
#' @param object rpv_Pool object
#' @param ... other arguments
#' 
#' @import ggplot2
#' @import scales
#' 
#' @export
#' @examples
#' test <- rpv(object = Pool(), 
#'             n = 500,
#'             interest = 0.04)
#' plot(test)
plot.rpv_Insuree <- function(object, ...) {
  data <- summary.rpv_Pool(object)
  losses <- apply(data, 1, sum)
  losses <- as.data.frame(pv = losses)
  ggplot2::ggplot(data.frame(pv = object$pv), aes(x = pv)) +
    stat_ecdf() +
    scale_x_continuous(labels = dollar) +
    xlab("Present Value of Death Benefits") +
    ylab("F(x)") +
    ggtitle("rpv() Empirical Cumulative Distribution")
}