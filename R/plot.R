#' hist.rpv_Life
#' 
#' plot a histogram of the present value from a rpv() simulation of the
#' \code{Life} class.
#' 
#' @param object object of class \code{rpv_Insuree}
#' @param ... other arguments
#' 
#' @import ggplot2
#' @import scales
#' @import methods
#' 
#' @export
#' @examples
#' test <- rpv(object = Life(benefit = list(BenefitDeath())), 
#'             n = 500,
#'             interest = Interest(t = 20, rate = 0.04))
#' hist(test)
hist.rpv_Life <- function(object, ...) {
  ggplot2::ggplot(data.frame(pv = object$pv), aes(x = pv)) +
    geom_histogram(fill = "white", colour = "black") +
    scale_x_continuous(labels = dollar) +
    xlab("Present Value of Death Benefit") +
    ylab("# of Observations") +
    ggtitle(paste0("rpv() Histogram"))
}

#' plot.rpv_Life
#' 
#' plot the empirical cumulative distribution of 
#' the present value of death benefits from a rpv() 
#' simulation
#' 
#' @param object object of class rpv_Life
#' @param ... other arguments
#' 
#' @import ggplot2
#' @import scales
#' @import methods
#' 
#' @export
#' @examples
#' test <- rpv(object = Life(benefit = list(BenefitDeath())), 
#'             n = 500,
#'             interest = Interest(t = 5, rate = 0.04))
#' plot(test)
plot.rpv_Life <- function(object, ...) {
  ggplot2::ggplot(data.frame(pv = object$pv), aes(x = pv)) +
    stat_ecdf() +
    scale_x_continuous(labels = dollar) +
    xlab("Present Value of Death Benefit") +
    ylab("F(x)") +
    ggtitle("rpv() Empirical Cumulative Distribution")
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
#' @import methods
#' 
#' @export
#' @examples
#' test <- rpv(object = Pool(), 
#'             n = 500,
#'             interest = Interest(t = 10, rate = 0.04))
#' hist(test)
hist.rpv_Pool <- function(object, ...) {
  data <- summary(object)
  losses <- apply(data, 1, sum)
  losses <- data.frame(pv = losses)
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
#' @import methods
#' 
#' @import ggplot2
#' @import scales
#' 
#' @export
#' @examples
#' test <- rpv(object = Pool(), 
#'             n = 500,
#'             interest = Interest(t = 5, rate = 0.04))
#' plot(test)
plot.rpv_Pool <- function(object, ...) {
  data <- summary(object)
  losses <- apply(data, 1, sum)
  losses <- data.frame(pv = losses)
  ggplot2::ggplot(losses, aes(x = pv)) +
    stat_ecdf() +
    scale_x_continuous(labels = dollar) +
    xlab("Present Value of Death Benefits") +
    ylab("F(x)") +
    ggtitle("rpv() Empirical Cumulative Distribution")
}