#' Y_x
#'
#' present value random variable for life contingent
#' annuity payments
#'
#' @include Z_x.R
#' @slot x_ x value for individual
#' @slot benefit the annuity benefit
#' 
#' @name Y_x-class
#' @rdname Y_x-class
#' @export Y_x
Y_x <- setClass("Y_x",
                contains = "Z_x",
                slots = list(annuity = "numeric"),
                prototype = prototype(annuity = 1),
                validity = function(object) identical(length(object@annuity), 1L)
)