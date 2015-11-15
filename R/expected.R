#' mean_life
#' 
#' calculates the mean amount of time the \code{object} argument is expected
#' to live
#' 
#' @param object object of one of several insuree package classes
#' @param ... addtitional parameters
#' 
#' @export
setGeneric("mean_life", 
           valueClass = "numeric",
           function(object, ...) {
             standardGeneric("mean_life")
           }
)

 

#' mean_life
#' 
#' The expected time lived of an object of class \code{LifeTable}
#' in the \code{object} argument.  Expected time lived only includes times
#' between x_ + m_ and t_.
#' 
#' @param object object of class LifeTable
#' @param x_ x_
#' @param t_ t_
#' @param m_ m_
#' 
#' @export
#' @examples
#' mean_life(LifeTable(), x_ = 2, t_ = 3, m_ = 0)
#' mean_life(LifeTable(), x_ = 2, t_ = 3, m_ = 1)
#' mean_life(LifeTable(), x_ = 2.5, t_ = 3, m_ = 1)
setMethod("mean_life", signature("LifeTable"), function(object, 
                                                       x_ = object@x[1], 
                                                       t_ = NULL, 
                                                       m_ = 0) {
  # if t_ is NULL assmume term period lasts for entire remaining LifeTable
  if (is.null(t_)) {
    t_ <- max(object@x) - x_ + m_
  }
  
  # trim the LifeTable
  trim <- object[x_ + m_, t_]
  death_probs <- tp_x8q_x(trim)
  ## calculate life expectancy
  t <- diff(trim@x)
  sum(t * cumprod(1 - trim@q_x[-length(trim@q_x)])) + # individual survives the interval
    sum(t * 0.5 * death_probs[-length(death_probs)]) # individual dies during interval, death
                                 # is assumed to be at midpoint of interval
})


#' mean_life
#' 
#' The expected time lived provided an object of class \code{Life}
#' in the \code{object} argument.  Expected time lived only includes times
#' between x_ + m_ and t_.
#' 
#' @param object object of class \code{Life}
#' 
#' @export
#' @examples
#' mean_life(Life())
#' mean_life(Life(x_ = 2.5, t_ = 3, m_ = 1))
setMethod("mean_life", signature("Life"), function(object) {
  lt <- LifeTable(x = object@life_table@x, q_x = object@life_table@q_x)
  mean_life(object = lt, x_ = object@x_, t_ = object@t_, m_ = object@m_)
})