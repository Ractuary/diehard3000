#' expected
#' 
#' calculates the actuarial present value of an individual at
#' birthday \code{x}.
#' 
#' @param object object of class LifeTable
#' @param x_ x_
#' @param t_ t_
#' @param m_ m_
#' 
#' @export
setGeneric("expected", 
           valueClass = "numeric",
           function(object, x_, t_ = NULL, m_ = 0) {
             standardGeneric("expected")
           }
)

 

#' expected
#' 
#' expected time lived during term period where term period
#' is defined as the period between x_ + m_ and t_.
#' 
#' @param object object of class LifeTable
#' @param x_ x_
#' @param t_ t_
#' @param m_ m_
#' 
#' @export
#' @examples
#' expected(LifeTable(), x_ = 2, t_ = 3, m_ = 0)
#' expected(LifeTable(), x_ = 2, t_ = 3, m_ = 1)
#' expected(LifeTable(), x_ = 2.5, t_ = 3, m_ = 1)
setMethod("expected", signature("LifeTable"), function(object, 
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