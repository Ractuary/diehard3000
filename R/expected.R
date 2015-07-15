#' expected
#' 
#' calculates the actuarial present value of an individual at
#' birthday \code{x}.
#' 
#' @export
setGeneric("expected", 
           valueClass = "numeric",
           function(object, t_ = NULL, m_ = 0, ...) {
             standardGeneric("expected")
           }
)

 

#' expected
#' 
#' expected number of years lived during term period where term period
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
  
  # calculate life expectancy
  sum(cumprod(1 - trim@q_x))
})