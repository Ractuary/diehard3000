#' p_x
#' 
#' probability of survival for person at birthday \code{x}
#' 
#' @export
setGeneric("p_x", 
           valueClass = "numeric",
           function(object, t_ = 1, ...) {
             standardGeneric("p_x")
           }
)

#' p_x
#' 
#' probability of survival for person at birthday x
#' 
#' @param object object of class LifeTable
#' @param x_ x
#' @param t_ t
#' 
#' @export
#' @examples
#' p_x(LifeTable(), x_ = 3, t_ = 5) # probability of x = 3 surviving 5 years
setMethod("p_x", signature("LifeTable"), function(object, t_ = 1, x_ = min(object@x)) {
  validate_x_(object, x_)
  validate_t_(object, x_, t_)
  
  # remove all q_x rows less than x argument
  q_x <- trim_table(object, slot_ = "q_x", x_ = x_, t_ = t_)
  
  # calculate curtate life expectancy
  prod(1 - q_x)
})