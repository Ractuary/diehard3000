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
#' @param x_ x_
#' @param t_ t_
#' 
#' @export
#' @examples
#' p_x(LifeTable(), x_ = 2.5, t_ = .5) # probability of x = 3 surviving 5 years
setMethod("p_x", signature("LifeTable"), function(object, t_ = 1, x_ = min(object@x)) {
  
  # remove all q_x rows less than x argument
  q_x <- trim_table(object, slot_ = "q_x", x_ = x_, t_ = t_)
  
  # calculation for partial q_x[1] if x_ is not an integer
  # assumes uniform mortality over course of year
  partial_x_ <- x_ %% 1
  if (partial_x_ != 0) {
    q_x[1] <- (q_x[1] * (1 - partial_x_)) / (1 - q_x[1] * partial_x_)
  }
  
  # calculation for partial q_x[length(q_x)] if (x_ + t_) is not an integer
  # assumes uniform mortality over course of year
  partial_t_ <- (x_ + t_) %% 1 
  if (partial_t_ != 0) {
    last_q_x <- length(q_x)
    q_x[last_q_x] <-  (q_x[last_q_x] - 
                         (q_x[last_q_x] * (1 - partial_t_)) / 
                         (1 - q_x[last_q_x] * partial_t_))
  }
  
  prod(1 - q_x)
})