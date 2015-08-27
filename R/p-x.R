#' p_x
#' 
#' probability of survival for person at birthday \code{x}
#' 
#' @param object object of class LifeTable
#' @param x_ x_
#' @param t_ t_
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
#' p_x(LifeTable(), x_ = 2.5, t_ = 2) # probability of x = 3 surviving 5 years
setMethod("p_x", signature("LifeTable"), function(object, t_ = 1, x_ = min(object@x)) {
  stopifnot(length(t_) == 1)
  stopifnot(length(x_) == 1)
  # remove all q_x rows less than x argument
  q_x <- object@q_x[index(object, x_ = x_, m_t_ = t_)]
  
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

#' tp_x8q_x
#' 
#' returns the probability of death in each x for a percon age x_.  This function
#' converts the `q_x` slot from the probability of death given the individual
#' is age x to the probability of death given the individual is age x_. 
#' 
#' @param object LifeTable object
#' 
#' @export
#' @examples
#' tp_x8q_x(LifeTable())
tp_x8q_x <- function(object) {
  # prob of surviving to each x
  tp_x <- cumprod(1 - object@q_x)
  
  # calculates probability of death in each x
  tp_x8q_x <- object@q_x[1]
  if (length(object@q_x) > 1) {
    for (j in 2:length(object@q_x)) {
      tp_x8q_x[j] <- tp_x[j - 1] * object@q_x[j]
    }
  }
  
  # return probabilities
  # final value is probability of survival
  c(tp_x8q_x, 1 - sum(tp_x8q_x))
}