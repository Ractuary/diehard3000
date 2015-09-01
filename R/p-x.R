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
           function(object, x_, t_ = 1) {
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
#' p_x(LifeTable(), x_ = 2.5, t_ = 2)
#' p_x(LifeTable(), x_ = 2.5, t_ = 2.5)
#' p_x(LifeTable(), x_ = 2.5, t_ = 3.5)
setMethod("p_x", signature("LifeTable"), function(object, x_ = min(object@x), t_ = 1) {
  stopifnot(length(t_) == 1)
  stopifnot(length(x_) == 1)
  
  # remove all q_x rows less than x argument
  q_x <- object@q_x[index(object@x, x_ = x_, m_t_ = t_)]
  x_old <- object@x[index_x(object@x, x_ = x_, m_t_ = t_)]
  t_old <- diff(x_old)
  .x <- new_x(object@x, x_ = x_, m_t_ = t_)
  t_new <- diff(.x)
  
  # calculate new q_x values
  .q_x <- c()
  for (k in seq_along(q_x)) {
    .q_x[k] <- (q_x[k] * (t_new[k] / t_old[k])) / 
                  (1 - q_x[k] * (1 - (min(.x[k + 1], x_old[k + 1]) - .x[k]) / t_new[k]))
  }
  
  # calculate probability of survival: t_p_x
  prod(1 - .q_x)
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
  tp_x <- cumprod(1 - object@q_x[-length(object@q_x)])
  
  # calculates probability of death in each x
  tp_x8q_x <- object@q_x[1]
  if (length(object@q_x) > 2) {
    for (j in 2:length(object@q_x[-length(object@q_x)])) {
      tp_x8q_x[j] <- tp_x[j - 1] * object@q_x[j]
    }
  }
  
  # return probabilities
  # final value is probability of survival
  c(tp_x8q_x, 1 - sum(tp_x8q_x))
}