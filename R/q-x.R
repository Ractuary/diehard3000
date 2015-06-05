#' q_x
#' 
#' probability of death for a person at birthday birthday \code{x}
#' in the following t years
#' 
#' @export
setGeneric("q_x",
           valueClass = "numeric",
           function(object, x_, t_ = 1, m_ = 0) {
             standardGeneric("q_x")
           }
)

#' q_x
#' 
#' probability of death for person at birthday x
#' 
#' @param life_table object of class life_table
#' @param x_ x
#' @param t_ t
#' @param m_ m
#' 
#' @export
#' @examples
#' q_x(new("LifeTable"), x = 3, t = 5) # probability of x = 3 surviving 5 years
#' q_x(new("LifeTable"), x = 2, t = 4, m = 1) # probability of x = 2 surviving 1 year and 
setMethod("q_x", signature("LifeTable"), function(object, x_, t_ = 1, m_ = 0) {
  validate_m_(object, x_, t_, m_)
  
  # find prob of survival for m years
  if (m_ > 0) {
    p <- p_x(object, x_ = x_, t_ = m_)
    x_ <- x_ + m_
  } else {
    p <- 1
  }
  
  # prob of survival from x_ + m_ to x_ + m_ + t_
  q <- 1 - p_x(object, x_ = x_, t_ = t_)
  
  p * q
})