#' mean
#' 
#' calculates the actuarial present value of an individual at
#' birthday \code{x}.
#' 
#' @export
setGeneric("mean", 
           valueClass = "numeric",
           function(object, t_ = NULL, m_ = 0) {
             standardGeneric("mean")
           }
)

 

#' mean
#' 
#' expected number of complete years for \code{T_x} to survive
#' 
#' @param object object of class T_x
#' @param t_ t
#' @param m_ m
#' 
#' @export
#' @examples
#' mean(new("T_x"), t_ = 3)
setMethod("mean", signature("T_x"), function(object, t_ = NULL, m_ = 0) {
  # isolate all q_x >= T_x@x_ 
  q_x <- object@q_x[object@x >= object@x_]
  
  # set t if not specified
  if (is.null(t_)) {
    t_ <- length(q_x)
  } else {
    stopifnot(t_ >= 1)
    stopifnot(m_ >= 0)
    # stop if life_table not large enough; t_ + m_ exceeds remaining q_x values
    stopifnot(length(q_x) >= m_ + t_)
  }
  
  # calculate curtate life expectancy
  sum(cumprod(1 - q_x[(1 + m_):(t_ + m_)]))
})

#' mean
#' 
#' expected number of complete years for \code{Z_x} to survive discounted for interest
#' 
#' @param object object of class Z_x
#' @param t_ t
#' @param m_ m
#' 
#' @export
#' @examples
#' mean(new("Z_x"), t_ = 3)
setMethod("mean", signature("Z_x"), function(object, t_ = NULL, m_ = 0) {
  # isolate all q_x >= T_x@x_ 
  q_x <- object@q_x[object@x >= object@x_]
  i <- object@i[object@x >= object@x_]
  
  # set t if not specified
  if (is.null(t_)) {
    t_ <- length(q_x)
  } else {
    stopifnot(t_ >= 1)
    stopifnot(m_ >= 0)
    # stop if life_table not large enough; t_ + m_ exceeds remaining q_x values
    stopifnot(length(q_x) >= m_ + t_)
  }
  
  x_trend <- 1 + object@i
  x_discount <- 1 / x_trend
  discount <- cumprod(x_discount[1:(m_ + t_)])
  discount <- discount[(m_ + 1):(m_ + t_)]
  tp_x <- cumprod(1 - q_x[(m_ + 1):(t_ + m_)])
  tp_x_discount <- tp_x * discount
  
  # calculate curtate life expectancy
  sum(tp_x_discount)
})