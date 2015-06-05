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
#' expected number of complete years for \code{T_x} to survive
#' 
#' @param object object of class T_x
#' @param t_ t
#' @param m_ m
#' 
#' @export
#' @examples
#' expected(LifeTable(), x_ = 0, t_ = 3)
setMethod("expected", signature("LifeTable"), function(object, 
                                                       x_ = object@x[1], 
                                                       t_ = max(object@x) - object@x[1], 
                                                       m_ = 0) {
  validate_x_(object, x_ = x_)
  validate_t_(object, x_ = x_, t_ = t_)
  validate_m_(object, x_ = x_, t_ = t_, m_ = m_)
  
  q_x <- trim_table(object, slot_ = "q_x", x_ = x_, t_ = t_, m_ = m_)
 
  # calculate curtate life expectancy
  sum(cumprod(1 - q_x))
})

#' expected
#' 
#' expected present value of an insurance payment \code{Z_x}.  \code{Z_x}
#' is discounted for interest and mortality. 
#' 
#' @param object object of class Z_x
#' @param t_ t
#' @param m_ m
#' 
#' @export
#' @examples
#' # 3 year term insurance with a payable amount or 4 
#' expected(Z_x(), t_ = 3) 
#' # 3 year term insurance effective in 1 year
#' expected(Z_x(benefit = 1, x_ = 0), t_ = 3, m_ = 1)
setMethod("expected", signature("Z_x"), function(object, t_ = max(object@x) - object@x_, m_ = 0) {

  validate_t_(object, x_ = object@x_, t_ = t_)
  validate_m_(object, x_ = object@x_, t_ = t_, m_ = m_)
  
  q_x <- trim_table(object, slot_ = "q_x", x_ = object@x_, t_ = m_ + t_)
  i <- trim_table(object, slot_ = "i", x_ = object@x_, t_ = m_ + t_)
  
  index <- (m_ + 1):(m_ + t_)
  # isolate applicable interest discount
  x_trend <- 1 + i
  x_discount <- 1 / x_trend
  discount <- cumprod(x_discount)
  
  # isolate applicatbe mortality discount
  tp_x <- c(1, cumprod(1 - q_x[-length(q_x)]))
  
  # calculate the present value of Z_x
  out <- tp_x[index] * discount[index] * q_x[index]
  out <- sum(out)
  out * object@benefit
})

#' expected
#' 
#' expected value of \code{Y_x}
#' 
#' @param object object of class Z_x
#' @param t_ t
#' @param m_ m
#' @param payable character string indicating the time the annuity
#' payments are made.  Can either be "beg" or "end".  Default value
#' set to "end".
#' 
#' @export
#' @examples
#' expected(new("Y_x"), t_ = 3)
setMethod("expected", signature("Y_x"), function(object, t_ = NULL, m_ = 0, payable = "end") {
  # need to think about this
  # might want to remove Y_x class and calculte expected Y_x in Z_x expected()
})