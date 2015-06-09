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
#' # 3 year term insurance with benefit of 4
#' expected(Z_x(t_ = 3, benefit = 4)) 
#' # 3 year term insurance effective in 1 year
#' expected(Z_x(benefit = 1, x_ = 0, m_ = 1, t_ = 3))
setMethod("expected", signature("Z_x"), function(object) {
  
  q_x <- trim_table(object, slot_ = "q_x", x_ = object@x_, t_ = object@m_ + object@t_)
  i <- trim_table(object, slot_ = "i", x_ = object@x_, t_ = object@m_ + object@t_)
  
  # isolate applicable interest discount
  discount <- discount(i = i)
  
  # isolate applicatbe mortality discount
  tp_x <- c(1, cumprod(1 - q_x[-length(q_x)]))
  
  # calculate the present value of Z_x
  index <- (object@m_ + 1):(object@m_ + object@t_)
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