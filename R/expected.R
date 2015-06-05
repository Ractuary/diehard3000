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
#' expected(T_x(), t_ = 3)
setMethod("expected", signature("T_x"), function(object, t_ = NULL, m_ = 0) {
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
#' expected(Z_x(payable = 4), t_ = 3) 
#' # 3 year term insurance effective in 1 year
#' expected(Z_x(), t_ = 3, m_ = 1)
setMethod("expected", signature("Z_x"), function(object, t_ = NULL, m_ = 0) {
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
  
  # isolate applicable interest discount
  x_trend <- 1 + object@i
  x_discount <- 1 / x_trend
  discount <- cumprod(x_discount[1:(m_ + t_)])
  discount <- discount[(m_ + 1):(m_ + t_)]
  
  # isolate applicatbe mortality discount
  tp_x <- c(1, cumprod(1 - q_x[1:(t_ + m_ - 1)]))
  tp_x <- tp_x[(m_ + 1):(t_ + m_)]
  
  # calculate the present value of Z_x
  z <- tp_x * discount * q_x[(m_ + 1):(t_ + m_)]
  z <- sum(z)
  z * object@payable
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