#' apv
#' 
#' calculates the actuarial present value of an individual at
#' birthday \code{x}.
#' 
#' @export
setGeneric("apv", 
           valueClass = "numeric",
           function(object, t, m) {
             standardGeneric("apv")
           }
)

 

#' apv
#' 
#' @param object object of class LifeTable
#' @param x_ x
#' @param t_ t
#' 
#' @export
#' @examples
#' apv(new("T_x"), t = 3)
setMethod("apv", signature("T_x"), function(object, t = NULL, m = 0) {
  # isolate all q_x >= T_x@x_ 
  q_x <- object@q_x[object@x >= object@x_]
  
  # set t if not specified
  if (is.null(t)) {
    t <- length(q_x)
  } else {
    # stop if life_table not large enough; t exceeds remaining q_x values
    stopifnot(length(q_x) >= t)
  }
  
  # calculate kurtate life expectancy
  sum(cumprod(1 - q_x[1:t]))
})