#' sim
#' 
#' simulates \code{T_x} (curtate years until death) in accordance
#' with the multinomial distribution provided by the life table.
#' 
#' @export
setGeneric("sim", 
           valueClass = "numeric",
           function(object, t_ = NULL) {
             standardGeneric("sim")
           }
)



#' sim
#' 
#' simulated number of complete years for \code{T_x} to survive.  The
#' simulation is in accordance with the multinomial distribution described
#' by the life table.
#' 
#' @param object object of class T_x
#' @param t_ t
#' 
#' @export
#' @examples
#' sim(T_x(), t_ = 3)
setMethod("sim", signature("T_x"), function(object, t_ = NULL) {
  # isolate all q_x >= T_x@x_ 
  q_x <- object@q_x[object@x >= object@x_]
  # remove q_x > t_
  if (!is.null(t_)) {
    q_x <- q_x[1:t_]  
  }
  
  # TODO: add tpx values.  Trying to come up with
  # one tpx that can be used anywhere
  #tpx <- vapply(x_:) 
  mqx <- list(vector, length(q_x))
  mqx[1] <- q_x[1]
  for (j in 2:length(q_x)) {
    mqx[j] <- tpx[j - 1] * q_x[j]
  }
  unlist(mqx)
  
})

