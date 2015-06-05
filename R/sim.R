#' sim
#' 
#' simulates \code{T_x} (curtate years until death) in accordance
#' with the multinomial distribution provided by the life table.
#' 
#' @export
setGeneric("sim", 
           #valueClass = "numeric",
           function(object, t_ = NULL, n) {
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
#' @param n number of observations
#' 
#' @export
#' @examples
#' sim(object = T_x(), t_ = 3, n = 1000)
setMethod("sim", signature("T_x"), function(object, t_ = (max(object@x) - object@x_), n) {
  # find the probability of death in each x for a person age x_
  tp_x8q_x <- tp_x8q_x(object, t_ = t_)
  
  # run the simulation
  deaths <- as.vector(rmultinom(n = 1, size = n, prob = tp_x8q_x))
  x <- trim_table(object, slot_ = "x", x_ = object@x, t_ = t_)
  data.frame(x =  c(x, x[length(x)] + 1),
             t = 0:(length(deaths) - 1),
             deaths = deaths)
})

