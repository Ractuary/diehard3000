#' rdeath
#' 
#' simulates \code{T_x} (curtate year of death) in accordance
#' with the multinomial distribution provided by the life table.
#' 
#' @export
setGeneric("rdeath", 
           #valueClass = "numeric",
           function(object, t_ , n) {
             standardGeneric("rdeath")
           }
)



#' rdeath
#' 
#' simulated for \code{Z_x}.  The
#' simulation is in accordance with the multinomial distribution described
#' by the LifeTable object.
#' 
#' @param object object of class Z_x
#' @param t_ t
#' @param n number of observations
#' 
#' @export
#' @examples
#' rdeath(object = Z_x(), t_ = 3, n = 5)
setMethod("rdeath", signature("Z_x"), function(object, t_ = (max(object@x) - object@x_), n) {
  # find the probability of death in each x for a person age x_
  tp_x8q_x <- tp_x8q_x(object, t_ = t_)
  
  # run the simulation
  deaths <- rmultinom(n = n, size = 1, prob = tp_x8q_x)
  deaths[deaths == 1] <- object@benefit
  
  x <- trim_table(object, slot_ = "x", x_ = object@x, t_ = t_)
  data.frame(x =  c(x, x[length(x)] + 1),
             t = c(1:(length(x)), NA),
             deaths = deaths)
})