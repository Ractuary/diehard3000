#' rdeath
#' 
#' simulates \code{T_x} (curtate year of death) in accordance
#' with the multinomial distribution provided by the life table.
#' 
#' @export
setGeneric("rdeath", 
           #valueClass = "numeric",
           function(object, t_ = (max(object@x) - object@x_), n) {
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
#' @param n number of observations
#' 
#' @export
#' @examples
#' rdeath(object = Z_x(t_ = 4), n = 5)
#' rdeath(object = Z_x(x_ = 2, t_ = 3), n = 5)
setMethod("rdeath", signature("Z_x"), function(object, n) {
  # find the probability of death in each x for a person age x_
  tp_x8q_x <- tp_x8q_x(object)
  
  # run the simulation
  deaths <- rmultinom(n = n, size = 1, prob = tp_x8q_x)
  deaths <- deaths[-nrow(deaths), , drop = FALSE]
  deaths[deaths == 1] <- object@benefit
  deaths <- as.data.frame(deaths)
    
  i <- trim_table(object, slot_ = "i", x_ = object@x_, t_ = object@t_)
  discount <- discount(i)
  out <- lapply(deaths, function(j) j * discount)
  
  x <- trim_table(object, slot_ = "x", x_ = object@x_, t_ = object@t_)
  data.frame(x =  x,
             t = 1:length(x),
             deaths = out)
})