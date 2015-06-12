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
#' rdeath(object = Z_x(t_ = 4, m_ = 2), n = 5)
#' rdeath(object = Z_x(x_ = 2, t_ = 3), n = 5)
setMethod("rdeath", signature("Z_x"), function(object, n) {
  # find the probability of death in each x for a person age x_
  tp_x8q_x <- tp_x8q_x(object)
  
  # run the simulation
  deaths <- rmultinom(n = n, size = 1, prob = tp_x8q_x)
  deaths <- deaths[-nrow(deaths), , drop = FALSE]
  deaths <- as.data.frame(deaths)
  
  ## discount the simulation to present value  
  pv <- deaths
  pv[pv == 1] <- object@benefit
  # if death in derreral period (i.e. x_ to x_ + m_)
  # set present value of benefit to 0
  if (object@m_ > 0) {
    pv[1:object@m_,] <- 0
  }
    
  i <- trim_table(object, slot_ = "i", x_ = object@x_, t_ = object@t_ + object@m_)
  discount <- discount(i)
  pv <- as.data.frame(lapply(pv, function(j) j * discount))
  
  # return simulation output
  x <- trim_table(object, slot_ = "x", x_ = object@x_, t_ = object@t_ + object@m_)
  t <- 1:length(x)
  list(x =  x,
       t = t,
       death_table = deaths,
       death_t = as.data.frame(lapply(deaths, function(j) ifelse(sum(j) > 0, t[j > 0], NA))),
       pv_table = pv,
       pv = unlist(lapply(pv, sum)),
       probs = data.frame(t = c(t, paste0(">", max(t))), prob_death = tp_x8q_x),
       prob_benefit = sum(tp_x8q_x[(object@m_ + 1):(object@m_ + object@t_)]))
})