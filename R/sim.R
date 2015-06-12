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
#' rdeath(object = Insuree(m_ = 2), n = 5)
#' rdeath(object = Insuree(x_ = 2, t_ = 3), n = 5)
setMethod("rdeath", signature("Insuree"), function(object, n) {
  # find the probability of death in each x for a person age x_
  tp_x8q_x <- tp_x8q_x(object)
  
  # run the simulation
  deaths <- rmultinom(n = n, size = 1, prob = tp_x8q_x)
  deaths <- deaths[-nrow(deaths), , drop = FALSE]
  
  ## discount the simulation to present value  
  #pv <- deaths
  
  # if death in defferal period (i.e. x_ to x_ + m_)
  # set present value of benefit to 0
  #if (object@m_ > 0) {
  #  pv[1:object@m_,] <- 0
  #}
  #pv <- pv * object@benefit
    
  #i <- trim_table(object, slot_ = "i", x_ = object@x_, t_ = object@t_ + object@m_)
  #discount <- discount(i)
  #pv <- as.data.frame(lapply(pv, function(j) j * discount))
  
  # return simulation output
  t <- 1:(object@t_ + object@m_)
  list(death_table = deaths,
       death_t = apply(deaths, 2, function(j) ifelse(sum(j) > 0, t[j > 0], NA)),
       probs = data.frame(t = c(t, paste0(">", max(t))), prob_death = tp_x8q_x),
       prob_benefit = sum(tp_x8q_x[(object@m_ + 1):(object@m_ + object@t_)]))
})