#' rdeath
#' 
#' simulates \code{Insuree} time of death in accordance
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
#' rdeath(object = Insuree(x_ = 2.4, t_ = 3, m_ = 0.5, benefit_t = c(1, 1, 1), benefit_value = c(3, 5, 7)), n = 5)
#' rdeath(object = Insuree(x_ = 3, m_ = 0.2, t_ = 3, benefit_t = c(1, 1, 1), benefit_value = c(2, 4, 5)), n = 5)
setMethod("rdeath", signature("Insuree"), function(object, n) {
  # find the probability of death in each x for a person age x_
  lt <- trim_table(object, x_ = object@x_, t_ = object@t_, m_ = object@m_)
  death_probs <- tp_x8q_x(lt)
  
  # run the simulation
  deaths <- rmultinom(n = n, size = 1, prob = death_probs)
  deaths <- deaths[-nrow(deaths), , drop = FALSE]
  
  t_s <- cumsum(lt@t)
  # return simulation output
  list(Insuree = object,
       death_table = deaths,
       death_t = apply(deaths, 2, function(l) ifelse(sum(l) > 0, t_s[l > 0], NA)),
       probs_death = death_probs,
       t = lt@t,
       x = lt@x)
})