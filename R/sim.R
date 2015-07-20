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
#' rdeath(object = Insuree(x_ = 2.4, t_ = 3, m_ = 0.5, benefit = c(1, 1, 1, 1)), n = 5)
#' rdeath(object = Insuree(x_ = 3, m_ = 0.2, t_ = 3, benefit = c(1, 1, 1, 1)), n = 5)
setMethod("rdeath", signature("Insuree"), function(object, n) {
  # find the probability of death in each x for a person age x_
  trim_m <- object[object@x_, object@m_]
  trim_t <- object[object@x_ + object@m_, object@t_]
  lt <- LifeTable(x = c(trim_m@x, trim_t@x),
                  t = c(trim_m@t, trim_t@t),
                  q_x = c(trim_m@q_x, trim_t@q_x))
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