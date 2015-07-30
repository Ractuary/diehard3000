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
  # this simulation outputs the time of death for each simulation
  # as a column in the matrix
  deaths <- rmultinom(n = n, size = 1, prob = death_probs)
  deaths <- deaths[-nrow(deaths), , drop = FALSE]
  
  # now that we have the period of death we will use a random draw from the
  # uniform distribution to simulate the time of death during the period (e.g. year)
  death_interval <- apply(deaths, 2, function(l) ifelse(sum(l) > 0, object@t[l > 0], NA))
  death_time <- lapply(death_interval, function(y) runif(n = 1, min = 0, max = y))
  death_time <- unlist(death_time)
  death_time[is.nan(death_time)] <- NA
  
  # we now have the time of death within the death interval
  # now we will find the total time leading up to that interval and add
  # that to the time lived during the interval (which we just calculated above)
  t_s <- cumsum(lt@t)
  full_periods_lived <- apply(deaths, 2, function(l) {
    ifelse(sum(l) > 0, t_s[which(l > 0) - 1], NA)
  })
  death_time <- full_periods_lived + death_time
  
  # return simulation output
  list(Insuree = object,
       death_table = deaths,
       death_t = death_time,
       probs_death = death_probs,
       t = lt@t,
       x = lt@x)
})