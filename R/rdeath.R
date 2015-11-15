#' rdeath
#' 
#' simulates \code{Life} time of death in accordance
#' with the multinomial distribution provided by the \code{LifeTable} object.
#' 
#' @param object object of class \code{Life}
#' @param n number of observations
#' 
#' @export
setGeneric("rdeath", 
           #valueClass = "numeric",
           function(object, n) {
             standardGeneric("rdeath")
           }
)



#' rdeath
#' 
#' simulated time of death for an object of class \code{Life}.  The
#' simulation is in accordance with the multinomial distribution described
#' by the LifeTable object.  Uniform time of death is assumed between x values
#' indicated by the LifeTable object.
#' 
#' @param object object of class Life
#' @param n number of observations
#' 
#' @export
#' @examples
#' rdeath(object = Life(m_ = 2), n = 5)
#' rdeath(object = Life(), n = 1000)
#' rdeath(object = Life(x_ = 2.4, t_ = 3, m_ = 0.5, benefit = list(BenefitDeath())), n = 5)
#' max(rdeath(Life(x_ = 2.4, 
#'                   t_ = 3,
#'                   m_ = 0), n = 1000)$death_t, na.rm = TRUE)
setMethod("rdeath", signature("Life"), function(object, n) {
  # find the probability of death in each x for a person age x_
  lt <- trim_table(object@life_table, x_ = object@x_, t_ = object@t_, m_ = object@m_)
  death_probs <- tp_x8q_x(lt)
  
  ## Simulate the time of death
  # The `rmultinom` simulation outputs a matrix.
  # Each column represents a simulated individual where a 0 
  # indicates the indivual survived the corresponding lt@t 
  # period and a 1 indicates the individual died during the corresponding
  # lt@t period. If the 1 is in the final element of the column than
  # the individual survived the entire insured term of the policy.
  deaths <- rmultinom(n = n, size = 1, prob = death_probs)
  # remove final row (final row of 1 indicates survival)
  deaths <- deaths[-nrow(deaths), , drop = FALSE]
  
  ## now that we have the t of death we will use a random draw from the
  ## uniform distribution to simulate the time of death during the t.  This
  ## allows us to simulate the exact time of death.
  # Determine length of t in which individual died
  t <- diff(lt@x)
  death_t_length <- apply(deaths, 2, function(l) ifelse(sum(l) > 0, t[l > 0], NA))
  # Run random uniform simulation over t of death
  death_t_time <- lapply(death_t_length, function(y) ifelse(is.na(y), NA, runif(n = 1, min = 0, max = y)))
  death_t_time <- unlist(death_t_time)
  
  # we now have the time of death within the death interval
  # now we will find the total time leading up to that interval and add
  # that to the time lived during the interval (which we just calculated above)
  t_s <- lt@x - min(lt@x)
  full_periods_lived <- apply(deaths, 2, function(l) {
    ifelse(sum(l) > 0, t_s[which(l > 0)], NA)
  })
  death_time <- full_periods_lived + death_t_time
  
  # return simulation output
  list(Insuree = object,
       death_t = death_time,
       probs_death = death_probs)
})