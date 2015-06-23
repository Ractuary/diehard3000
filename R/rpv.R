#' rpv
#' 
#' present value of \code{rdeath} simulation
#' 
#' @export
setGeneric("rpv", 
           #valueClass = "numeric",
           function(object, n, benefit_type = "life") {
             standardGeneric("rpv")
           }
)



#' rpv
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' @param benefit_type character string of either "life" or "annuity"
#' 
#' @export
#' @examples
#' rpv(object = Insuree(m_ = .5, benefit = c(1,1, 1, 1)), n = 5, benefit_type = "annuity")
#' rpv(object = Insuree(x_ = 2, t_ = 3, benefit = c(1, 1, 1, 1), m_ = 0.3), n = 5)
#' rpv(object = Insuree(x_ = 2.2, t_ = 3.4, benefit = c(1, 1, 1, 1), m_ = 0.3), n = 5)
#' rpv(object = Insuree(x_ = 2.48, t_ = 3.57, benefit = c(1, 1, 1, 1, 1), m_ = 0), n = 5)
setMethod("rpv", signature("Insuree"), function(object, n, benefit_type = "life") {
  
  stopifnot(benefit_type %in% c("life", "annuity"))
  # simulate deaths
  deaths <- rdeath(object, n = n)
  pv <- deaths[["death_table"]]
  
  # convert 1s to 0s if annuity
  if (identical(benefit_type, "annuity")) {
    # set insuree time of death to t_ + m_ if insuree did not die
    tod <- deaths[["death_t"]]
    tod[is.na(tod)] <- object@t_ + object@m_
    
    # change death_table to 1s for years insuree survives
    for (j in seq_along(tod)) {
      pv[1:tod[j], j] <- 1
    }
  }
  
  # if death in defferal period (i.e. x_ to x_ + m_)
  # set present value of benefit to 0
  if (object@m_ > 0) {
    pv[1:ceiling((object@x_ %% 1) + object@m_), ] <- 0
  }
  
  # find undiscounted benefit amounts
  # TODO: make this more simple
  if (object@x_ %% 1 + object@m_ %% 1 > 1) {
    pv[ceiling(object@m_ + 2):nrow(pv), ] <- 
      pv[ceiling(object@m_ + 2):nrow(pv), ] * object@benefit
  } else {
    pv[ceiling(object@m_ + 1):nrow(pv), ] <- 
      pv[ceiling(object@m_ + 1):nrow(pv), ] * object@benefit
  }
  # returns vector of discount factors
  discount <- discount(object, x_ = object@x_, t_ = object@t_, m_ = object@m_)
  pv <- apply(pv, 2, function(j) j * discount)
  list(deaths,
       discount = discount,
       pv = apply(pv, 2, sum))
})