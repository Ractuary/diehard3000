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
#' 
#' 
#' @param object object of class Insuree
#' @param n number of observations
#' @param benefit_type character string of either "life" or "annuity"
#' 
#' @export
#' @examples
#' rpv(object = Insuree(m_ = 0), n = 5, benefit_type = "annuity")
#' rpv(object = Insuree(x_ = 2.5, t_ = 3), n = 5)
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
    pv[1:object@m_, ] <- 0
    
  }
  
  pv[(object@m_ + 1):nrow(pv), ] <- pv[(object@m_ + 1):nrow(pv), ] * object@benefit
  
  i <- trim_table(object, slot_ = "i", x_ = object@x_, t_ = object@t_ + object@m_)
  discount <- discount(i)
  pv <- apply(pv, 2, function(j) j * discount)
  list(deaths,
       pv = apply(pv, 2, sum))
})