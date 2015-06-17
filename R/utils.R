#' trim_table
#' 
#' removes all values that are unnecessary for the
#' specific calculation from the mortaility table column
#' 
#' @param object LifeTable
#' @param slot_ slot to be trimmed
#' @param x_
#' @param t_
#' @param m_
#' 
#' @export
#' @examples 
#' trim_table(LifeTable(), slot_ = "q_x", x_ = 2, t_ = 3, m_ = 2)
trim_table <- function(object,
                       slot_,
                       x_ = object@x[1], 
                       t_ = 1,
                       m_ = 0) {
  trim <- slot(object, slot_)
  trim <- trim[object@x >= floor(x_)]
  trim <- trim[1:ceiling((x_ %% 1) + t_ + m_)]
  if (m_ > 0) {
    trim <- trim[-(1:floor(m_))]
  }
  trim
}



#' tp_x8q_x
#' 
#' returns the probability of death in each x for a percon age x_ 
#' 
#' @param object Insuree object
#' 
#' @export
#' @examples
#' tp_x8q_x(Insuree(x_ = 2, t_ = 3))
#' tp_x8q_x(Insuree(x_ = 2.4, t_ = 3))
tp_x8q_x <- function(object) {
  # isolate all q_x >= x_ 
  q_x <- trim_table(object, slot_ = "q_x", x_ = object@x_, t_ = object@t_ + object@m_)
  
  # isolate t_ values to use to find tp_x values
  # should work even for x_, t_, and m_ are non integer numeric values
  t_s <- 1 - ((object@m_ + object@x_) %% 1)
  t_s <- seq(t_s, t_s + object@t_ + object@m_, by = 1)
  
  # if t_ is non integer
  if ((object@t_ %% 1) != 0) {
    t_s <- c(t_s, t_s[length(t_s) + (t_ %% 1)])
  }
  
  q_x[1] <- 1 - p_x(object, x_ = object@x_, t_ = t_s[1])
  if ((t_s[length(t_s)] %% 1) != 0) {
    q_x[length(q_x)] <- 1 - p_x(object, 
                                x_ = floor(t_s[length(t_s)]), 
                                t_ = t_s[length(t_s)] %% 1)
  }
  # prob of surviving to each x
  tp_x <- lapply(t_s, function(j) p_x(object = object, x_ = object@x_, t_ = j))

  # prob of dying in each year given single age x_
  tp_x8q_x <- list()
  tp_x8q_x[[1]] <- q_x[1]
  
  if (length(q_x) > 1) {
    for (j in 2:length(q_x)) {
      tp_x8q_x[[j]] <- tp_x[[j - 1]] * q_x[j]
    }
  }

  out <- unlist(tp_x8q_x)
  list(t = c(t_s, NA),
      probs = c(out, 1 - sum(out))
  )
}

#' find interest discount rate
discount <- function(i) {
  x_trend <- 1 + i
  x_discount <- 1 / x_trend
  cumprod(x_discount)
}