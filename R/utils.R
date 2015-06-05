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
#' object <- LifeTable()
#' trim_table(object, slot_ = "q_x", x_ = 2, t_ = 3)
trim_table <- function(object,
                       slot_,
                       x_ = object[1], 
                       t_ = object[length(object)],
                       m_ = 0) {
  trim <- slot(object, slot_)
  trim <- trim[object@x >= x_]
  trim <- trim[1:t_]
  if (m_ > 0) {
    trim[-(1:m_)]
  }
  trim
}

#' validate_x_
validate_x_ <- function(object, x_) {
  stopifnot(length(x_) == 1)
  stopifnot(x_ %in% object@x)
}

#' validate_t_
validate_t_ <- function(object, x_, t_) {
  stopifnot(t_ > 0)
  stopifnot(x_ + t_ <= max(object@x))
}

#' validate_m_
validate_m_ <- function(object, x_, t_, m_) {
  stopifnot(m_ >= 0)
  stopifnot(m_ + x_ + t_ <= max(object@x))
}

#' tp_x8q_x
#' 
#' returns the probability of death in each x for a percon age x_ 
tp_x8q_x <- function(object, t_) {
  # isolate all q_x >= T_x@x_ 
  q_x <- trim_table(object, slot_ = "q_x", x_ = object@x_, t_ = t_)

  # prob of surviving to each x
  tp_x <- sapply(seq_along(q_x), function(i) p_x(object = object, x = object@x_, t_ = i))

  # prob of dieing in each year given single age x_
  tp_x8q_x <- list(vector, length(q_x))
  tp_x8q_x[1] <- q_x[1]
  for (j in 2:length(q_x)) {
    tp_x8q_x[j] <- tp_x[j - 1] * q_x[j]
  }

  tp_x8q_x <- unlist(tp_x8q_x)
  c(tp_x8q_x, 1 - sum(tp_x8q_x))
}

#' find interest discount rate
discount <- function(i) {
  x_trend <- 1 + i
  x_discount <- 1 / x_trend
  cumprod(x_discount)
}