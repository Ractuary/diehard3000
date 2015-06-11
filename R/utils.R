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



#' tp_x8q_x
#' 
#' returns the probability of death in each x for a percon age x_ 
tp_x8q_x <- function(object) {
  # isolate all q_x >= Z_x@x_ 
  q_x <- trim_table(object, slot_ = "q_x", x_ = object@x_, t_ = object@t_, m_ = object@m_)

  # prob of surviving to each x
  tp_x <- sapply(seq_along(q_x), function(j) p_x(object = object, x = object@x_, t_ = j))

  # prob of dying in each year given single age x_
  tp_x8q_x <- list()
  tp_x8q_x[[1]] <- q_x[1]
  
  if (length(q_x) > 1) {
    for (j in 2:length(q_x)) {
      tp_x8q_x[[j]] <- tp_x[j - 1] * q_x[j]
    }
  }

  out <- unlist(tp_x8q_x)
  c(out, 1 - sum(out))
}

#' find interest discount rate
discount <- function(i) {
  x_trend <- 1 + i
  x_discount <- 1 / x_trend
  cumprod(x_discount)
}