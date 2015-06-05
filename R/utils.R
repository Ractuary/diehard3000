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
  stopifnot(x_ + t_ < max(object@x))
}

#' validate_m_
validate_m_ <- function(object, x_, t_, m_) {
  stopifnot(m_ >= 0)
  stopifnot(m_ + x_ + t_ < max(object@x))
}