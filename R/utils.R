#' trim_table
#' 
#' removes all values that are unnecessary for the
#' specific calculation from the mortaility table column
#' 
#' @param table LifeTable
#' @param table_slot slot to be trimmed
#' @param x_
#' @param t_
#' @param m_
#' 
#' @export
#' @examples 
#' trim_table(table = LifeTable(), table_slot = "q_x", x_ = 2, t_ = 3)
trim_table <- function(table,
                       table_slot,
                       x_ = object[1], 
                       t_ = object[length(object)],
                       m_ = 0) {
  trim <- slot(table, table_slot)
  trim <- trim[table@x >= x_]
  trim <- trim[1:t_]
  if (m_ > 0) {
    trim[-(1:m_)]
  }
  trim
}