#' life_table
#' 
#' life_table S3 class constructor
#'
#' @param df data.frame containing x and q_x
#' @param x x
#' @param q_x q_x
#'
#' @export
#' @examples
#' my_table <- data.frame("x" = 0:9,
#'                        "q_x" = seq(0.05, 0.14, by = 0.01)
#'                       )
#'            
#' life_table(my_table, x = "x", q_x = "q_x") 
life_table <- function(df, x, q_x) {
  tbl <- data.frame("x" = df[, x], 
                    "q_x" = df[, q_x]
                   )
  class(tbl) <- c("life_table", "data.frame")
  tbl
}
