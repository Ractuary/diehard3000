#' q_x
#' 
#' probability of death for person at birthday x
#' 
#' @param life_table object of class life_table
#' @param x x
#' @param t t
#' @param m m
#' 
#' @export
#' @examples
#' my_table <- data.frame("x" = 0:9,
#'                        "q_x" = seq(0.05, 0.14, by = 0.01)
#'                       )
#'            
#' my_table <- life_table(my_table, x = "x", q_x = "q_x")
#' q_x(my_table, x = 3, t = 5) # probability of x = 3 surviving 5 years
#' q_x(my_table, x = 2, t = 4, m = 1) # probability of x = 2 surviving 1 year and 
#' # dieing in the following 4 years
q_x <- function(life_table, x, t = 1, m = 0) {
  # chack arguments
  stopifnot(m >= 0)
  # find prob of survival for m years
  if (m > 0) {
    p <- p_x(life_table = life_table,
             x = x,
             t = m
             )
    x <- x + m
  } else {
    p <- 1
  }
  
  q <- 1 - p_x(life_table = life_table,
               x = x,
               t = t
               )
  p * q
}