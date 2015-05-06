#' p_x
#' 
#' probability of survival for person at birthday x
#' 
#' @param life_table object of class life_table
#' @param x x
#' @param t t
#' 
#' @export
#' @examples
#' my_table <- data.frame("x" = 0:9,
#'                        "q_x" = seq(0.05, 0.14, by = 0.01)
#'                       )
#'            
#' my_table <- life_table(my_table, x = "x", q_x = "q_x")
#' p_x(my_table, x = 3, t = 5) # probability of x = 3 surviving 5 years
p_x <- function(life_table, x, t = 1) {
  # check x is length 1
  stopifnot(length(x) == 1)
  stopifnot(t > 0)
  
  # remove all x rows less than x argument
  life_table <- life_table[life_table$x >= x, ]
  stopifnot(nrow(life_table) >= t)
  
  # calculate kurtate life expectancy
  prod(1 - life_table$q_x[1:t])
}