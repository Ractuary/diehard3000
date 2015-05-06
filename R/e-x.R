#' e_x
#' 
#' calculates the kurtate future life expectancy
#' 
#' @param life_table object of class life_table
#' @param x x
#' @param t t
#' 
#' @examples
#' my_table <- data.frame("x" = 0:9,
#'                        "q_x" = seq(0.05, 0.14, by = 0.01)
#'                       )
#'            
#' my_table <- life_table(my_table, x = "x", q_x = "q_x")
#' e_x(my_table, x = 3)
e_x <- function(life_table, x, t = NULL) {
  # remove all x rows less than x argument
  life_table <- life_table[life_table$x >= x, ]
  
  # set t if not specified
  if (is.null(t)) {
    t <- nrow(life_table)
  } else {
    # stop if life_table not large enough; t exceeds rows in table
    stopifnot(nrow(life_table) >= t)
  }
  
  # calculate kurtate life expectancy
  sum(cumprod(1 - life_table$q_x[1:t]))
}