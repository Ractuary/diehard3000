#' index
#' 
#' removes all values that are unnecessary for the
#' specific calculation from the mortaility table column
#' 
#' @param object LifeTable
#' @param slot_ slot to be trimmed
#' @param x_
#' @param t_ t_ + m_
#' 
#' @export
#' @examples 
#' index(LifeTable(), x_ = 2, t_ = 3)
index <- function(object, x_, t_ = 1) {
  if ((x_ %% 1) + t_ >= 1) {
    index <- which(object@x == floor(x_)):which(object@x == (ceiling(x_ + t_) - 1))
  } else {
    index <- which(object@x == floor(x_[1]))
  }
  index
}

#' "["
#' 
#' Helper function for subsetting ActuarialTables
#' I want to use this to replace the trim_table() function
#' 
#' Need to expand so that it works for LifeTable before removing trim_table
#' Also need to add 
#' 
#' @param x object of class ActuarialTable
#' @param i x_
#' @param j t_
#' 
#' @examples 
#' test <- LifeTable()
#' test[2.5, 3]
setMethod("[", c("LifeTable", "numeric", "numeric", "ANY"),
          function(x, i, j, ..., drop=TRUE)
          { 
            stopifnot(identical(length(i), length(j), 1))
            object <- x
            
            # find index of relevant x values
            index <- index(object, x_ = i, t_ = j)
            
            # identify partials
            partial_x <- i %% 1
            partial_t <- (i + j) %% 1
            # find table
            # should work for partial years
            x <- object@x[index]
            q_x <- object@q_x[index]
          
            # if i and j represent integer times
            if (partial_x == 0 && partial_t == 0) {  
              LifeTable(x = x,
                        q_x = q_x
              )
            } else {
              if (partial_t == 0) {
                LifeTable(x = c(i, x[-1]),
                          q_x = c(1 - p_x(object, x_ = i, t_ = 1 - i%%1), q_x[-1])
                )
              } else {
                LifeTable(x = c(i, x[-1]),
                          q_x = c(1 - p_x(object, x_ = i, t_ = 1 - i%%1), q_x[-c(1, length(index))], 
                                  1- p_x(object, x_ = floor(i + j), t_ = partial_t))
                )
              }
            }  
          })


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
  trim <- object[object@x_, object@t_ + object@m_]
  q_x <- trim@q_x
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
  list(x = trim@x,
       t = c(t_s, NA),
       probs = c(out, 1 - sum(out))
  )
}

#' find interest discount rate
#' 
#' @param i vector for interest rates
#' @param duration vector of length == length(i) for time of each period
#' 
#' @examples
#' i <- c(0.04, 0.05, 0.03)
#' duration <- c(0.5, 1, 0.5)
#' 
#' discount(i = i, duration = duration)
discount <- function(i, duration = NULL) {
  x_trend <- 1 + i
  x_discount <- 1 / x_trend
  if (!is.null(duration)) {
    x_discount <- (x_discount)^(duration)
  }
  cumprod(x_discount)
}