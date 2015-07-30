#' index
#' 
#' finds index positions for all necessary values from the LifeTable
#' given the x_, t_, and m_.  For this specific function, t_ + m_
#' should be provided for the t_ agrument because all information
#' in the m_ period must be preserved.
#' 
#' The function does not adjust the q_x values for partial years.
#' It only subsets all values that are relevant to x_, t_, and m_.
#' See the `[` LifeTable method for partial year q_x values.
#' 
#' @param object LifeTable
#' @param x_
#' @param t_ t_ + m_
#' 
#' @examples 
#' index(LifeTable(), x_ = 2, t_ = 3)
#' index(LifeTable(), x_ = 2.4, t_ = 3)
#' index(LifeTable(), x_ = 2.4, t_ = 0)
index <- function(object, x_, t_ = 1) {
  if (t_ == 0) return(c())
  
  if ((x_ %% 1) + t_ >= 1) {
    index <- which(object@x == floor(x_)):which(object@x == (ceiling(x_ + t_) - 1))
  } else {
    index <- which(object@x == floor(x_[1]))
  }
  index
}

#' "["
#' 
#' LifeTable method for subsetting
#' 
#' The function resturns all `x` and `t` values that are applicable
#' to the supplied `x_` and `t_` for the individual.  The `q_x` slot is
#' adjusted for partial years where applicable based on uniform force of
#' mortality.
#' 
#' @param x object of class LifeTable
#' @param i x_
#' @param j t_
#' 
#' @export
#' @examples 
#' test <- LifeTable()
#' test[2.5, 0]
#' test[2.5, 3.5]
#' test[2, 3]
#' test[2.4, 3.2]
#' test[2.4, 3.1]
#' test[2.2, 0.1]
setMethod("[", c("LifeTable", "numeric", "numeric", "ANY"),
          function(x, i, j, ..., drop=TRUE)
          { 
            stopifnot(identical(length(i), length(j), 1))
            object <- x
            
            # find index of relevant x values
            index <- index(object, x_ = i, t_ = j)
            
            # identify partials
            partial_x_ <- i %% 1
            partial_t_ <- (i + j) %% 1
            # find table
            # should work for partial years
            x <- object@x[index]
            t <- object@t[index]
            q_x <- object@q_x[index]
            if (j == 0) {
              LifeTable(x = vector(mode="numeric", length=0),
                        t = vector(mode="numeric", length=0),
                        q_x = vector(mode="numeric", length=0)
              )
            } else {
            # if i and j represent integer times
              if (partial_x_ == 0 && partial_t_ == 0) {  
                LifeTable(x = x,
                          t = t,
                          q_x = q_x
                          )
              } else {
                # find first t value
                t_1 <- c(min(1 - (i %% 1), j))
                # if LifeTable ends on an integer value
                if (partial_t_ == 0) {
                  LifeTable(x = c(i, x[-1]),
                            t = c(t_1, t[-1]),
                            q_x = c(1 - p_x(object, x_ = i, t_ = t_1), q_x[-1])
                  )
                # if LifeTable ands on a partial year  
                } else {
                  t[length(t)] <- partial_t_
                  q_x[length(q_x)] <- 1 - p_x(object, x_ = floor(i + j), t_ = partial_t_)
                  LifeTable(x = c(i, x[-1]),
                            t = c(t_1, t[-1]),
                            q_x = c(1 - p_x(object, x_ = i, t_ = t_1), q_x[-1])
                  )
                }
              }  
            }  
          }
)


#' trim_table
#'
#' Helper function for creating new LifeTables given the 
#' x_, t_, and m_ values.
#' 
#' @param object LifeTable
#' @param x_ x_
#' @param t_ t_
#' @param m_ m_
#' 
#' @export
#' @examples
#' trim_table(LifeTable(), x_ = 2, t_ = 3, m_ = 1)
trim_table <- function(object, 
                       x_ = object@x[1], 
                       t_ = NULL, 
                       m_ = 0) {
  trim_m_ <- object[x_, m_]
  trim_t_ <- object[x_ + m_, t_]
  LifeTable(x = c(trim_m_@x, trim_t_@x),
            t = c(trim_m_@t, trim_t_@t),
            q_x = c(trim_m_@q_x, trim_t_@q_x)
  )
}

#' find interest discount rate
#' 
#' @param object object of class ActuarialTable
#' @param x_ exact age at current time
#' @param t_ term time
#' @param m_ deferral time
#' @param payment_time time in x_ to x_ + t interval when the
#' payment is to be made.  Should be supplied as a number between
#' 0 and 1.  0 for the beginning of the interval.  1 for the end of the 
#' interval, and values between 0 and 1 for times between the beginning
#' and the end of the interval.
#' 
#' @export
#' @examples
#' discount(object = ActuarialTable(), x_ = 2.5, t_ = 4, m_ = 0.5)
#' discount(object = ActuarialTable(), x_ = 2.48, t_ = 4.57, m_ = 0)
discount <- function(object, 
                     x_, 
                     t_, 
                     m_,
                     payment_time = 0.5,
                     death_time = NA) {
  lt <- trim_table(object, x_ = x_, t_ = t_, m_ = m_)
  i <- c(object@i[index(object, x_ = x_, t_ = m_)], object@i[index(object, x_ = x_ + m_, t_ = t_)])
  x_trend <- 1 + i
  t <- lt@t
  t_s <- cumsum(lt@t)
  # for random uniform death simulation in year of death
  if (!is.na(death_time)) {
    t_s <- t_s[t_s < death_time]
    t <- t[seq_along(t_s)]
    t <- c(t, death_time - t_s[length(t_s)])
    t_s <- c(t_s, death_time)
    x_trend <- x_trend[seq_along(t)]
  }
  # for payment time
  discount_periods <- t_s - (1 - payment_time) * t
  (1 / x_trend)^(discount_periods)
}