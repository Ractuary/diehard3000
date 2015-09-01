#' index
#' 
#' Returns the index numbers for all values in an ascending
#' vector between the floor of the x_ argument and the ceiling
#' of the x + m_t_ arguments.  This is a utility function often
#' used for extracting the q_x values that are applicable to
#' a subset of a LifeTable object.
#' 
#' @param x vector of ascending numerics
#' @param x_ the index of the first x value to be returned.
#' @param m_t_ the index of the last x value to be returned.  This is oftern m_ + t_, m_
#' or t_ when the x_ argument is set to x_ + t_.
#' 
#' @keywords internal
#' @export
#' @examples 
#' my_x <- 0:9
#' index(my_x, x_ = 2, m_t_ = 3)
#' index(my_x, x_ = 2.4, m_t_ = 3)
#' index(my_x, x_ = 2.4, m_t_ = 0)
index <- function(x, x_, m_t_ = 1) {
  stopifnot(m_t_ >= 0)
  if (m_t_ == 0) return(c())
  upper_cutoff <- x_ + m_t_
  
  if (upper_cutoff %in% x) {
    findInterval(x_, x):(findInterval(upper_cutoff, x) - 1)
  } else {
    findInterval(x_, x):findInterval(upper_cutoff, x)
  }
}

#' index_x
#' 
#' Returns the x values for an actuarial table
#' 
#' @param x vector of ascending numerics
#' @param x_ the index of the first x value to be returned.
#' @param m_t_ the index of the last x value to be returned.  This is oftern m_ + t_, m_
#' or t_ when the x_ argument is set to x_ + t_.
#' 
#' @keywords internal
#' @export
#' @examples 
#' my_x <- 0:9
#' index_x(my_x, x_ = 2, m_t_ = 3)
#' index_x(my_x, x_ = 2.4, m_t_ = 3)
#' index_x(my_x, x_ = 2.4, m_t_ = 0)
index_x <- function(x, x_, m_t_ = 1) {
  .index <- index(x = x, x_ = x_, m_t_ = m_t_)
  # return index for x values
  c(.index, .index[length(.index)] + 1)
}

#' new_x
#' 
#' create new x values by specifying new age x_ and
#' interval m_t_
#' 
#' @param x
#' @param x_
#' @param m_t_
#' 
#' @export
#' 
#' @examples 
#' new_x(x = 0:5, x_ = 2.3, m_t_ = 1.8)
new_x <- function(x, x_, m_t_) {
  .x <- x[index_x(x = x, x_ = x_, m_t_ = m_t_)]
  .x[1] <- x_
  .x[length(.x)] <- x_ + m_t_
  .x
}

#' "["
#' 
#' LifeTable method for subsetting
#' 
#' The function returns all `x` values that are applicable
#' to the supplied `x_` and `t_`.  The `q_x` slot is
#' adjusted for partial years where applicable based on uniform force of
#' mortality.
#' 
#' @param x object of class LifeTable
#' @param i x_ the first x value to be returned.
#' @param j the x value at which to end the subsetted \code{LifeTable}.
#' This value is usually either m_ or t_.
#' @param ... not used
#' @param drop not used
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
  function(x, i, j, ..., drop=TRUE) { 
    stopifnot(identical(length(i), length(j), 1))
    object <- x
            
    # find proper x values
    .x <- new_x(object@x, x_ = i, m_t_ = j)
    # find q_x values
    # note: the full year q_x value will be returned for
    # partial years at this point
    q_x <- object@q_x[index(object@x, x_ = i, m_t_ = j)]
    
    # recalculate q_x for partial years    
    .q_x <- c()
    for (k in seq_along(q_x)) {
      .q_x[k] <- 1 - p_x(object, x_ = .x[k], t_ = .x[k + 1] - .x[k])
    }
    
    # return new LifeTable       
    LifeTable(x = .x,
              q_x = c(.q_x, NA_real_)
    )
    
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
#' trim_table(object, x_ = 2.47, t_ = 4.57, m_ = 0)
trim_table <- function(object, 
                       x_ = object@x[1], 
                       t_ = NULL, 
                       m_ = 0) {
  
  trim_t_ <- object[x_ + m_, t_]
  if (m_ > 0) {
    trim_m_ <- object[x_, m_]
    len <- length(trim_m_@x)
    trim_m_@x <- trim_m_@x[-len]
    trim_m_@q_x <- trim_m_@q_x[-len]
    LifeTable(x = c(trim_m_@x, trim_t_@x),
              q_x = c(trim_m_@q_x, trim_t_@q_x)
    )
  } else {
    trim_t_
  }
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
#' @param death_time the time (from x_) of death
#' 
#' @export
#' @examples
#' discount(object = ActuarialTable(), x_ = 2.5, t_ = 4, m_ = 0.5)
#' discount(object = ActuarialTable(), x_ = 2.48, t_ = 4.57, m_ = 0)
discount <- function(object, 
                     x_, 
                     t_, 
                     m_,
                     death_time = NA) {
  lt <- trim_table(object, x_ = x_, t_ = t_, m_ = m_)
  i <- object@i[index(object@x, x_ = x_ + m_, m_t_ = t_)]
  x_trend <- 1 + i
  t <- diff(lt@x)
  t_s <- cumsum(t)
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

#' discount_death
#'
#' function to discount single death benefit
#' 
#' @param object object of class Insuree
#' @param death_time the time (from x_) of death
#' 
#' @export
#' @examples
#' discount_death(Insuree(), death_time = 1.01)
discount_death <- function(object, death_time = NA) {
  out <- discount(object, 
                  x_ = object@x_, 
                  t_ = object@t_, 
                  m_ = object@m_,
                  death_time = death_time)
  out[length(out)]
}