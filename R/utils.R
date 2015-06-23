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
#' index(LifeTable(), x_ = 2.4, t_ = 3)
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
#' Helper function for subsetting LifeTables
#' 
#' @param x object of class ActuarialTable
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
                t_1 <- c(min(1 - (i %% 1), j))
                if (partial_t_ == 0) {
                  LifeTable(x = c(i, x[-1]),
                            t = c(t_1, t[-1]),
                            q_x = c(1 - p_x(object, x_ = i, t_ = t_1), q_x[-1])
                  )
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
#' tp_x8q_x(Insuree(x_ = 2.4, t_ = 3, benefit = c(1, 1, 1, 1), m_ = 0.5))
#' tp_x8q_x(Insuree(x_ = 3, m_ = 0.2, t_ = 3, benefit = c(1, 1, 1, 1)))
tp_x8q_x <- function(object) {
  # isolate all q_x >= x_ 
  trim_m_ <- object[object@x_, object@m_]
  trim_t_ <- object[object@x_ + object@m_, object@t_]
  lt <- LifeTable(x = c(trim_m_@x, trim_t_@x),
                  t = c(trim_m_@t, trim_t_@t),
                  q_x = c(trim_m_@q_x, trim_t_@q_x)
                  )
  
  # prob of surviving to each x
  tp_x <- cumprod(1 - lt@q_x)

  tp_x8q_x <- lt@q_x[1]
  
  if (length(lt@q_x) > 1) {
    for (j in 2:length(lt@q_x)) {
      tp_x8q_x[j] <- tp_x[j - 1] * lt@q_x[j]
    }
  }

  list(x = lt@x,
       t = lt@t,
       probs = c(tp_x8q_x, 1 - sum(tp_x8q_x))
  )
}

#' find interest discount rate
#' 
#' @param object object of class ActuarialTable
#' @param x_ age x at current time
#' @param t_ interval over which to apply discount
#' @param payment_time time in x_ to x_ + t interval when the
#' payment is to be made.  Should be supplied as a number between
#' 0 and 1.  0 for the beginning of the interval.  1 for the end of the 
#' interval, and values between 0 and 1 for times between the beginning
#' and the end of the interval.
#' 
#' @export
#' @examples
#' 
#' discount(object = ActuarialTable(), x_ = 2.5, t_ = 4, m_ = 0.5)
discount <- function(object, 
                     x_, 
                     t_, 
                     m_,
                     payment_time = 0.5) {
  trim_m_ <- object[x_, m_]
  trim_t_ <- object[x_ + m_, m_ + t_]
  lt <- LifeTable(x = c(trim_m_@x, trim_t_@x),
                  t = c(trim_m_@t, trim_t_@t),
                  q_x = c(trim_m_@q_x, trim_t_@q_x)
  )
  i <- c(object@i[index(object, x_ = x_, t_ = m_)], object@i[index(object, x_ = x_ + m_, t_ = t_)])
  x_trend <- 1 + i
  t_s <- cumsum(lt@t)
  # for payment time
  t_s <- t_s - (1-payment_time) * lt@t
  (1 / x_trend)^(t_s)
}