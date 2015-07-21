#' show
#' 
#' show LifeTable method
#' 
#' @export
#' @examples 
#' show(LifeTable()) 
setMethod("show", "LifeTable",
  function(object) {
    print(
      data.frame(
        x = object@x, 
        t = object@t, 
        q_x = object@q_x
      )
    )
  }
)

#' show ActuarialTable method
#' 
#' show ActuarialTable method
#' 
#' @export
#' @examples 
#' show(ActuarialTable()) 
setMethod("show", "ActuarialTable",
  function(object) {
    print(
      data.frame(
        x = object@x, 
        t = object@t, 
        q_x = object@q_x,
        i = object@i
      )
    )
  }
)

#' show Insuree method
#' 
#' show Insuree method
#' 
#' @export
#' @examples 
#' show(Insuree())
#' show(Insuree(x_ = 2.5, t_ = 3, benefit = c(1, 1, 4, 1))) 
setMethod("show", "Insuree",
  function(object) {
    lt <- trim_table(object, x_ = object@x_, t_ = object@t_, m_ = object@m_)        
    print(
      list(x_ = object@x_,
           t_ = object@t_,
           m_ = object@m_,
           table_ = data.frame(
                      show(lt),
                      discount = discount(
                                   object, 
                                   x_ = object@x_, 
                                   t_ = object@t_,
                                   m_ = object@m_
                                 ),
                      benefit = c(rep(NA, ceiling(object@m_)), 
                                  object@benefit)
                    )
      )
    )
  }
)