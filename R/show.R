#' show
#' 
#' show LifeTable method
#' 
#' @export
#' @examples 
#' show(LifeTable()) 
setMethod("show", "LifeTable",
  function(object) {
    data.frame(x = object@x, 
               t = object@t, 
               q_x = object@q_x)
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
            data.frame(x = object@x, 
                       t = object@t, 
                       q_x = object@q_x,
                       i = object@i)
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
            trim_m_ <- object[object@x_, object@m_]
            trim_t_ <- object[object@x_ + object@m_, object@t_]
            lt <- data.frame(x = c(trim_m_@x, trim_t_@x),
                             t = c(trim_m_@t, trim_t_@t),
                             q_x = c(trim_m_@q_x, trim_t_@q_x)
            )
            
            list(x_ = object@x_,
                 t_ = object@t_,
                 m_ = object@m_,
                 table = data.frame(lt,
                                    #discount = discount(object, 
                                    #                    x_ = object@x_, )
                                    benefit = c(rep(NA, ceiling(object@m_)), 
                                                object@benefit))
                 )
            
          }
)