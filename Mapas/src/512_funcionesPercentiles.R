#-------------------------------------------------------------------------------#
#' Crear lista de funciones para utilizar con método *Forecast*
#'
#' @param ls Una lista u objeto.
#' @param probs probabilidades para anexar límites inferiores y superiores 
#' de percentiles.
#'
#' @return
#' @export
#'
#' @examples
#' listaNiveles('ls', c(0.10, 0.05, 0.01))
#' 
listaPercentiles <- function(ls, probs) {
  ls <- list(media = mean)
  
  for (i in seq_along(probs)) {
    p = probs[i]
    
    limitName <- c('LI', 'LS') %>% paste0(., '_P', p * 100)
    
    ls1 <- list(
      LI = function(x) quantile(x, p / 2),
      LS = function(x) quantile(x, 1 - p / 2)
    )
    
    names(ls1) <- limitName
    ls <- append(ls, ls1)
  }
  
  return(ls)
}