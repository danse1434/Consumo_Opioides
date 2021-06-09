#-------------------------------------------------------------------------------#
#' Crear modelo de serie de tiempo
#'
#' @param data Un objeto tsibble.
#' @param models Una lista con modelos propuestos como expresiones.
#'
#' @return Un objeto de tipo **mdl_df**
#' @export
#'
#' @examples
#' crearModeloTS(aus_retail, list(ets = ETS(
#' log(Turnover) ~ error("A") + trend("A") + season("A")
#' )))
#'
crearModeloTS <- function(data, models, combination = FALSE) {
  stopifnot(is.list(models))
  
  mod <- fabletools::model(data, !!!models)
  
  if (combination) {
    n_models <- length(names(models))
    p_models <- rlang::parse_expr(paste0(names(models), collapse = ' + '))
    p_models <- rlang::enexpr(p_models)
    
    mod <- mod %>%
      mutate(Combinación = (!!p_models) / n_models)
  }
  
  return(mod)
}
