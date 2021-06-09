#' Crear un mapa interactivo de tipo Leaflet
#'
#' @param data dataFrame
#' @param consumo1 columna con indicador de consumo 1
#' @param consumo2 columna con indicador de consumo 2
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
crearMapaInteraction <- function(data, consumo1 = 'sDDD', consumo2 = 'DDDc') {
  
  # Verificar tipo
  stopifnot(is.character(consumo1))
  stopifnot(is.character(consumo2))
  
  tm <- data %>% 
    tm_shape(., name = 'NAME_1') + 
    tm_fill(consumo1, legend.show = TRUE, 
            id = "NAME_1",
            title = "DDDs por 100.000 habitantes",
            popup.vars = c("DDDs/100.000 hab" = consumo1, 
                           "DDD total"        = consumo2,
                           "Ventas"           = "mn",
                           "Población"        = "pp"),
            palette = sf.colors(5)) +
    tm_borders(col = "black") +
    tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
  
  return(tm)
}