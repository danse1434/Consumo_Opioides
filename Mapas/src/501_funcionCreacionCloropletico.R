source(file.path('src', '500_funcionCreacionGrafico_1.R'))

#' Title
#'
#' @param data 
#' @param etiquetas 
#' @param tipo 
#' @param titulo 
#' @param medicamento 
#'
#' @return
#' @export
#'
#' @examples
#' 
crearMapaCloropletColombia <- function(data, etiquetas, tipo, titulo = FALSE, medicamento = FALSE) {
  
  # Termina si no es cáracter
  stopifnot(is.character(tipo))
  # Conversión en quo
  tipo_q <- ensym(tipo)
  
  g1 <- data %>% 
    filter(!str_detect(NAME_1, 'San Andrés')) %>% 
    ggplot(.) +
    geom_sf(aes(fill = !!tipo_q)) +
    geom_sf_text(data = etiquetas, aes(label = Q),
                 colour = 'white', 
                 size = 2, check_overlap = TRUE) +
    coord_sf(crs = st_crs(32618)) +
    scale_fill_binned(guide = guide_bins(show.limits = TRUE,
                                         axis = FALSE))
  
  # Se invoca cuando el gráfico tiene un título
  if (titulo) {
    
    # Título
    graficoTitulo = glue::glue("DISTRIBUCIÓN NACIONAL MENSUAL PROMEDIO {tipo}")
    
    # Subtítulo
    if (tipo == 'sDDD') {
      subTituloIndicador = "sDDD: DDDs por cada 100,000 habitantes"
    } else if (tipo == 'DDDc') {
      subTituloIndicador = "DDDs totales por medicamento"
    } else {
      subTituloIndicador = ""
    }
    
    graficoSubTitulo = 
      glue::glue("{str_to_upper(medicamento)}
                       {subTituloIndicador}")
    
    # Nota
    graficoNota = glue::glue(
      "Nota: los numeros (blancos) en cada departamento",
      " representan el cuartil de la distribución."
    )
    
    g1 <- g1 +
      labs(title = graficoTitulo,
           subtitle = graficoSubTitulo,
           caption = graficoNota)
  }
  
  # Insertos gráficos (San Andrés y Providencia)
  g1a <- mapf_subplot(data, etiquetas,
                      c(-81.75, -81.68), c(12.47, 12.62), !!tipo_q)
  
  g1b <- mapf_subplot(data, etiquetas, 
                      c(-81.40, -81.34), c(13.31, 13.40), !!tipo_q)
  
  # Adición de gráficos
  g2 <- g1 + 
    annotation_custom(ggplotGrob(g1a), -3.0E5, -1.0E5, 10E5, 14E5) +
    annotation_custom(ggplotGrob(g1b), -1.0E5, +1.0E5, 12E5, 14E5)
  
  return(g2)
}