#-------------------------------------------------------------------------------#
#' Interfaz Usuario - Panel tabular N.º 2 
#'
#' @param id nombre de espacio
#'
#' @return
#' @export
#'
#' @examples
#' panelTab2_UI('selectMed')
#' 
panelTab2_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Mapa interactivo", leafletOutput(ns("mapa_iter")))
}

#-------------------------------------------------------------------------------#
#' Módulo - Panel tabular N.º 2
#'
#' @param input entradas
#' @param output salidas
#' @param session sesiones
#' @param seleccion módulo con selección de medicamento
#'
#' @return
#' @export
#'
#' @examples
#' panelTab1 <- callModule(panelTab1_module, 'selectMed', seleccion = selectMed)
#' 
panelTab2_module <- function(input, output, session, seleccion) {
  
  # Mapa iterativo en "tmap"
  output$mapa_iter <- renderLeaflet({
    
    seleccion$COL_tot() %>% 
      crearMapaInteraction(.) %>% 
      tmap_leaflet(.)
  })
}