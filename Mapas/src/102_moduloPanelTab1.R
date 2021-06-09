#-------------------------------------------------------------------------------#
#' Interfaz Usuario - Panel tabular N.º 1
#'
#' @param id nombre de espacio
#'
#' @return
#' @export
#'
#' @examples
#' panelTab1_UI('selectMed')
#' 
panelTab1_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Mapa estático",
    plotOutput(ns("distPlot")),
    downloadButton(outputId = ns("downloadPlot1"),  label = "Descargar Mapa con sDDD"),
    downloadButton(outputId = ns("downloadPlot2"), label = "Descargar Mapa con DDDc"),
    br(),
    br(),
    DT::dataTableOutput(ns("tablaResumen"), height = 400)
  )
}

#-------------------------------------------------------------------------------#
#' Módulo - Panel tabular N.º 1
#'
#' @param input entrada
#' @param output salida
#' @param session sesión
#' @param seleccion módulo con selección de medicamento
#'
#' @return
#' @export
#'
#' @examples
#' panelTab1 <- callModule(panelTab1_module, 'selectMed', seleccion = selectMed)
#' 
panelTab1_module <- function(input, output, session, seleccion){
  # Unión de datos
  output$distPlot <- renderPlot({
    crearMapaCloropletColombia(seleccion$COL_tot(), seleccion$COL_lab(), 'sDDD')
  })
  
  # Tabla descriptiva 
  output$tablaResumen <- DT::renderDataTable({  
    seleccion$data1() %>% crearDataTable()
  })
  
  # Descarga de mapa con sDDD
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      "mapaConsumo1.pdf"
    },
    content = function(file) {
      gMapa <- crearMapaCloropletColombia(seleccion$COL_tot(),
                                          seleccion$COL_lab(),
                                          'sDDD',
                                          TRUE,
                                          seleccion$A())
      
      ggsave(file, gMapa, 'pdf', width = 8, height = 9)
    },
    contentType = 'pdf'
  )
  
  # Descarga de mapa con DDD magnitud absoluta
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      "mapaConsumo2.pdf"
    },
    content = function(file) {
      gMapa <- crearMapaCloropletColombia(seleccion$COL_tot(),
                                          seleccion$COL_lab(),
                                          'DDDc',
                                          TRUE,
                                          seleccion$A())
      
      ggsave(file, gMapa, 'pdf', width = 8, height = 9)
    },
    contentType = 'pdf'
  )
}