source(file.path('src', '800_listaMedicamentos.R'), encoding = 'UTF-8')

#-------------------------------------------------------------------------------#
#' Interfaz Usuario - Panel lateral principal
#'
#' @param id nombre de espacio
#'
#' @return
#' @export
#'
#' @examples
#' panelPrincipal_UI('selectMed')
#' 
panelPrincipal_UI <- function(id) {
  ns <- NS(id)
  
  sidebarPanel(
    h4(strong(
      "Distribución promedio mensual por FRE - Año 2019"
      )),
    br(),
    p("En este aplicativo se muestran ventas a territorios de medicamentos MME",
      " para 2019, entre los meses de ",
      span("enero", style = "color:green"), " y ", 
      span("agosto.", style = "color:green"), "
        Seleccione el medicamento para obtener su distribución en este periodo:"),
    
    selectInput(inputId = ns("bins"), 
                label   = h5(strong("Medicamento:")), 
                choices = listaMedicamentos, 
                selected = 8),
    br(),
    p("El medicamento seleccionado es: ", style="display:inline"),
    span(textOutput(ns("Departamento_Var")), style = "color:blue"), 
    p(", y este presenta un consumo promedio nacional de", style="display:inline"),
    span(textOutput(ns("sDDD_pais")), style = "color:blue"),
    p(" DDDs por 100.000 habitantes", style="display:inline"),
    br(),
    br(),
    plotlyOutput(ns("scatterPlot")),
    br(),
    downloadButton(outputId = ns("downloadbarplot"), label = "Descargar")
  )
}

# Output: 
# 
# bins
# Departamento_Var
# sDDD_pais
# scatterPlot
# downloadbarplot

#-------------------------------------------------------------------------------#
#' Servidor con selección de medicamentos
#'
#' @param input entrada
#' @param output salida
#' @param session sesión
#'
#' @return
#' @export
#'
#' @examples
#' selectMed <- callModule(panelPrincipal_server, 'selectMed')
#' 
panelPrincipal_server <- function(input, output, session){
  medSel <- reactive({
    input$bins %>% 
      as.numeric(.) %>%
      {unique(MMEDF$Medicamento)[.]} %>% 
      as.character(.)
    })
  
  # Creación de data.frame para mapa
  data1 = reactive({
    MMEDF %>% filter(Medicamento == medSel())
  })
  # Creación de data.sf para mapa
  COL_tot <- reactive({
    COL_map %>% 
      right_join(data1(), by = c('NAME_1' = "Departamento")) %>% 
      st_as_sf(.)
  })
  # Creación de data.sf para etiquetas de cuartil
  COL_lab <- reactive({
    COL_map %>%
      sf::st_centroid() %>%
      right_join(data1(), by = c('NAME_1' = "Departamento"))
  })
  
  return(list(
    A       = medSel,
    data1   = data1,
    COL_tot = COL_tot,
    COL_lab = COL_lab
  ))
}

#-------------------------------------------------------------------------------#
#' Módulo - barra lateral selección medicamento
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
#' sideBarPanel <- 
#' callModule(panelPrincipal_module, 'selectMed', seleccion = selectMed)
#' 
panelPrincipal_module <- function(input, output, session, seleccion) {
  # Departamento escogido
  output$Departamento_Var <- renderText({
    seleccion$A() %>% str_to_title(.)
  })
  
  # DDDs totales
  output$sDDD_pais <- renderText({
    seleccion$data1() %>%
      summarise(mn_s = sum(DDDc),
                pp_s = sum(pp),
                sDDD = mn_s * 1E5 / pp_s) %>%  # DDD por 1000 hab
      pull(sDDD) %>%
      round(., 2)
  })
  
  # Gráfico de barras
  output$scatterPlot <- renderPlotly({
    crearGraficoBarras1(MMEDF, seleccion$A(), 'sDDD', tipo = 'plotly')
  })
  
  # Descargar gráfico de barra 
  output$downloadbarplot <- downloadHandler(
    filename = function() { "graficoBarras.pdf" },
    content = function(file) {
      
      gs1 <- crearGraficoBarras1(MMEDF, seleccion$A(), 'sDDD', TRUE)
      ggsave(file, gs1, 'pdf', width = 8, height = 6)
    },
    contentType = 'pdf'
  )
}