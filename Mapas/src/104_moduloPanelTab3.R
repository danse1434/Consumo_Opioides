source(file.path('src', '510_crearTSDF.R'), encoding = 'UTF-8')
source(file.path('src', '511_crearTSModelo.R'), encoding = 'UTF-8')
source(file.path('src', '512_funcionesPercentiles.R'), encoding = 'UTF-8')
source(file.path('src', '513_plotlyPredicciones.R'), encoding = 'UTF-8')

listaEqv <-
  read_csv(
    file.path('data', 'processed', 'lista_equivalencia.csv'),
    locale = locale(encoding = 'ISO-8859-1')
  ) %>%
  column_to_rownames('medicamento1')

ventas <- read_csv(
  file.path('data', 'processed', 'datos_venta_cod.csv'),
  locale = locale(encoding = 'ISO-8859-1')
)

#-------------------------------------------------------------------------------#
#' Interfaz Usuario - Panel tabular N.º 3
#'
#' @param id nombre de espacio
#'
#' @return
#' @export
#'
#' @examples
#' panelTab1_UI('selectMed')
#' 
panelTab3_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Predicciones",
    br(),
    br(),
    # textOutput(ns('panelTab3Text')),
    fluidRow(
      column(8, plotlyOutput(ns('panelTab3Plot'))),
      column(3, offset = 1,
             numericInput(ns('yearsForecast'), 'Nº años a pronosticar', 3, 1, 10, 1))
    ),
    br(),
    dataTableOutput(ns('panelTab3Pronostico'), width = "100%", height = 300)
  )
}


#-------------------------------------------------------------------------------#
#' Módulo - Panel tabular N.º 3
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
#' panelTab3 <- callModule(panelTab3_module, 'selectMed', seleccion = selectMed)
#' 
panelTab3_module <- function(input, output, session, seleccion){
  
  ls1 <- list(
    # tslm  = TSLM(distribucion ~ año),
    # piece = TSLM(distribucion ~ trend(knots = c(2010))),
    ets   = ETS(distribucion),
    arima = ARIMA(distribucion)#,
    # prophet = prophet(distribucion ~ año)
  )
  
  codigoSeleccionado <- reactive({
    seleccion$A() %>% 
      str_to_lower(.) %>%
      {listaEqv[., ]['codigo'][[1]]}
  })
  
  tablaSeleccionada <- reactive({
    ventas %>% 
      extraerMedicTS(., codigoSeleccionado())
  })
  
  modelTS <- reactive({
    tablaSeleccionada() %>% 
      crearModeloTS(ls1, TRUE)
  })
  
  forecTS <- reactive({
    modelTS() %>% 
      forecast(h = input$yearsForecast, 
               times = 10, 
               point_forecast = listaPercentiles('ls', c(0.10, 0.05, 0.01)))
  })
  
  
  # output$panelTab3Text <- renderText({
  #   codigoSeleccionado()
  # })
  
  output$panelTab3Plot <- renderPlotly({
    plotlyPredicciones(tablaSeleccionada(), forecTS(), .x = 'ano', .y = 'distribucion')
  })
  
  output$panelTab3Pronostico <- renderDataTable({
    forecTS() %>% 
      as_tibble(.) %>% 
      select(-medicamento, -distribucion) %>% 
      rename('Modelo'=.model, 'Año'=ano, 'Media'=media) %>% 
      mutate(across(matches('(Media|(LI|LS)\\_)'), 
                    ~formatC(.x, format = 'e', digits = 2)),
             Modelo = str_to_upper(Modelo)) %>% 
      DT::datatable(., options = list(
      pageLength = 10,
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        # info = "Mostrando _START_ a _END_ entradas de _TOTAL_ departamentos",
        # paginate = list('next' = 'Posterior', previous = 'Anterior')
      )
    ), width = '100%', fillContainer = TRUE)
  })
  
}