##------------------------------------------------------------------------------#
## Nombre del Script: Aplicativo Web para gráficos de consumo de Medicamentos 
## Monopolio del Estado (MME) ---------------------------------------------------
##  
## Propósito del Script: mostrar de manera interactiva gráficos de consumo y 
## distribución de medicamentos MME por departamento.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 19-04-2020 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Cargar de módulos
source(file.path('src', '001_cargaPaquetes.R'),  encoding = 'UTF-8')
source(file.path('src', '002_inicializacion.R'), encoding = 'UTF-8')

source(file.path('src', '800_listaMedicamentos.R'), encoding = 'UTF-8')

# Funciones de módulos
source(file.path('src', '100_moduloHeader.R'),  encoding = 'UTF-8')
source(file.path('src', '101_moduloPanel1.R'),  encoding = 'UTF-8')
source(file.path('src', '102_moduloPanelTab1.R'),  encoding = 'UTF-8')
source(file.path('src', '103_moduloPanelTab2.R'),  encoding = 'UTF-8')
source(file.path('src', '104_moduloPanelTab3.R'),  encoding = 'UTF-8')

# Funciones auxiliares
source(file.path('src', '500_funcionCreacionGrafico_1.R'), encoding = 'UTF-8')
source(file.path('src', '501_funcionCreacionCloropletico.R'), encoding = 'UTF-8')
source(file.path('src', '502_funcionCreacionBarras.R'), encoding = 'UTF-8')
source(file.path('src', '503_funcionCreacionMapaInteraccion.R'), encoding = 'UTF-8')
source(file.path('src', '504_funcionCreacionDataTable.R'), encoding = 'UTF-8')

# Definir la interfaz de usuario para la aplicación que grafica

#-------------------------------------------------------------------------------#
# Interfaz Gráfica          -----------------------------------------------------
#-------------------------------------------------------------------------------#

ui <- fluidPage(
  header_UI(),
  titulo_UI(), 
  br(),
  # Barra lateral con una entrada de control deslizante para el número de bins
  sidebarLayout(
    panelPrincipal_UI('selectMed'),
    
    # Mostrar un diagrama de la distribución generada
    mainPanel(
      tabsetPanel(
        panelTab1_UI('selectMed'),
        panelTab2_UI('selectMed'),
        panelTab3_UI('selectMed')
      )
    ))
)

#-------------------------------------------------------------------------------#
# Servidor ----------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Definir la lógica del servidor requerida para hacer gráficos
server <- function(input, output) {
  # Ejecutar variable de selección de medicamento
  selectMed <- callModule(panelPrincipal_server, 'selectMed')
  
  # 1. Barra lateral
  sideBarPanel <-
    callModule(panelPrincipal_module, 'selectMed', seleccion = selectMed)
  
  # 2. Paneles tabulares
  panelTab1 <- callModule(panelTab1_module, 'selectMed', seleccion = selectMed)
  panelTab2 <- callModule(panelTab2_module, 'selectMed', seleccion = selectMed)
  panelTab3 <- callModule(panelTab3_module, 'selectMed', seleccion = selectMed)
}

# Run !!
shinyApp(ui = ui, server = server)
