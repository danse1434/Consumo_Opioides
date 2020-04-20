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
# Cargar Paquetes
require(leaflet)
require(DT)
# Definir la interfaz de usuario para la aplicación que grafica

#-------------------------------------------------------------------------------#
# Interfaz Gráfica          -----------------------------------------------------
#-------------------------------------------------------------------------------#

ui <- fluidPage(
   
  tags$p(
    tags$style("#sDDD_pais{display:inline}"),
    tags$style("#Departamento_Var{display:inline}")
    ),
    
  # Titulo de Aplicación
  titlePanel("Programa de Farmacovigilancia - Fondo Nacional de Estupefacientes"),
  br(),
  # Barra lateral con una entrada de control deslizante para el número de bins
  sidebarLayout(
    sidebarPanel(
      h4(strong("Distribución Promedio Mensual por FRE - Año 2019")),
      br(),
      p("En este aplicativo se muestran ventas a territorios de medicamentos MME",
        " para 2019, entre los meses de ",
        span("enero", style = "color:green"), " y ", 
        span("agosto.", style = "color:green"), "
        Seleccione el medicamento para obtener su distribución en este periodo:"),
      
      selectInput("bins", 
                  h5(strong("Medicamento:")), 
                  choices = list("Fenobarbital 0,4% Solución Oral"                           = 1, 
                                 "Fenobarbital 10 mg Tabletas"                               = 2, 
                                 "Fenobarbital 100 mg Tabletas"                              = 3, 
                                 "Fenobarbital 50 mg Tabletas"                               = 4, 
                                 "Fenobarbital Sódico 200 mg/mL Solución Inyectable"         = 5, 
                                 "Fenobarbital Sódico 40 mg/mL Solución Inyectable"          = 6, 
                                 "Hidromorfona HCl 2 mg/mL Solución Inyectable"              = 7, 
                                 "Hidromorfona HCl 2.5 mg Tabletas"                          = 8, 
                                 "Hidromorfona HCl 5 mg Tabletas"                            = 9, 
                                 "Meperidina HCl 100 mg/2 mL Solución Inyectable"            = 10, 
                                 "Metadona HCl 10 mg Tabletas"                               = 11, 
                                 "Metadona HCl 40 mg Tabletas"                               = 12, 
                                 "Metilfenidato HCl 18 mg Tabletas de Liberación Prolongada" = 13, 
                                 "Metilfenidato HCl 20 mg Cápsulas de Liberación Prolongada" = 14, 
                                 "Metilfenidato HCl 30 mg Cápsulas de Liberación Prolongada" = 15, 
                                 "Metilfenidato HCl 36 mg Tabletas de Liberación Prolongada" = 16, 
                                 "Metilfenidato HCl x 10 mg Comprimidos"                     = 17, 
                                 "Morfina HCl 10 mg/mL Solución Inyectable"                  = 18, 
                                 "Primidona 250 mg Tabletas"                                 = 19), 
                  selected = 8),
      br(),
      p("El medicamento seleccionado es: ", style="display:inline"),
      span(textOutput("Departamento_Var"), style = "color:blue"), 
      p(", y este presenta un consumo promedio nacional de", style="display:inline"),
      span(textOutput("sDDD_pais"), style = "color:blue"),
      p(" DDDs por 100.000 habitantes", style="display:inline"),
      br(),
      br(),
      plotOutput("scatterPlot"),
      br(),
      downloadButton(outputId = "downloadbarplot", label = "Descargar")
      
    ),
    
    # Mostrar un diagrama de la distribución generada
    mainPanel(
      
      tabsetPanel(
        tabPanel("Gráfico estático", 
                 plotOutput("distPlot"), 
                 downloadButton(outputId = "downloadPlot",  label = "Descargar Mapa con sDDD"),
                 downloadButton(outputId = "downloadPlot2", label = "Descargar Mapa con DDDc"),
                 br(), br(),
                 DT::dataTableOutput("Tab_Resumen") ),
        tabPanel("Gráfico iterativo",
                 leafletOutput("mapa_iter"))
      )
      
      
    ))
)

#-------------------------------------------------------------------------------#
# Servidor ----------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Definir la lógica del servidor requerida para hacer gráficos
server <- function(input, output) {
  # Carga de paquetes
  require(tidyverse)
  require(sf)
  require(rlang)
  require(leaflet)
  
  setwd(file.path('F:', 'Documentos', 'Estudio - Documentos', 'Trabajos', 'UAS-FNE', 
                  'Consumo_Opioides', 'Mapas'))
  
  #-------------------------------------------------------------------------------#
  # Introducción -----------------------------------------------------
  #-------------------------------------------------------------------------------#
  # Lectura de mapa de Colombia por departamento incluyendo Bogotá
  COL_map <- read_sf('Datos/gadm36_modificado.shp')
  
  #-------------------------------------------------------------------------------#
  # Lectura y modificación proyecciones poblacionales 2019
  # Se realiza lectura de proyecciones demográficas 2020 mediante codificación ISO
  PopDF <- read_csv("Datos/Poblacion/data_population.csv",
                    locale = locale("es", encoding = "ISO-8859-1", asciify = TRUE))  
  
  # Modificación inicial
  #................................................................................
  #   1 Reemplazar nombres de departamento en San Andrés y Bogotá
  #................................................................................
  
  PopDF <- PopDF %>% 
    mutate(Depto = str_replace(Depto,
    "Archipiélago de San Andrés, Providencia y Santa Catalina",
    'San Andrés y Providencia'),
    Depto = str_replace(Depto, "Bogotá, D.C.", 'Santafé de Bogotá')) 
  
  #-------------------------------------------------------------------------------#
  # Lectura de archivo con DDD para medicamentos MME
  #................................................................................
  #   1 Leer archivo de datos de MME
  #   2 Convertir columna sentence en tipo oración
  #................................................................................
  MME_DF0 <- read_csv(
    'Datos/MME/ddd_medicamentos_MME.csv',
    locale = locale("es", encoding = "ISO-8859-1", asciify = TRUE)) %>% 
    mutate(Medicamento = str_to_sentence(Medicamento))
  
  #-------------------------------------------------------------------------------#
  # Lectura de archivo con Ventas de MME
  #................................................................................
  #   1 Leer archivo de datos de venta MME para 2019
  #   2 Unir tabla de datos demográficos por "Departamento"
  #   3 Seleccionar los meses de Enero a Agosto de 2019
  #   4 Agrupar por variables "Medicamento" y "Departamento"
  #   5 Calcular promedio mensual de consumo por los 8 meses en el grupo, y calcular 
  #   el promedio de población en el resumen.
  #   6 Unir tabla de DDD
  #   7 Calcular suma de "DDDc" como ventas mensuales promedio en forma de DDD
  #   8 Calcular DDDc por cada 100.000 habitantes.
  #   9 Desagrupar tabla
  #   10 Eliminar registros de Morfina solución inyectable 3%
  #   11 Eliminar registros de Hidrato de cloral
  #   12 Agrupar por medicamento
  #   13 Calcular quantiles por grupo, y convertir en factor ordenado
  #................................................................................
  
  MMEDF <- read_csv("Modulos/preparacion_data/ventas_MME.csv") %>%
    left_join(PopDF, by = c('Departamento' = 'Depto')) %>%
    filter(Mes %in% c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago')) %>%
    group_by(Medicamento, Departamento) %>%
    summarise(mn = sum(Ventas, na.rm = TRUE) / 8,
              pp = mean(Total, na.rm = TRUE)) %>%
    left_join(MME_DF0, by = c('Medicamento')) %>%
    mutate(DDDc = mn * `mg/u` / DDD,
           sDDD = DDDc * 1E5 / pp) %>% # Consumo por 100,000 hab
    ungroup(.) %>% 
    filter(!str_detect(Medicamento, "Morfina solucion 3%")) %>%
    filter(!str_detect(Medicamento, "cloral")) %>% 
    group_by(Medicamento) %>% 
    mutate(Q = case_when(
      (sDDD <=quantile(sDDD, 0.25)) ~ 1, 
      (sDDD > quantile(sDDD, 0.25)) & (sDDD <= quantile(sDDD, 0.50)) ~ 2,
      (sDDD > quantile(sDDD, 0.50)) & (sDDD <= quantile(sDDD, 0.75)) ~ 3,
      (sDDD > quantile(sDDD, 0.75)) ~ 4, 
      TRUE ~ NA_real_),
      Q = factor(Q, 1:4)) 
  
  #-------------------------------------------------------------------------------#
  # Creación de mapa :D
  theme_set(theme_bw() + 
              theme(panel.grid      = element_blank(),
                    panel.border    = element_rect(),
                    legend.position = c(0.8, 0.80),
                    axis.title      = element_blank()))
  
  #-------------------------------------------------------------------------------#
  # Función de creación subgráficos
  mapf_subplot <- function(main, sub, xvec, yvec, fili) {
    fili <- rlang::enquo(fili)
    ggplot(main) +
      geom_sf(aes(fill = !!fili)) +
      geom_sf_text(data = sub, aes(label = Q), colour = 'black', 
                   size = 2, check_overlap = TRUE) +
      coord_sf(xlim = xvec, ylim = yvec, expand = F) +
      theme(legend.position = "none", 
            axis.ticks = element_blank(), axis.line  = element_blank(),
            axis.text  = element_blank(), axis.title = element_blank(),
            plot.margin= grid::unit(c(0,0,0,0), "mm"))
  }
  
  #-------------------------------------------------------------------------------#
  # Selección de mapa
  A <- reactive({
    unique(MMEDF$Medicamento)[as.numeric(input$bins)] %>% as.character(.)
  })
  
  #-------------------------------------------------------------------------------#
  # Creación de data.frame para mapa
  data1 <- reactive({
    MMEDF %>% filter(Medicamento == A())
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
  
  #-------------------------------------------------------------------------------#
  # Unión de datos
  output$distPlot <- renderPlot({
    G1 <- COL_tot() %>% 
      filter(!str_detect(NAME_1, 'San Andrés')) %>% 
      ggplot(.) +
      geom_sf(aes(fill = sDDD)) +
      geom_sf_text(data = COL_lab(), aes(label = Q),
                   colour = 'white', 
                   size = 2, check_overlap = TRUE) +
      coord_sf(crs = st_crs(32618)) +
      scale_fill_binned(guide = guide_bins(show.limits = TRUE,
                                           axis = FALSE))
    
    G1a <- mapf_subplot(COL_tot(), COL_lab(),
                        c(-81.75, -81.68), c(12.47, 12.62), sDDD)
    
    G1b <- mapf_subplot(COL_tot(), COL_lab(), 
                        c(-81.40, -81.34), c(13.31, 13.40), sDDD)
    
    
    G2 <- G1 + 
      annotation_custom(ggplotGrob(G1a), -3.0E5, -1.0E5, 10E5, 14E5) +
      annotation_custom(ggplotGrob(G1b), -1.0E5, +1.0E5, 12E5, 14E5)
    
    G2
  })
  
  #-------------------------------------------------------------------------------#
  # Departamento escogido
  output$Departamento_Var <- renderText({
    str_to_title(A())
  })
  
    #-------------------------------------------------------------------------------#
  # Gráfico de barras
  output$scatterPlot <- renderPlot({
  data2 <- MMEDF %>% 
    filter(Medicamento == A()) %>% 
    arrange(., desc(sDDD)) %>% 
    mutate(Departamento = fct_reorder(Departamento, desc(sDDD)))
  
  GS1 <- ggplot(data2, aes(x = Departamento, y = sDDD)) +
    geom_bar(stat='identity', 
             col='black', fill = 'green3') +
    theme_bw()+
    geom_text(aes(label = round(DDDc,2)), 
              check_overlap = TRUE,
              angle = 90, 
              nudge_y = +5)+
    theme(axis.text.x = element_text(angle = 90)) +
    labs(y = 'DDDs por 100,000 habitantes')
  
  GS1
  })
  
  #-------------------------------------------------------------------------------#
  # Descarga de mapa con sDDD -----------------------------------------------------
   
  output$downloadPlot <- downloadHandler(
    filename = function() {paste("Pais", 'pdf', sep = '.')},
    content = function(file) {
      
      G1 <- COL_tot() %>% 
        filter(!str_detect(NAME_1, 'San Andrés')) %>% 
        ggplot(.) +
        geom_sf(aes(fill = sDDD)) +
        geom_sf_text(data = COL_lab(), aes(label = Q),
                     colour = 'white', 
                     size = 2, check_overlap = TRUE) +
        coord_sf(crs = st_crs(32618)) +
        scale_fill_binned(guide = guide_bins(show.limits = TRUE,
                                             axis = FALSE)) +
        labs(title = 'DISTRIBUCIÓN NACIONAL MENSUAL PROMEDIO sDDD',
             subtitle = paste(str_to_upper(A()), "-", 
                              "sDDD: DDDs por cada 100,000 habitantes"), 
             caption = 'Nota: los numeros (blancos) en cada departamento representan el cuartil de la distribución.'
             )
      
      G1a <- mapf_subplot(COL_tot(), COL_lab(),
                          c(-81.75, -81.68), c(12.47, 12.62), sDDD)
      G1b <- mapf_subplot(COL_tot(), COL_lab(), 
                          c(-81.40, -81.34), c(13.31, 13.40), sDDD)
      G2 <- G1 + 
        annotation_custom(ggplotGrob(G1a), -3.0E5, -1.0E5, 10E5, 14E5) +
        annotation_custom(ggplotGrob(G1b), -1.0E5, +1.0E5, 12E5, 14E5)
      
      ggsave(plot = G2, filename = file, device = 'pdf', width = 8, height = 9)
    },
    contentType = 'pdf'
  )
  #-------------------------------------------------------------------------------#
  # Descarga de mapa con DDD magnitud absoluta ------------------------------------
  output$downloadPlot2 <- downloadHandler(
    
    filename = function() {paste("Pais", 'pdf', sep = '.')},
    content = function(file) {
      
      G1 <- COL_tot() %>% 
        filter(!str_detect(NAME_1, 'San Andrés')) %>% 
        ggplot(.) +
        geom_sf(aes(fill = DDDc)) +
        geom_sf_text(data = COL_lab(), aes(label = Q),
                     colour = 'white', 
                     size = 2, check_overlap = TRUE) +
        coord_sf(crs = st_crs(32618)) +
        scale_fill_binned(guide = guide_bins(show.limits = TRUE,
                                             axis = FALSE)) +
        labs(title = 'DISTRIBUCIÓN NACIONAL MENSUAL PROMEDIO DDD',
             subtitle = paste(str_to_upper(A()), "-", 
                              "DDDs totales por medicamento"), 
             caption = 'Nota: los numeros (blancos) en cada departamento representan el cuartil de la distribución.'
        )
      
      G1a <- mapf_subplot(COL_tot(), COL_lab(),
                          c(-81.75, -81.68), c(12.47, 12.62), DDDc)
      G1b <- mapf_subplot(COL_tot(), COL_lab(), 
                          c(-81.40, -81.34), c(13.31, 13.40), DDDc)
      G2 <- G1 + 
        annotation_custom(ggplotGrob(G1a), -3.0E5, -1.0E5, 10E5, 14E5) +
        annotation_custom(ggplotGrob(G1b), -1.0E5, +1.0E5, 12E5, 14E5)
      
      ggsave(plot = G2, filename = file, device = 'pdf', width = 8, height = 9)
    },
    contentType = 'pdf'
  )
  
  #-------------------------------------------------------------------------------#
  # Descarga de Gráfico de Barra -------------------------------------------------
  output$downloadbarplot <- downloadHandler(
    filename = function() { paste("Barras", 'pdf', sep='.') },
    content = function(file) {
      
      data2 <- MMEDF %>% 
        filter(Medicamento == A()) %>% 
        arrange(., desc(sDDD)) %>% 
        mutate(Departamento = fct_reorder(Departamento, desc(sDDD)))
      
      GS1 <- ggplot(data2, aes(x = Departamento, y = sDDD)) +
        geom_bar(stat='identity', 
                 col='black', fill = 'green3') +
        theme_bw()+
        geom_text(aes(label = round(DDDc,2)), 
                  check_overlap = TRUE,
                  angle = 90, 
                  nudge_y = +5) +
        labs(title = "DISTRIBUCIÓN NACIONAL PROMEDIO MENSUAL en DDD",
             subtitle = "Medicamentos Monopolio del Estado",
             caption = "Nota: en las etiquetas se muestran valores absolutos de DDD") + 
        theme(axis.text.x = element_text(angle = 90)) +
        labs(y = 'DDDs por 100,000 habitantes')
      
      ggsave(plot = GS1, filename = file, device = 'pdf',width = 8, height = 6)
    },
    contentType = 'pdf'
    
  )
   
  #-------------------------------------------------------------------------------#
  # Mapa iterativo en "tmap" ----------------------------------------------------
  
  output$mapa_iter <- renderLeaflet({
    tm <- COL_tot() %>% 
      tm_shape(., name = 'NAME_1') + 
      tm_fill("sDDD", legend.show = TRUE, 
              id = "NAME_1",
              title = "DDDs por 100.000 habitantes",
              popup.vars = c("DDDs/100.000 hab" = "sDDD", 
                             "DDD total"        = "DDDc",
                             "Ventas"           = "mn",
                             "Población"        = "pp"),
              palette = sf.colors(5)) +
      tm_borders(col = "black") +
      tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
    tmap_leaflet(tm)
  })
  
  #-------------------------------------------------------------------------------#
  # Tabla descriptiva ----------------------------------------------------
  output$Tab_Resumen <- DT::renderDataTable({
    data1() %>% 
      ungroup(.) %>% 
      select(-Medicamento, -`mg/u`, - DDD) %>% 
      mutate(DDDc = round(DDDc, 1),
             sDDD = round(sDDD, 2)) %>% 
      rename("Ventas Promedio Mes" = mn,
             "Población"           = pp,
             "DDDs totales"        = DDDc,
             "DDD/1E5 hab"         = sDDD,
             "Cuartil"             = Q)
  })
  
  #-------------------------------------------------------------------------------#
  # sDDD_pais ----------------------------------------------------
  output$sDDD_pais <- renderText({
    texto <- data1() %>%
      summarise(mn_s = sum(DDDc),
                pp_s = sum(pp),
                sDDD = mn_s * 1E5 / pp_s) %>%  # DDD por 1000 hab
      pull(sDDD) %>%
      round(., 2)
    texto
  })  

}





# Run the application 
shinyApp(ui = ui, server = server)


