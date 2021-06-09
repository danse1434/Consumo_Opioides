#' Crear gráfico de Barras
#'
#' @param data
#' @param medicamento
#' @param consumo
#' @param titulo
#' @param tipo
#'
#' @return
#' @export
#'
#' @examples
#'
#'
crearGraficoBarras1 <-
  function(data, medicamento, consumo = 'sDDD', titulo = FALSE,
           tipo = 'ggplot2') {
    # Conversión a quo, verificar tipo
    # stopifnot(is.character(consumo))
    consumo_q <- rlang::ensym(consumo)
    
    # Filtrar datos
    datosFiltrados <- data %>%
      filter(Medicamento == medicamento) %>%
      arrange(., desc(!!consumo_q)) %>%
      mutate(Departamento = fct_reorder(Departamento, desc(!!consumo_q)))
    
    {
      if (consumo == 'sDDD') {
        ylabel = 'DDDs por 100,000 habitantes'
      } else {
        ylabel = ''
      }
    }
    
    if (tipo == 'ggplot2') {
      # Crear gráfico de consumo
      gs1 <-
        ggplot(datosFiltrados, aes(x = Departamento, y = !!consumo_q)) +
        geom_bar(stat = 'identity', col = 'black', fill = 'green3') +
        theme_bw() +
        geom_text(
          aes(label = round(DDDc, 2), nudge_y = !!consumo_q * 1.1),
          check_overlap = TRUE,
          angle = 90, hjust = 0
        ) +
        theme(axis.text.x = element_text(angle = 90)) +
        coord_cartesian(ylim = c(0, 100)) + 
        ylab(ylabel)
      
      # Adicionar títulos
      if (titulo) {
        # Título
        gTitulo <-
          glue::glue("DISTRIBUCIÓN NACIONAL PROMEDIO MENSUAL en {consumo}")
        gSubTitulo <- 
          glue::glue("Medicamentos Monopolio del Estado - {medicamento}")
        gCaption <-
          "Nota: en las etiquetas se muestran valores absolutos de DDD"
        
        gs1 <- gs1 +
          labs(title = gTitulo,
               subtitle = gSubTitulo,
               caption = gCaption)
      }
    } else if(tipo == 'plotly') {
      gs1 <-
        plot_ly(
          datosFiltrados,
          x = ~ Departamento,
          y = ~ eval(consumo_q),
          type = 'bar',
          marker = list(
            color = 'rgb(38,189,78)',
            line = list(color = 'rgb(0,0,0)', width = 1.5)
          )
        ) %>% layout(
          xaxis = list(title = 'Departamento', tickangle = -90),
          yaxis = list(title = 'DDDs por 100.000 habitantes'),
          plot_bgcolor = 'rgb(255, 255, 255, 0.1)',
          paper_bgcolor = 'rgb(255, 255, 255, 0.1)'
        ) %>%
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
        )
    } else {
      gs1 <- NA
    }
    
    
    
    return(gs1)
  }