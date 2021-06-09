#-------------------------------------------------------------------------------#
#' Gráfico de predicciones Plotly
#'
#' @param historico Tabla con datos de serie de tiempo (tbl_ts).
#' @param predicciones Tabla con predicciones de varios modelos, se 
#' recomienda calcular límites de intervalos de predicción.
#' @param .x Variable a graficar en eje X.
#' @param .y Variable a graficar en eje Y.
#'
#' @return
#' @export
#' Objeto de tipo _Plotly_.
#'
#' @examples
#' plotlyPredicciones(filt_data, foreC)
#' 
plotlyPredicciones <- function(historico, predicciones, 
                               .x = 'año', .y = 'distribucion') {
  x <- rlang::ensym(.x)
  y <- rlang::ensym(.y)
  
  fig <-  plot_ly(
    as_tibble(historico),
    x =  ~ eval(x),
    y =  ~ eval(y),
    type = 'scatter',
    mode = 'lines',
    showlegend = FALSE
  )
  
  fig <- fig %>%
    add_markers(
      type = "scatter",
      mode = "markers+text",
      text = ~ paste(paste('Año: ', eval(x)),
                     paste('Unidades:', distribucion), sep = '<br>'),
      
      hoverinfo = 'text',
      marker = list(color = 'rgb(0, 0, 0)'),
      showlegend = FALSE
    )
  
  u <- unique(predicciones$.model)
  
  for (i in seq_along(u)) {
    dataModel <- as_tibble(predicciones) %>%
      filter(.model == u[i])
    
    fig <- fig %>%
      add_ribbons(
        data = dataModel,
        x = ~ eval(x),
        ymin = ~ LI_P5,
        ymax =  ~ LS_P5,
        color = I(toRGB(palette()[i], 0.1)),
        name = str_to_upper(u[i]),
        showlegend = TRUE
      ) %>%
      add_lines(
        data = dataModel,
        y = ~ media,
        color = I(palette()[i]),
        name = str_to_upper(u[i]),
        showlegend = TRUE
      ) %>%
      add_markers(
        data = dataModel,
        type = "scatter",
        mode = "markers+text",
        text = ~ paste(paste('Año: ', eval(x)),
                       paste('Unidades:', media), sep = '<br>'),
        hoverinfo = 'text',
        color = I(palette()[i]) # marker = list(color = 'rgb(0, 0, 0)')
      )
  }
  
  fig <- fig %>%
    layout(
      xaxis = list(title = 'Año'),
      yaxis = list(
        title = 'Unidades Vendidas',
        showexponent = 'all',
        exponentformat = "e"
      ),
      showlegend = TRUE,
      legend = list(x = 0.1, y = 0.9)
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d")
    )
  
  return(fig)
}
