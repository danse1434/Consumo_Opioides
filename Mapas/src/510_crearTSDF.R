#-------------------------------------------------------------------------------#
#' Crea un dataFrame de tipo **tsibble**
#'
#' @param data Un DataFrame (o tibble).
#' @param medicamento Una variable (string) que determina el medicamento 
#' a estudiar.
#' @param colMed_var Una variable que indica el nombre de columna en _data_ 
#' que tiene los nombres de medicamentos.
#' @param colInd_var Una variable que indica el nombre de columna en _data_ que 
#' tiene los indices de tiempo para la serie.
#'
#' @return Un objeto tsibble.
#' @export
#'
#' @examples
#' as_tibble(airquality) %>%
#' mutate(
#'    Fecha = paste(Day, Month, '1973', sep = '-'),
#'    Fecha = as.Date(Fecha, format = "%d-%m-%Y")
#'    ) %>% 
#' extraerMedicTS(., 5, 'Month', 'Fecha')
#' 
#' 
extraerMedicTS <- function(data, medicamento, 
                           colMed_var='medicamento', colInd_var = 'ano') {
  
  colMed <- rlang::ensym(colMed_var)
  colInd <- rlang::ensym(colInd_var)
  
  filt_data <- data %>% 
    filter((!!colMed) == !!medicamento) %>%
    tsibble::tsibble(key = !!colMed, index = !!colInd)
  
  return(filt_data)
}