#' Creación de dataTable
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
#' 
crearDataTable <- function(df) {
  ls1 <- c('mn', 'pp', 'DDDc', 'sDDD', 'Q')
  ls2 <- c('Ventas Promedio Mes',
           'Población',
           'DDDs totales',
           'DDD/1E5 hab',
           'Cuartil')
  ls1 <- setNames(ls1, ls2)
  
  dataFrame <- df %>%
    ungroup(.) %>%
    select(-Medicamento, -`mg/u`, -DDD) %>%
    mutate(DDDc = round(DDDc, 1),
           sDDD = round(sDDD, 2)) %>%
    rename(all_of(ls1))
  
  DT::datatable(dataFrame, options = list(
    pageLength = 10,
    language = list(
      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
      # info = "Mostrando _START_ a _END_ entradas de _TOTAL_ departamentos",
      # paginate = list('next' = 'Posterior', previous = 'Anterior')
    )
  ), width = '100%', fillContainer = TRUE)
  
}