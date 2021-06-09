#' Función de creación subgráficos
#'
#' @param main 
#' @param sub 
#' @param xvec 
#' @param yvec 
#' @param fili 
#'
#' @return
#' @export
#'
#' @examples
#' 
mapf_subplot <- function(main, sub, xvec, yvec, fill) {
  fill_q <- rlang::ensym(fill)
  ggplot(main) +
    geom_sf(aes(fill = !!fill_q)) +
    geom_sf_text(data = sub, aes(label = Q), colour = 'black', 
                 size = 2, check_overlap = TRUE) +
    coord_sf(xlim = xvec, ylim = yvec, expand = F) +
    theme(legend.position = "none", 
          axis.ticks = element_blank(), axis.line  = element_blank(),
          axis.text  = element_blank(), axis.title = element_blank(),
          plot.margin= grid::unit(c(0,0,0,0), "mm"))
}