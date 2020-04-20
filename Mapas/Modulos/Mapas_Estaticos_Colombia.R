##------------------------------------------------------------------------------#
## Nombre del Script: Código de obtención de mapas estáticos --------------------
##  
## Propósito del Script: realizar una estimación de consumo de medicamentos MME 
## en departamentos de Colombia.
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 07-08-2019 
##  
## Copyright (c) Daniel S. Parra, 2019 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
library(tidyverse)
library(sf)

#-------------------------------------------------------------------------------#
# Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#
# Lectura de mapa de Colombia GADM por departamento 
xx <- readRDS('Datos/gadm36_COL_1_sf.rds')
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

PopDF <- PopDF %>% mutate(Depto = str_replace(
      Depto,
      "Archipiélago de San Andrés, Providencia y Santa Catalina",
      'San Andrés y Providencia'
    ),
    Depto = str_replace(Depto, "Bogotá, D.C.", 'Santafé de Bogotá')
  ) 

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
# Unión de datos
COL_tot <- COL_map %>% 
  right_join(MMEDF, by = c('NAME_1' = "Departamento")) %>% 
  st_as_sf(.)


#-------------------------------------------------------------------------------#
# Creación de mapa :D
theme_set(theme_bw() + 
            theme(panel.grid      = element_blank(),
                  panel.border    = element_rect(),
                  legend.position = c(0.8, 0.85),
                  axis.title      = element_blank()))

COL_lab <- COL_map %>%
  sf::st_centroid() %>%
  right_join(MMEDF, by = c('NAME_1' = "Departamento")) %>% 
  filter(Medicamento == 'Metadona hcl 10 mg tabletas')



G1 <- COL_tot %>% 
  filter(!str_detect(NAME_1, 'San Andrés')) %>% 
  filter(Medicamento == 'Metadona hcl 10 mg tabletas') %>% 
  ggplot(.) +
  geom_sf(aes(fill = sDDD)) +
  geom_sf_text(data = COL_lab, aes(label = Q),
               colour = 'white', 
               size = 2, check_overlap = TRUE) +
  coord_sf(crs = st_crs(32618)) +
  scale_fill_binned(guide = guide_bins(show.limits = TRUE,
                                       axis = FALSE))

G1
COL_subplot <- COL_tot %>%
  filter(str_detect(NAME_1, 'San Andrés')) %>%
  filter(Medicamento == 'Metadona hcl 10 mg tabletas') 

COL_labsubplot <- COL_lab %>% 
  filter(str_detect(NAME_1, 'San Andrés'))

mapf_subplot <- function(main, sub, xvec, yvec) {
    ggplot(main) +
    geom_sf(aes(fill = sDDD)) +
    geom_sf_text(data = sub, aes(label = Q), colour = 'black', 
                 size = 2, check_overlap = TRUE) +
    coord_sf(xlim = xvec, ylim = yvec, expand = F) +
    theme(legend.position = "none", 
          axis.ticks = element_blank(), axis.line  = element_blank(),
          axis.text  = element_blank(), axis.title = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
}

# mapf_subplot(COL_subplot, COL_labsubplot,
#              c(-81.75, -81.68), c(12.47, 12.62))
# 

G1a <- mapf_subplot(COL_subplot, COL_labsubplot,
                    c(-81.75, -81.68), c(12.47, 12.62)) %>% 
  ggplotGrob(x = .)

G1b <- mapf_subplot(COL_subplot, COL_labsubplot, 
                    c(-81.40, -81.34), c(13.31, 13.40)) %>% 
  ggplotGrob(x = .)


G2 <- G1 + 
  # geom_hline(yintercept = 14E5) +
  # geom_hline(yintercept = 10E5) + 
  # geom_vline(xintercept = -3.0E5, col = 'red') +
  # geom_vline(xintercept = -2.0E5, col = 'red') +
  # geom_vline(xintercept = +0.0E5, col = 'red', lty = 'dotted') +
  # geom_vline(xintercept = +1.0E5, col = 'red') +
  annotation_custom(G1a, -3.0E5, -1.0E5, 10E5, 14E5) +
  annotation_custom(G1b, -1.0E5, +1.0E5, 12E5, 14E5)

#
1










#-------------------------------------------------------------------------------#
# 





# Almacenamiento de gráfico
ggsave('Figuras/Mapa_2.pdf', G2, 'pdf', width = 6, height = 8)





# Mapa de tipo Tmap ---------------------------------------------------------------------------
require(tmap)

tmap_mode("view")

COL_tot %>% 
  filter(Medicamento == "Metadona hcl 10 mg tabletas") %>% 
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
  
tmap_tip()


MMEDF %>% 
  filter(Medicamento == "Metadona hcl 10 mg tabletas") %>% 
  summarise(mn_s = sum(DDDc), 
            pp_s = sum(pp), 
            sDDD = mn_s*1E5/pp_s) %>%  # DDD por 1000 hab
  pull(sDDD) %>% 
  round(., 2)













