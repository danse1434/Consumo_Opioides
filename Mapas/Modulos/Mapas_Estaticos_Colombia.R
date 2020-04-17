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
xx <- readRDS('Datos/gadm36_COL_1_sf.rds')

#-------------------------------------------------------------------------------#
# Leer Datos Poblacionales
data <- read_csv("Datos/Poblacion/data_population.csv",
                 locale = locale("es", encoding = "ISO-8859-1", asciify = TRUE))  %>%
  rename(NAME_1 = Depto)


#-------------------------------------------------------------------------------#
# Unión de datos
xx <- xx %>%
  left_join(data, by = 'NAME_1') %>% 
  st_as_sf(.)


#-------------------------------------------------------------------------------#
# Creación de mapa :D
G1 <- ggplot(data = xx) +
  geom_sf(aes(fill = Area)) +
  geom_sf_text(aes(label = NAME_1),
                size = 2, 
               check_overlap = TRUE) +
  coord_sf(crs = st_crs(32618)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        panel.border=element_rect()) + 
  scale_fill_gradientn(colours = heat.colors(32))


# Almacenamiento de gráfico
ggsave('Figuras/Mapa_1.pdf', G1, 'pdf', width = 4, height = 5)

