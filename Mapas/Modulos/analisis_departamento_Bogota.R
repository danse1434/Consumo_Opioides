##------------------------------------------------------------------------------#
## Nombre del Script: Adición de Bogotá como departamento de Colombia  ----------
##  
## Propósito del Script: adicionar Bogotá como un departamento de Colombia adicional 
## en el mapa obtenido de GADM versión 36, con el fin de mejorar la visibilidad del 
## mismo (script de un sólo uso).
##  
## Autor: Daniel S. Parra González 
## Fecha de creación: 17-04-2010 
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
library(tidyverse)
library(sf)

# Configuración de directorio de trabajo
setwd(file.path('F:', 'Documentos', 'Estudio - Documentos', 'Trabajos', 'UAS-FNE', 
                'Consumo_Opioides', 'Mapas'))

#-------------------------------------------------------------------------------#
# Introducción ------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Lectura de datos
# Los datos son descargados del sitio web https://gadm.org/download_country_v3.htm
# Los archivos se descargan como un objeto de R de tipo R (sp), y se ubican en la 
# carpeta correspondiente.
munic <- readRDS('Datos/gadm36_COL_2_sf.rds') # Mapa DIVIPOLA por municipio
depto <- readRDS('Datos/gadm36_COL_1_sf.rds') # Mapa DIVIPOLA por departamento

#-------------------------------------------------------------------------------#
# Procesamiento inicial de los datos  -------------------------------------------
#-------------------------------------------------------------------------------#
# Obtención de datos de la ciudad de Bogotá, a partir de datos de municipio
munic1 <- munic %>% 
  filter(str_detect(NAME_2, "Bogot"))
# Obtención de datos sf del departamento de Cundinamarca
depto1 <- depto %>% 
  filter(str_detect(NAME_1, "Cundi"))

# Visualización preliminar de ciudad y departamento seleccionados
# munic1 %>% plot()
# depto1 %>% plot()

#-------------------------------------------------------------------------------#
# Obtención de diferencia entre departamento y municipio seleccionados
cundi1 <- sf::st_difference(x = depto1, y = munic1)

# Selección de todos los municipios a excepción de Cundinamarca
depto2 <- depto %>% 
  filter(!str_detect(NAME_1, "Cundi"))

#-------------------------------------------------------------------------------#
# Creación de un nuevo archivo de depto con adición de cundinamarca, a la cual se 
# le han retirado columnas innecesarias, para permitir compatibilidad con *depto2*
depto <- cundi1 %>% 
  select(-ends_with("_2")) %>% 
  select(-ends_with(".1")) %>% 
  rbind(depto2, .)

#-------------------------------------------------------------------------------#
# Incorporación de datos de la ciudad de Bogotá D.C.
#................................................................................
#   1 Seleccionar nombre NAME_1 (Cundinamarca) y colocar el nombre NAME_2 (Bogotá 
#   D.C.).
#   2 Cambiar el valor de GID_1 por "COL.33_1"
#   3 Seleccionar *munic1*
#   4 Eliminar las variables: 'GID_2', 'NAME_2', 'NL_NAME_2'
#   5 Renombrar columnas seleccionadas
#   6 Unir datos de ciudad con el resto de los departamentos en *total*. 
#................................................................................

munic1[,"NAME_1"]$NAME_1 = munic1[,'NAME_2']$NAME_2
munic1[,"GID_1"]$GID_1 = "COL.33_1"

bogota <- munic1 %>%
  select(-GID_2, -NAME_2, -NL_NAME_2) %>% 
  rename(VARNAME_1 = VARNAME_2,
         TYPE_1    = TYPE_2,
         ENGTYPE_1 = ENGTYPE_2,
         CC_1      = CC_2,
         HASC_1    = HASC_2)
         
total <- rbind(x = depto, y = bogota)

#-------------------------------------------------------------------------------#
# Escritura de archivo nuevo de tipo st_write -----------------------------------
#-------------------------------------------------------------------------------#
# Escritura como "gadm35_modificado.shp"
st_write(obj = total, dsn = "Datos/gadm36_modificado.shp")
         


















