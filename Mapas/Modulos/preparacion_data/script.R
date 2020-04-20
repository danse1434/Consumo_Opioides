##------------------------------------------------------------------------------#
## Nombre del Script: preparacion de archivo de datos de consumo ----------------
##  
## Propósito del Script:  preparacion de archivo de datos de consumo, se toma archivo 
## "13. DISTRIBUCION MME", se toman los datos de la tabla, y se pegan en una hoja 
## en blanco, se borra la columna de presentación, y se borra la fila con los nombres 
## de capitales de cada departamento. Se concatenan los datos de departamento con 
## mes del año 2019. 
##  
## Autor: Daniel S. Parra González 
## Fecha de creación:  17-04-2020
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------------#
# Carga de paquetes
require(tidyverse)
require(readxl)

#-------------------------------------------------------------------------------#
# Introducción ------------------------------------------------------------------
#-------------------------------------------------------------------------------#
# Lectura de archivo de Excel
# Se lee la hoja "Inicial", desde la fila 2, y se reemplazan diversos caractéres 
# como NA. 
datos <- read_excel("Modulos/preparacion_data/datos_agosto.xlsx", 
                    sheet = "Inicial",
                    na = c("-", "_", ".", "__", " ", "#¡VALOR!", ""), 
                    skip = 2)

#-------------------------------------------------------------------------------#
# Modificación inicial de archivo
# Se encuentra que hay columnas de diciembre repetidas para META y NARIÑO, se 
# eliminan y se cambian los nombres por la denominación correcta.

datos <- datos %>%
  select(-META_DIC...242, -NARIÑO_DIC...255) %>%
  rename(META_DIC = META_DIC...241,
         NARIÑO_DIC = NARIÑO_DIC...254,
         Medicamento = M)  

#-------------------------------------------------------------------------------#
# Conversión de estructura de tabla
#................................................................................
#   1 Seleccionar el objeto *datos*
#   2 Colapsar la tabla en el tipo de registro como "Tipo" y colocar valor como 
#   "Ventas".
#   3 Separar la columna tipo en el "Departamento" y "Mes" correspondiente.
#   4 Calcular el valor absoluto de "Ventas"
#   5 Cambiar las columnas Medicamento, Departamento y Mes por texto tipo oración
#   6 Quitar espacios al final de string en la columna Departamento
#   7 Cambiar Colsusidio en Santafé de Bogotá
#   8 Cambiar Valle del Cauca, Guajira, Norte de Santander, y San Andrés por nombre 
#   de tipo oración.
#................................................................................

datos1 <- datos %>%
  pivot_longer(-Medicamento,
               names_to  = 'Tipo',
               values_to = 'Ventas') %>% 
  separate(col = Tipo, into = c('Departamento', 'Mes'), sep = "\\_") %>% 
  mutate(Ventas       = abs(Ventas),
         # Nueva característica
         across(c('Medicamento', 'Departamento', 'Mes'), 
                ~str_to_sentence(.x)),
         Departamento = str_replace_all(Departamento, "\\s$", ""),
         Departamento = str_replace(Departamento, "Colsudsidio", "Santafé de Bogotá"),
         Departamento = str_replace(Departamento, "Valle del cauca", "Valle del Cauca"),
         Departamento = str_replace(Departamento, "Guajira", "La Guajira"),
         Departamento = str_replace(Departamento, "Norte de santander", "Norte de Santander"),
         Departamento = str_replace(Departamento, "San andrés y providencia", "San Andrés y Providencia")
         ) 


#-------------------------------------------------------------------------------#
# Escritura de archivos ---------------------------------------------------------
#-------------------------------------------------------------------------------#
write_csv(x = datos1, path = "Modulos/preparacion_data/ventas_MME.csv") 
