##------------------------------------------------------------------------#
## Nombre del Script: Adición de datos de 2019 de bases de SISMED a estudio
## de consumo de opioides -------------------------------------------------
##  
## Proposito del Script: Adicionar datos de consumo de opioides de 2019 a los
## datos de consumo de 2012 a 2018 presentes en un estudio anterior.
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creacion:  30 de enero de 2019
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
# Apertura de paquetes R
require(tidyverse)
require(readxl)
# Selección de directorio de trabajo
setwd(file.path('F:','Documentos','Estudio - Documentos','Trabajos','UAS-FNE',
                'Consumo'))

##########################################################################-
# Apertura de archivos de datos  ------------------------------------------
##########################################################################-
# Abrir los archivos de datos en formato MS Excel
data_trim_01 <- read_excel(file.path('DATOS', '2019',
      'Publicacion_PreciosReportados_2019-01_a_2019-03.XLS'), skip = 11)
data_trim_02 <- read_excel(file.path('DATOS', '2019',
      'Publicacion_PreciosReportados_2019-04_a_2019-06.XLS'), skip = 11)
data_trim_03 <- read_excel(file.path('DATOS', '2019',
      'Publicacion_PreciosReportados_2019-07_a_2019-09.XLS'), skip = 11)

##########################################################################-
# Modificación de archivo - limpieza inicial
data_trim_01 <- data_trim_01 %>%
  select(-c("...4", "...7", "...10", "...11"))
data_trim_02 <- data_trim_02 %>%
  select(-c("...4", "...7", "...10", "...11"))
data_trim_03 <- data_trim_03 %>%
  select(-c("...4", "...7", "...10", "...11"))

names_vector <- c("Medicamento",
  "Presentación",
  "Fab. Nal.",
  "Código ATC",
  "ATC",
  "Principio Activo",
  "Via Administración",
  "POS",
  "CUM",
  "SEQ",
  "Periodo de Precios",
  "Ventas Canal Comercial|Laboratorio|$ Mínimo",
  "Ventas Canal Comercial|Laboratorio|$ Máximo",
  "Ventas Canal Comercial|Laboratorio|Unidades",
  "Ventas Canal Comercial|Laboratorio|Precio",
  "Ventas Canal Comercial|Mayorista|$ Mínimo",
  "Ventas Canal Comercial|Mayorista|$ Máximo",
  "Ventas Canal Comercial|Mayorista|Unidades",
  "Ventas Canal Comercial|Mayorista|Precio",
  "Ventas Canal Institucional|Laboratorio|$ Mínimo",
  "Ventas Canal Institucional|Laboratorio|$ Máximo",
  "Ventas Canal Institucional|Laboratorio|Unidades",
  "Ventas Canal Institucional|Laboratorio|Precio",
  "Ventas Canal Institucional|Mayorista|$ Mínimo",
  "Ventas Canal Institucional|Mayorista|$ Máximo",
  "Ventas Canal Institucional|Mayorista|Unidades",
  "Ventas Canal Institucional|Mayorista|Precio",
  "Compras|Mayorista|$ Mínimo",
  "Compras|Mayorista|$ Máximo",
  "Compras|Mayorista|Unidades",
  "Compras|Mayorista|Precio",
  "Ventas EPS - IPS - DTS - CCF|Mayorista|$ Mínimo",
  "Ventas EPS - IPS - DTS - CCF|Mayorista|$ Máximo",
  "Ventas EPS - IPS - DTS - CCF|Mayorista|Unidades",
  "Ventas EPS - IPS - DTS - CCF|Mayorista|Precio",
  "Recobro|Mayorista|$ Mínimo",
  "Recobro|Mayorista|$ Máximo|",
  "Recobro|Mayorista|Unidades",
  "Recobro|Mayorista|Precio")

# Cambiar nombre de archivos
colnames(data_trim_01) <- names_vector
colnames(data_trim_02) <- names_vector
colnames(data_trim_03) <- names_vector
  
# 











