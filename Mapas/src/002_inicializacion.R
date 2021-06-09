#-------------------------------------------------------------------------------#
# Introducción -----------------------------------------------------
#-------------------------------------------------------------------------------#
# 1. Lectura de mapa de Colombia por departamento incluyendo Bogotá
COL_map <- read_sf(file.path('data', 'gadm36_modificado.shp'))

# 2. Lectura y modificación proyecciones poblacionales 2019
# Se realiza lectura de proyecciones demográficas 2020 mediante codificación ISO
PopDF <- read_csv(file.path('data', 'raw', 'data_population.csv'),
                  locale = locale('es', encoding = 'ISO-8859-1', asciify = TRUE))  

# Modificación inicial
#................................................................................
#   1 Reemplazar nombres de departamento en San Andrés y Bogotá
#................................................................................

PopDF <- PopDF %>% 
  mutate(Depto = str_replace(Depto,
                             'Archipiélago de San Andrés, Providencia y Santa Catalina',
                             'San Andrés y Providencia'),
         Depto = str_replace(Depto, 'Bogotá, D.C.', 'Santafé de Bogotá')) 

#-------------------------------------------------------------------------------#
# Lectura de archivo con DDD para medicamentos MME
#................................................................................
#   1 Leer archivo de datos de MME
#   2 Convertir columna sentence en tipo oración
#................................................................................
MME_DF0 <- read_csv(
  file.path('data', 'raw', 'ddd_medicamentos_MME.csv'), 
  locale = locale('es', encoding = 'ISO-8859-1', asciify = TRUE)) %>% 
  mutate(Medicamento = str_to_sentence(Medicamento))

#-------------------------------------------------------------------------------#
# Lectura de archivo con Ventas de MME
#................................................................................
#   1 Leer archivo de datos de venta MME para 2019
#   2 Unir tabla de datos demográficos por 'Departamento'
#   3 Seleccionar los meses de Enero a Agosto de 2019
#   4 Agrupar por variables 'Medicamento' y 'Departamento'
#   5 Calcular promedio mensual de consumo por los 8 meses en el grupo, y calcular 
#   el promedio de población en el resumen.
#   6 Unir tabla de DDD
#   7 Calcular suma de 'DDDc' como ventas mensuales promedio en forma de DDD
#   8 Calcular DDDc por cada 100.000 habitantes.
#   9 Desagrupar tabla
#   10 Eliminar registros de Morfina solución inyectable 3%
#   11 Eliminar registros de Hidrato de cloral
#   12 Agrupar por medicamento
#   13 Calcular quantiles por grupo, y convertir en factor ordenado
#................................................................................

MMEDF <- read_csv(file.path('data', 'intermediate', 'ventas_MME.csv')) %>%
  left_join(PopDF, by = c('Departamento' = 'Depto')) %>%
  filter(Mes %in% c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago')) %>%
  group_by(Medicamento, Departamento) %>%
  summarise(mn = sum(Ventas, na.rm = TRUE) / 8,
            pp = mean(Total, na.rm = TRUE)) %>%
  left_join(MME_DF0, by = c('Medicamento')) %>%
  mutate(DDDc = mn * `mg/u` / DDD,
         sDDD = DDDc * 1E5 / pp) %>% # Consumo por 100,000 hab
  ungroup(.) %>% 
  filter(!str_detect(Medicamento, 'Morfina solucion 3%')) %>%
  filter(!str_detect(Medicamento, 'cloral')) %>% 
  group_by(Medicamento) %>% 
  mutate(Q = case_when(
    (sDDD <=quantile(sDDD, 0.25)) ~ 1, 
    (sDDD > quantile(sDDD, 0.25)) & (sDDD <= quantile(sDDD, 0.50)) ~ 2,
    (sDDD > quantile(sDDD, 0.50)) & (sDDD <= quantile(sDDD, 0.75)) ~ 3,
    (sDDD > quantile(sDDD, 0.75)) ~ 4, 
    TRUE ~ NA_real_),
    Q = factor(Q, 1:4)) 

#-------------------------------------------------------------------------------#
# Creación de mapa :D
theme_set(theme_bw() + 
            theme(panel.grid      = element_blank(),
                  panel.border    = element_rect(),
                  legend.position = c(0.8, 0.80),
                  axis.title      = element_blank()))
