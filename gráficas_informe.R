# EUM CONSUMO DE OPIOIDES EN COLOMBIA 2012 - 2018

## Carga de Paquetes
require(tidyverse)
require(lubridate)
require(treemap)
require(directlabels)
## Configurar Directorio de Trabajo
setwd(dir = file.path('F:','Documentos','Estudio - Documentos','Trabajos','UAS-FNE','Consumo'))

## Carga de Archivos de SISMED
data_SISMED_2012 = read_csv(file = './DATOS/SISMED_2012_OPIOIDES.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data_SISMED_2013 = read_csv(file = './DATOS/SISMED_2013_OPIOIDES.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data_SISMED_2014 = read_csv(file = './DATOS/SISMED_2014_OPIOIDES.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data_SISMED_2015 = read_csv(file = './DATOS/SISMED_2015_OPIOIDES.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data_SISMED_2016 = read_csv(file = './DATOS/SISMED_2016_OPIOIDES.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data_SISMED_2017 = read_csv(file = './DATOS/SISMED_2017_OPIOIDES.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data_SISMED_2018 = read_csv(file = './DATOS/SISMED_2018_OPIOIDES.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
## Unir los Archivos de 2012 a 2016 en BASE MAESTRA
data_SISMED = bind_rows(data_SISMED_2012, data_SISMED_2013, data_SISMED_2014, data_SISMED_2015, data_SISMED_2016, data_SISMED_2017,data_SISMED_2018)
## Eliminacion de datos del monopolio del Estado
data_SISMED = data_SISMED  %>% 
  filter(!(`Código ATC` %in% c("N02AA03", "N02AB02", "N02AA01", "N02AC52")) )
# Se eliminan registros de monopolio del estado, de esta BD Maestra, ya que se usaran los datos internos del FNE: hidromorfona (N02AA03), meperidina (N02AB02), 
# morfina (N02AA01), metadona (N02AC52)
## Se debe obtener el ano al que corresponde cada registro
data_SISMED[,'Periodo de Precios'] = apply(data_SISMED[,'Periodo de Precios'], 
                                           MARGIN = 1, FUN = function(x){str_sub(x, 0, 4)}) %>% as.double(.)
# Esta funcion obtiene los primeros 4 digitos de esta columna, y convierte todas las celdas en numero
# Apertura de Base de Datos de Monopolio del Estado -----------------------
data_MME = read_csv(file = './DATOS/Ventas_MME.csv',col_names = TRUE)
colnames(data_MME) = colnames(data_SISMED) # Se colocan los mismos encabezados que ya han sido modificados en esta base de datos
# Se retiran sólo los años requeridos para el estudio, ya que hay datos desde 1997 en esta BD
data_MME <- data_MME %>% 
  filter(`Periodo de Precios` >= 2012)
# data_MME = data_MME[data_MME[,"Periodo de Precios"] >= 2012,]
# Unión de Bases de Datos -------------------------------------------------
# Se crea una BD con datos de SISMED y datos de referencia interna de ventas
data = bind_rows(data_SISMED, data_MME)
## Ajustes Iniciales a Base de Datos Maestra
data = data %>% 
  rename(Presentacion = colnames(data[,2])) %>% 
  rename(ATC_Code = colnames(data[,4])) %>% 
  rename(Via_Admon = colnames(data[,7])) %>%
  rename(Año = colnames(data[,11])) %>% 
  rename(VCC_LAB_MIN = 'Ventas Canal Comercial|Laboratorio|$ Mínimo') %>% 
  rename(VCC_LAB_MAX = 'Ventas Canal Comercial|Laboratorio|$ Máximo') %>% 
  rename(VCC_LAB_UND = 'Ventas Canal Comercial|Laboratorio|Unidades') %>% 
  rename(VCC_LAB_MED = 'Ventas Canal Comercial|Laboratorio|Precio') %>% 
  rename(VCC_MAY_MIN = 'Ventas Canal Comercial|Mayorista|$ Mínimo') %>% 
  rename(VCC_MAY_MAX = 'Ventas Canal Comercial|Mayorista|$ Máximo') %>% 
  rename(VCC_MAY_UND = 'Ventas Canal Comercial|Mayorista|Unidades') %>% 
  rename(VCC_MAY_MED = 'Ventas Canal Comercial|Mayorista|Precio') %>% 
  rename(VCI_LAB_MIN = 'Ventas Canal Institucional|Laboratorio|$ Mínimo') %>% 
  rename(VCI_LAB_MAX = 'Ventas Canal Institucional|Laboratorio|$ Máximo') %>% 
  rename(VCI_LAB_UND = 'Ventas Canal Institucional|Laboratorio|Unidades') %>% 
  rename(VCI_LAB_MED = 'Ventas Canal Institucional|Laboratorio|Precio') %>% 
  rename(VCI_MAY_MIN = 'Ventas Canal Institucional|Mayorista|$ Mínimo') %>% 
  rename(VCI_MAY_MAX = 'Ventas Canal Institucional|Mayorista|$ Máximo') %>% 
  rename(VCI_MAY_UND = 'Ventas Canal Institucional|Mayorista|Unidades') %>% 
  rename(VCI_MAY_MED = 'Ventas Canal Institucional|Mayorista|Precio') %>% 
  rename(COMPRAS_MIN = 'Compras|Mayorista|$ Mínimo') %>% 
  rename(COMPRAS_MAX = 'Compras|Mayorista|$ Máximo') %>% 
  rename(COMPRAS_UND = 'Compras|Mayorista|Unidades') %>% 
  rename(COMPRAS_MED = 'Compras|Mayorista|Precio') %>% 
  rename(GRUPO_MIN = 'Ventas EPS - IPS - DTS - CCF|Mayorista|$ Mínimo') %>% 
  rename(GRUPO_MAX = 'Ventas EPS - IPS - DTS - CCF|Mayorista|$ Máximo') %>% 
  rename(GRUPO_UND = 'Ventas EPS - IPS - DTS - CCF|Mayorista|Unidades') %>% 
  rename(GRUPO_MED = 'Ventas EPS - IPS - DTS - CCF|Mayorista|Precio') %>% 
  rename(RECOB_MIN = 'Recobro|Mayorista|$ Mínimo') %>% 
  rename(RECOB_MAX = 'Recobro|Mayorista|$ Máximo|') %>% 
  rename(RECOB_UND = 'Recobro|Mayorista|Unidades') %>% 
  rename(RECOB_MED = 'Recobro|Mayorista|Precio')

## Borrar tablas que no se necesitan
rm('data_SISMED_2012', 'data_SISMED_2013','data_SISMED_2014','data_SISMED_2015','data_SISMED_2016','data_SISMED_2017','data_SISMED_2018')
## Apertura de Archivo de Referencia por CUM
REF_Tabla = read_csv(file = './DATOS/REF_CUM.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data_aux = data

## Unión de Archivo
data = data %>% 
  mutate('CUM-SEQ' = paste0(CUM,'-',SEQ)) %>% 
  inner_join(., REF_Tabla, by = 'CUM-SEQ') %>% 
  select(-c('Medicamento.y', 'Presentación', 'Código ATC', 'ATC.y', 'Principio Activo.y')) %>% 
  filter(!is.na(A)) %>% 
  mutate(DDD_T = (VCC_LAB_UND + VCI_LAB_UND)*A*B/C) %>%  # Cálculo DDD por ventas de laboratorios a canales comerciales e institucionales
  mutate(Costos_T = (VCC_LAB_UND * VCC_LAB_MED) + (VCI_LAB_UND * VCI_LAB_MED)) %>% # Ventas en pesos colombianos VCC, y VCI
  rename('CUM_SEQ' = `CUM-SEQ`)

#####  
# Se encuentra que el valor de ventas reportados para Nodol Tabletas es muy alto
# master %>% 
#  filter(str_detect(Medicamento, 'NODOL')) %>% 
#   group_by(ATC_Code) %>% 
#   summarise(sum = sum(`Ventas Canal Comercial|Laboratorio|Unidades`))
# Suma de Ventas 9.297.371 es muy alto casi sobrepasando a Dolex que es el analgésico más vendido en el país.
# Se tiene una alta sospecha que el valor reportado corresponde a unidades farmacéuticas

# La Dihidrocodeína no se utiliza en dolor sino como antitusivo se debe retirar
# master %>%
#   filter(ATC_L == "DIHIDROCODEINA") %>%
#   arrange(desc(`Ventas Canal Comercial|Laboratorio|Unidades`)) %>%
#   View(.)

# VERIFICACIÓN TRAMADOL 
# master = master %>% 
#   filter(ATC_L != "DIHIDROCODEINA")
# 
# master = master %>% 
#   filter((ATC_L %in% c("TRAMADOL"))) %>% 
#   arrange(desc(`Ventas Canal Comercial|Laboratorio|Unidades`)) %>% 
#   select(., c("Medicamento", "CUM", "Año"), matches("Unidad\\w+"))
# 
# master[,"Venta_Totales"] = master[,4] + master[,6]
# master[,"Venta_CUM"] = cumsum(master[,"Venta_Totales"])
# 
# #master = master[master[,"Año"] == 2017,]
# 
# ggplot(master, aes(x = Medicamento)) + 
#   geom_point(aes(y=master$Venta_CUM)) + 
#   geom_path(aes(y=master$Venta_CUM, group = 1))
# 
# write.csv(x = master, file = "borradme.csv")
# master %>% View(.)
# 

# Se encuentra que el valor de Nodol Tabletas es muy alto # master %>% 
#  filter(str_detect(Medicamento, 'NODOL')) %>% 
#   group_by(ATC_Code) %>% 
#   summarise(sum = sum(`Ventas Canal Comercial|Laboratorio|Unidades`))
# Suma de Ventas 9.297.371 es muy alto casi sobrepasando a Dolex que es el analgésico más vendido en el país.
# Se tiene una alta sospecha que el valor reportado corresponde a unidades farmacéuticas

# La Dihidrocodeína no se utiliza en dolor sino como antitusivo se debe retirar
# master %>%
#   filter(ATC_L == "DIHIDROCODEINA") %>%
#   arrange(desc(`Ventas Canal Comercial|Laboratorio|Unidades`)) %>%
#   View(.)
#####
data = data %>% 
  mutate(New = grepl('NODOL', data$Medicamento.x)) %>% # Seleccionamos aquellos registros con el medicamento NODOL
  mutate(DDD_T = if_else(New == TRUE, DDD_T/A, DDD_T)) %>% # Los tratamos como si hubiesen sido reportados como UF
  select(., -c('New')) %>% # Remover la columna auxiliar
  #filter(ATC_LIMPIO != "DIHID-ROCODEINA") %>% # Se elimina dihidrocodeína que es antitusivo
  mutate_at(vars(A, B, C, DDD_T), as.numeric) # Volver las Variables mencionadas en números

data = data %>% 
  mutate(Marca = gsub("([A-Za-z]+).*", "\\1", data$Medicamento.x)) # Selecciona la primera palabra en el nombre del medicamento (marca)

data[,"Marca"] = apply(data[,"Marca"], MARGIN = 1, FUN = function(x){str_to_title(x, locale = "en")})
data[,"Medicamento.x"] = apply(data[,"Medicamento.x"], MARGIN = 1, FUN = function(x){str_to_title(x, locale = "en")})
data[,"Concatenado"] <- paste(data$Marca, "\n", data$CUM)

# Ajustes de la Base de Datos ---------------------------------------------
# Se cambia el producto Tramasindol con CUM 20080775-1 ya que tiene un valor alto de unidades (+100) pero aún reportan 
# un precio de 2300 pesos colombianos por cada caja de 100 frascos, y esto no tiene sentido
# De manera adicional, no se tiene una amplia variabilidad en los precios minimos y máximos reportados
# Se realiza un cambio en R para modificar 20080775-1 por 20080775-3, en la base de datos maestra.
data$DDD_T[data$CUM_SEQ == '20080775-3'] <- data$DDD_T[data$CUM_SEQ == '20080775-3']/100
data$A[data$CUM_SEQ == '20080775-3'] <- data$A[data$CUM_SEQ == '20080775-3']/100
# Se encuentran valores muy altos para dos CUM_SEQ en el año 2012, haciendo un exámen más a fondo se encuentra que:
# 19934552-5 tiene 548,973.02 unidades vendidas con costo altamente variable: 272.13 (0.29 - 186,000)
# 19986830-4 tiene 622,966 unidades vendidas con costo variable 276.11 (230-35000) 
# Esto sugiere que el datos reportado está en unidades farmacéuticas por el bajo precio, en el primer caso parece haber 
# un reporte mixto de datos entre unidades farmacéuticas y comerciales.
# Por tal, se divide por el valor A en estos casos específicos y sólo para este año, no hay más anomalías. 
data[(data$CUM_SEQ=='19934552-5') & (data$Año==2012),]$DDD_T = data[(data$CUM_SEQ=='19934552-5') & (data$Año==2012),]$DDD_T/
  data[(data$CUM_SEQ=='19934552-5') & (data$Año==2012),]$A
data[(data$CUM_SEQ=='19934552-5') & (data$Año==2012),]$A = data[(data$CUM_SEQ=='19934552-5') & (data$Año==2012),]$A/
  data[(data$CUM_SEQ=='19934552-5') & (data$Año==2012),]$A

data[(data$CUM_SEQ=='19986830-4') & (data$Año==2012),]$DDD_T = data[(data$CUM_SEQ=='19986830-4') & (data$Año==2012),]$DDD_T/
  data[(data$CUM_SEQ=='19986830-4') & (data$Año==2012),]$A
data[(data$CUM_SEQ=='19986830-4') & (data$Año==2012),]$A = data[(data$CUM_SEQ=='19986830-4') & (data$Año==2012),]$A/
  data[(data$CUM_SEQ=='19986830-4') & (data$Año==2012),]$A

#####
# Escritura de Archivo de Datos en Bruto
write.csv(x = data, file = './RESULTADOS/BASE_DATOS/data.csv')


data = 
  data %>% filter(DDD_T != Inf)

pdf(file = './RESULTADOS/INFORME/IN_MAP_OP_CONSUMO.pdf', width = 10.96083*1,height = 11*1)
{treemap(filter(data, Año >= 2018), 
        index = c("MME", "ATC_LIMPIO", "Concatenado"), 
        vSize = "DDD_T", 
        type = "index", 
        palette = "Greens", 
        title = "Consumo de Opioides - Año 2018 (DDD)", 
        fontsize.title = 14)
}; dev.off()

pdf(file = './RESULTADOS/INFORME/IN_MAP_OP_CONSUMO_MCE.pdf', width = 10.96083*1,height = 11*1)
{treemap(filter(data, Año >= 2018, MME %in% c('MCE', 'MME')), 
         index = c("MME", "ATC_LIMPIO", "Concatenado"), 
         vSize = "DDD_T", 
         type = "index", 
         palette = "Greens", 
         title = "Consumo de Opioides Controlados - Año 2018 (DDD)", 
         fontsize.title = 14)
}; dev.off()

pdf(file = './RESULTADOS/INFORME/IN_MAP_OP_VENTAS.pdf', width = 10.96083*1,height = 11*1)
{treemap(filter(data, Año >= 2018), 
         index = c("MME", "ATC_LIMPIO", "Concatenado"), 
         vSize = "Costos_T", 
         type = "index", 
         palette = "Reds", 
         title = "Ventas de Opioides - Año 2018 (Pesos)", 
         fontsize.title = 14)
}; dev.off()

pdf(file = './RESULTADOS/INFORME/IN_MAP_OP_VENTAS_MCE.pdf', width = 10.96083*1,height = 11*1)
{treemap(filter(data, Año >= 2018, MME %in% c('MCE', 'MME')), 
         index = c("MME", "ATC_LIMPIO", "Concatenado"), 
         vSize = "Costos_T", 
         type = "index", 
         palette = "Reds", 
         title = "Ventas de Opioides Controlados - Año 2018 (Pesos)", 
         fontsize.title = 14)
}; dev.off()

## Realizar un Resumen por MME
data1 = data %>% 
  group_by(MME, Año, ATC_LIMPIO) %>% 
  summarise(sum_DDT = sum(DDD_T), 
            sum_Costos = sum(Costos_T)) 

data1 <- data1 %>% 
  mutate(ATC_LIMPIO = factor(ATC_LIMPIO))

# data1$ATC_LIMPIO = as.factor(data1$ATC_LIMPIO)

data1 = data1 %>% 
  filter(sum_DDT != 0) # Se eliminan aquellos opioides que tienen un consumo igual a cero

# Apertura de Archivo de Censo en Colombia
CENSO_DATA = read_csv(file = './DATOS/POBLACION/CENSO_COLOMBIA_DANE.csv',col_names = TRUE, locale = readr::locale(encoding = "latin1"))
data1 = data1 %>% 
  inner_join(., CENSO_DATA, by = 'Año') %>% 
  mutate(Prop_Uso = sum_DDT*100000/(Censo*365))


data = data %>% 
  inner_join(., CENSO_DATA, by = 'Año') %>% 
  mutate(Prop_Uso = DDD_T*100000/(Censo*365))

data1$ATC_LIMPIO = fct_relevel(data1$ATC_LIMPIO, 
                       c("TRAMADOL", "HIDROCODONA", "DIHIDROCODEINA","CODEINA", "FENTANILO", "BUPRENORFINA", "OXICODONA", 
                       "TAPENTADOL", "MORFINA", "HIDROMORFONA", "METADONA", "MEPERIDINA")) %>% fct_rev(.)


cols <- c("TRAMADOL" = 'green4', "HIDROCODONA" = 'green3', "CODEINA" = 'green1', "DIHIDROCODEINA" = "green4",
          "FENTANILO"  = 'indianred4', "BUPRENORFINA" = 'indianred3', "OXICODONA"='indianred2', "TAPENTADOL"='indianred1',
          "MORFINA"='#6970ff', "HIDROMORFONA"='#6999ff', "METADONA"='#acc5fc', "MEPERIDINA"='#b5eeff')

attr(cols,"names") = attr(cols,"names") %>% str_to_title(.) # Se cambian los valores a minúscula por legibilidad
data1$ATC_LIMPIO = data1$ATC_LIMPIO %>% fct_relabel(., ~ str_to_title(.x))

pdf(file = './RESULTADOS/INFORME/Tendencia/Tendencia_Crecimiento_OP.pdf', width = 6*1.5,height = 4*1.5)
{ggplot(data1, aes(x = Año,y = Prop_Uso, group = ATC_LIMPIO, fill = ATC_LIMPIO)) + 
  geom_col(col='gray10') + 
  scale_x_continuous(sec.axis = dup_axis(labels = NULL, name = NULL), breaks = c(2012:2018)) +
  xlab("Año") + 
  ylab("DDD/100.000 hab/día") + 
  ggtitle(expression(paste("Tendencia en Consumo de Opioides (",DDD[C],")"))) +
  labs(caption = "Tomado de Base de Datos de SISMED - Ministerio de Salud y la Protección Social") +
  theme_bw() + 
  scale_fill_manual('Fármaco',values = cols)}; dev.off()

pdf(file = './RESULTADOS/INFORME/Tendencia/Tendencia_Crecimiento_OP_MCE.pdf', width = 6*1.5,height = 4*1.5)
{ggplot(filter(data1,MME %in% c('MCE','MME')), aes(x = Año,y = Prop_Uso, group = ATC_LIMPIO, fill = ATC_LIMPIO)) + 
  geom_col(col='gray10') + 
  scale_x_continuous(sec.axis = dup_axis(labels = NULL, name = NULL), breaks = c(2012:2018)) +
  xlab("Año") + 
  ylab("DDD/100.000 hab/día") + 
  ggtitle(expression(paste("Tendencia en Consumo de Opioides (",DDD[C],") - MCE"))) +
  labs(caption = "Tomado de Base de Datos de SISMED - Ministerio de Salud y la Protección Social") +
  theme_bw() + 
  scale_fill_manual('Fármaco',values = cols)}; dev.off()


# Gráficos de Líneas Tendencia de Consumo Anual ---------------------------
cols <- c("TRAMADOL" = '#000000', "HIDROCODONA" = '#E69F00', "CODEINA" = 'red3', "DIHIDROCODEINA" = 'green4',
          "FENTANILO"  = '#009E73', "BUPRENORFINA" = 'goldenrod4', "OXICODONA"='#0072B2', "TAPENTADOL"='#D55E00',
          "MORFINA"='#AD9A76', "HIDROMORFONA"='#34373B', "METADONA"='#BFC79B', "MEPERIDINA"='#6C727A')
attr(cols,"names") = attr(cols,"names") %>% str_to_title(.)

cols_lty <- c("TRAMADOL"=4, "HIDROCODONA" = 4, "CODEINA" = 4, "DIHIDROCODEINA" = 4,
              "FENTANILO"=2, "BUPRENORFINA"=2, "OXICODONA"=2,"TAPENTADOL"=2,
              "MORFINA"=1, "HIDROMORFONA"=1, "METADONA"=1, "MEPERIDINA"=1)
attr(cols_lty,"names") = attr(cols_lty,"names") %>% str_to_title(.)

# Consumo de Opiodes Debiles

p <- ggplot(filter(data1,MME %in% c('NMCE')), aes(x = Año,y = Prop_Uso, group = ATC_LIMPIO)) + 
  scale_x_continuous(breaks = c(2012:2018), limits = c(2012,2020)) +
  geom_line(aes(lty = ATC_LIMPIO, colour = ATC_LIMPIO)) + 
  geom_point(aes(lty = ATC_LIMPIO, colour = ATC_LIMPIO)) +
  theme_classic() +
  xlab('Año') + ylab('DDD/100.000 hab/día') + 
  scale_colour_manual('Fármaco', values = cols) +
  scale_linetype_manual('Fármaco',values = cols_lty) + 
  ggtitle('Consumo de Opioides no Controlados') +
  labs(caption = "Adaptado de Base de Datos de SISMED - MSPS") +
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = NA, colour = NA), panel.background =  element_rect(fill = NA, colour = NA))

# No se van a agregar ejes secundarios: sec.axis = dup_axis(labels = NULL, name = NULL)

pdf(file = './RESULTADOS/INFORME/IN_TENDENCIA_OP_NMCE.pdf', width = 7.5,height = 5)
{direct.label(p, list("last.points", hjust = -0.10))}; dev.off()


# Consumo de Opiodes Fuertes
p1 <- ggplot(filter(data1,MME %in% c('MCE')), aes(x = Año,y = Prop_Uso, group = ATC_LIMPIO)) + 
  scale_x_continuous(breaks = c(2012:2018), limits = c(2012,2020)) +
  geom_line(aes(lty = ATC_LIMPIO, colour = ATC_LIMPIO)) + 
  geom_point(aes(lty = ATC_LIMPIO, colour = ATC_LIMPIO)) +
  theme_classic() +
  xlab('Año') + ylab('DDD/100.000 hab/día') + 
  scale_colour_manual('Fármaco', values = cols) +
  scale_linetype_manual('Fármaco',values = cols_lty) + 
  ggtitle(expression(paste("Consumo de Opioides MCE (",DDD[C],")"))) +
  labs(caption = "Adaptado de Base de Datos de SISMED - MSPS") +
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = NA, colour = NA), panel.background =  element_rect(fill = NA, colour = NA))

p2 <- ggplot(filter(data1,MME %in% c('MME')), aes(x = Año,y = Prop_Uso, group = ATC_LIMPIO)) + 
  scale_x_continuous(breaks = c(2012:2018), limits = c(2012,2020)) +
  geom_line(aes(lty = ATC_LIMPIO, colour = ATC_LIMPIO)) + 
  geom_point(aes(lty = ATC_LIMPIO, colour = ATC_LIMPIO)) +
  theme_classic() +
  xlab('Año') + ylab('DDD/100.000 hab/día') + 
  scale_colour_manual('Fármaco', values = cols) +
  scale_linetype_manual('Fármaco',values = cols_lty) + 
  ggtitle(expression(paste("Consumo de Opioides MME (",DDD[C],")"))) +
  labs(caption = "Adaptado de Base de Datos de SISMED - MSPS") +
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = NA, colour = NA), panel.background =  element_rect(fill = NA, colour = NA))

pdf(file = './RESULTADOS/INFORME/IN_TENDENCIA_OP_MCE.pdf', width = 7.5,height = 5)
{direct.label(p1, list("last.points", hjust = -0.10, vjust = 1))}; dev.off()

pdf(file = './RESULTADOS/INFORME/IN_TENDENCIA_OP_MME.pdf', width = 7.5,height = 5)
{direct.label(p2, list("last.points", hjust = -0.10, vjust = 1))}; dev.off()

write.csv(x = data, file = './RESULTADOS/BASE_DATOS/Dato_Original.csv')
write.csv(x = data1, file = './RESULTADOS/BASE_DATOS/Dato_Resumen.csv')

# Costos ------------------------------------------------------------------
data2 = 
data %>% 
  mutate(Costo_DDD = ((VCC_LAB_UND*VCC_LAB_MED)+(VCI_LAB_UND*VCI_LAB_MED))/(DDD_T)) %>% 
  filter(Costo_DDD>0) %>% 
  filter(Año %in% c(2018))

data2$ATC_LIMPIO = fct_relevel(data2$ATC_LIMPIO, 
                               c("TRAMADOL", "HIDROCODONA","DIHIDROCODEINA", "CODEINA", "FENTANILO", "BUPRENORFINA", "OXICODONA", 
                                 "TAPENTADOL", "MORFINA", "HIDROMORFONA", "METADONA", "MEPERIDINA"))
data2$ATC_LIMPIO = data2$ATC_LIMPIO %>% fct_relabel(., ~ str_to_title(.x))

cols <- c("TRAMADOL" = 'green4', "HIDROCODONA" = 'green3', "CODEINA" = 'green1', "DIHIDROCODEINA" = 'green2',
          "FENTANILO"  = 'indianred4', "BUPRENORFINA" = 'indianred3', "OXICODONA"='indianred2', "TAPENTADOL"='indianred1',
          "MORFINA"='#6970ff', "HIDROMORFONA"='#6999ff', "METADONA"='#acc5fc', "MEPERIDINA"='#b5eeff')
attr(cols,"names") = attr(cols,"names") %>% str_to_title(.)


pdf(file = './RESULTADOS/INFORME/Costos/Costo_Dia_Tratamiento.pdf', width = 8,height = 6)
  ggplot(data2, 
       aes(x=ATC_LIMPIO,y = Costo_DDD, group = ATC_LIMPIO, fill = ATC_LIMPIO)) + 
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(position=position_jitter(width=.1, height=0)) + 
  coord_cartesian(ylim=c(-1000,45000)) +
  theme_bw() +
  facet_grid(cols = vars(Año)) + 
  scale_fill_manual('Fármaco', values = cols) +
  ggtitle('Costo por Día de Tratamiento') +
  labs(caption = "Tomado de Base de Datos de SISMED") +
  xlab('') + ylab('Costo Pesos por DDD') +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = c(0.85, 0.65), 
        legend.box.background = element_rect(fill = 'gray90'),
        legend.key = element_rect(fill = "gray90")) +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL), breaks =seq(0,50000,5000), minor_breaks = seq(0,50000,1000))
  dev.off()

# Resumen de Datos Agrupados
data2 %>% group_by(ATC_LIMPIO) %>% summarise(Q2 = quantile(Costo_DDD)[3],
                                             Q1 = quantile(Costo_DDD)[2], 
                                             Q3 = quantile(Costo_DDD)[4])

# Cálculo de FENTANILO suponiendo ATC para forma farmacéutica IV
data_aux = data_aux %>% 
  mutate('CUM-SEQ' = paste0(CUM,'-',SEQ)) %>% 
  inner_join(., REF_Tabla, by = 'CUM-SEQ') %>% 
  select(-c('Medicamento.y', 'Presentación', 'Código ATC', 'ATC.y', 'Principio Activo.y')) %>% 
  filter(!is.na(A)) %>% 
  rename('CUM_SEQ' = `CUM-SEQ`) %>% 
  filter(ATC_LIMPIO == 'FENTANILO')

data_aux = 
data_aux %>% 
  filter(., !grepl('TRANSD', Via_Admon)) %>%
  filter(., !grepl('NASAL', Via_Admon)) %>%
  filter(., !grepl('ORAL', Via_Admon)) %>%
  filter(., !grepl('PARCHE', Presentacion)) %>% 
  mutate(C = 0.6) %>% 
  mutate(DDD_T = (VCC_LAB_UND + VCI_LAB_UND)*A*B/C) %>%  # Cálculo DDD por VCC, y VCI
  mutate(Costos_T = (VCC_LAB_UND * VCC_LAB_MED) + (VCI_LAB_UND * VCI_LAB_MED)) %>% # Ventas en pesos colombianos VCC, y VCI
  mutate(Costo_DDD = ((VCC_LAB_UND*VCC_LAB_MED)+(VCI_LAB_UND*VCI_LAB_MED))/(DDD_T))

# Consumo por años para fentanilo parenteral
data_aux %>% 
  group_by(Año) %>% 
  summarise(DDD_T = sum(DDD_T), Ventas = sum(Costos_T)) %>% 
  inner_join(., CENSO_DATA, by = 'Año') %>% 
  mutate(Prop_Uso = DDD_T*100000/(Censo*365))

# Costos de tratamiento con fentanilo para administración parenteral en tiempo
data_aux %>% 
  group_by(Año) %>% 
  filter(Costo_DDD != 'NaN') %>% 
  summarise(Q1 = quantile(Costo_DDD)[2],
            Q2 = quantile(Costo_DDD)[3],
            Q3 = quantile(Costo_DDD)[4])
