setwd(file.path('F:','Documentos','Mapas'))

library(maptools)
library(tidyverse)
library(lattice)
library(sp)
library(rgdal)
#El archivo .shp se puede bajar de http://www.gadm.org/country #
#xx <- readShapePoly("gadm36_COL_0.shp",IDvar="NAME_1", proj4string=CRS("+proj=longlat +ellps=clrk66"))

### Lectura d eDatos Topográficos
xx <- readOGR("gadm36_COL_1.shp") 
# 0, 1, y 2; muestra el mapa en casos de nación, departamentos, y municipios respectivamente

### Leer Datos Poblacionales
data <- read.csv("data_population.csv")


map <- fortify(xx)

map$id = factor(map$id, levels = c("0","1","2","3","4","5",
                                   "6","7","8","9","10",
                                   "11","12","13","14","15",
                                   "16","17","18","19","20",
                                   "21","22","23","24","25",
                                   "26","27","28","29","30","31"))


# col_mp <- c("0"="red", "1"="red", "2"="red", "3"="red", "4"="red", 
#             "5"="red", "6"="red", "7"="red", "8"="red", "9"="red", 
#             "10"="red", "11"="red", "12"="red", "13"="red", "14"="red", 
#             "15"="red", "16"="red", "17"="black", "18"="red", "19"="red", 
#             "20"="red", "21"="red", "22"="red", "23"="red", "24"="red", 
#             "25"="red", "26"="red", "27"="red", "28"="red", "29"="red", 
#             "30"="red", "31"="red")
# Para comprobar la identidad de cada departamento cambie en el mapa y coloque
# scale_fill_manual(values = col_mp)


data <- read.csv("data_population.csv")
data <- data %>% rename("id" = "ID")
data[,"id"] = factor(data$id, levels = c("0","1","2","3","4","5",
                                        "6","7","8","9","10",
                                        "11","12","13","14","15",
                                        "16","17","18","19","20",
                                        "21","22","23","24","25",
                                        "26","27","28","29","30","31"))

# Se le asigna al mapa las cantidades de las variables a representar.
mapx = full_join(map, data, by = "id")
# Tarea siguiente - Crear Percentiles


# Gráficos
gg <- ggplot()
gg <- gg + geom_map(data=mapx, map=mapx,
                    aes(x=long, y=lat, map_id=id, group=group, fill = Area),
                    color="#0e0e0e", size=0.15) + 
  scale_size(range = c(0, 8)) + 
  coord_map() + 
  theme_bw() + 
  labs(x=NULL, y=NULL) + 
  theme(panel.grid=element_blank()) + 
  theme(panel.border=element_rect()) +
  scale_fill_gradientn(colours = heat.colors(32))
gg