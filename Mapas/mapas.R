library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

library("tmap")    # for static and interactive maps
library("leaflet") # for interactive maps
library("mapview") # for interactive maps


tm_shape(co) +
  tm_fill() +
  tm_borders() 


install.packages("devtools")
devtools::install_github("nebulae-co/colmaps")

require(tidyverse)

library("colmaps")
colmap(municipios) +
  ggtitle("Colombia - Fronteras Municipales")


colmap(departamentos)


library("homicidios")

colmap(municipios, subset(homicidios, año == 2000), var = "tasa")



homicidios %>%
  filter(año == 2000) %>%
  mutate(log_tasa = log1p(tasa)) %>%
  colmap(municipios, data = ., var = "log_tasa") +
  scale_fill_continuous(low = "#bcbddc", high = "#3f007d", na.value = "wheat")

homicidios %>%
  filter(año == 2000) %>%
  group_by(id_depto) %>%
  summarise(tasa = 10^5 * sum(homicidios, na.rm = TRUE) /
              sum(poblacion, na.rm = TRUE)) %>%
  colmap(departamentos, data = ., data_id = "id_depto")


homicidios %>%
  filter(año >= 2008) %>% 
  colmap(map = municipios, data = ., var = "tasa")
