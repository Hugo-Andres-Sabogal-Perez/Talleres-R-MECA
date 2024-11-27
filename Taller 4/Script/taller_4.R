#Taller 4 - R MECA ------------------------------------------------
rm(list = ls())

## 0.1. Librerias -------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library("pacman")
p_load(sf)

## 0.2. Directorio ------------------------------------------------
# Obtener el nombre de usuario del sistema operativo
usuario <- Sys.info()["user"]

# Crear el directorio condicional dependiendo del usuario
if (usuario == "Natalia") {
  setwd("C:/Users/Natalia/OneDrive - Universidad de los Andes/Documentos/2024-2/R/Talleres-R-MECA/Taller 4")
} else if (usuario == "hugos") {
  setwd("") 
}

#Punto 1-----------------------------------------------------------

## 1.2. Cargar Shapefile ------------------------------------------

mundo <- st_read("shapefile/geoBoundariesCGAZ_ADM0.shp")

## 1.7. Datos de WITS ---------------------------------------------

exportaciones <- read.csv("Exports/DataJobID-2761539_2761539_ExportsCOL.csv")

## 1.8. Procesamiento de datos ------------------------------------

exportaciones_col <- exportaciones %>% subset(ReporterISO3 == "COL") %>%
  select(PartnerISO3, )

