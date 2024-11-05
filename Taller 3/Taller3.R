#0.1. Librerías ------------------------------------

library(dplyr)
library(sf)
library(openxlsx)

#0.2. Directorio -----------------------------------

setwd("C:/Users/Natalia/OneDrive - Universidad de los Andes/Documentos/2024-2/R/Talleres-R-MECA/Taller 3")

#1.1. Cargar shapefile -----------------------------

shapefile_col <- st_read("Datos/Shapefile/MGN_ADM_MPIO_GRAFICO.shp")
shapefile_col <- Colombia %>% subset(dpto_cnmbr != "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA")

#1.2. Cargar Panel CEDE ----------------------------

panel_cede <- read.xlsx("Datos/CEDE/PANEL_CARACTERISTICAS_GENERALES(2022).xlsx")
panel_cede <- panel_cede %>% select(contains("cod") | contains("pob"), )

#1.3. Unir bases -----------------------------------

df_completo <- shapefile_col %>%