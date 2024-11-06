rm(list = ls())

#0.1. Librerías ------------------------------------

library(dplyr)
library(sf)
library(openxlsx)
library(ggplot2)
library(plotly)

#0.2. Directorio -----------------------------------

setwd("C:/Users/Natalia/OneDrive - Universidad de los Andes/Documentos/2024-2/R/Talleres-R-MECA/Taller 3")

#1.1. Cargar shapefile -----------------------------

shapefile_col <- st_read("Datos/Shapefile/MGN_ADM_MPIO_GRAFICO.shp")
shapefile_col <- shapefile_col %>% subset(dpto_cnmbr != 
  "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA") %>%
  rename(codmpio = mpio_cdpmp)

#1.2. Cargar Panel CEDE ----------------------------
  
panel_cede <- read.xlsx("Datos/CEDE/PANEL_CARACTERISTICAS_GENERALES(2022).xlsx")
panel_cede <- panel_cede %>% select(contains("cod") | contains("pob"), 
  depto, provincia, municipio, ano, ao_crea) %>% 
  mutate(codmpio = sprintf("%05d", codmpio))

#1.3. Unir bases -----------------------------------

df_completo <- shapefile_col %>% left_join(panel_cede, by = "codmpio") 

#Aquí perdí 100 observaciones pero no sé por qué (Pueden ser San Andrés y Providencia del panel CEDE?)

#1.4. Mapa interactivo -----------------------------

datos_mapa <- subset(df_completo, df_completo$ano == 2009)

mapa <- ggplot(data = datos_mapa) +
  geom_sf(aes(fill = pobl_rur), color = "white", size = 0.2) +  # Color y borde de los municipios
  scale_fill_viridis_c(name = "Población Rural", option = "viridis", labels = scales::comma) +  # Escala de color
  labs(title = "Mapa de Población Rural por Municipio (2009)", 
       subtitle = "Distribución de la población rural a nivel municipal",
       caption = "Fuente: CEDE") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

mapa_interactivo <- ggplotly(mapa)

mapa_interactivo