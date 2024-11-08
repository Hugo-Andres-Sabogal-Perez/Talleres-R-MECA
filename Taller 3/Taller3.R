#Taller 3 
rm(list = ls())

#0.1. Librerías -------------------------------------

library(tidyverse)
library(sf)
library(openxlsx)
library(plotly)
library(rvest)
library(tidytext)
library(rebus)
library(tidyr)


#0.2. Directorio ------------------------------------

setwd("C:/Users/hugos/OneDrive - Universidad de los andes/MECA/Semestre 1/taller R/Talleres-R-MECA/Taller 3")

# Punto 1 -------------------------------------------

##1.1. Cargar shapefile -----------------------------

shapefile_col <- st_read("Datos/Shapefile/MGN_ADM_MPIO_GRAFICO.shp")
shapefile_col <- shapefile_col %>% subset(dpto_cnmbr != 
  "ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA") %>%
  rename(codmpio = mpio_cdpmp)

##1.2. Cargar Panel CEDE ----------------------------
  
panel_cede <- read.xlsx("Datos/CEDE/PANEL_CARACTERISTICAS_GENERALES(2022).xlsx")
panel_cede <- panel_cede %>% select(contains("cod") | contains("pob"), 
  depto, provincia, municipio, ano, ao_crea) %>% 
  mutate(codmpio = sprintf("%05d", codmpio))

##1.3. Unir bases -----------------------------------

df_completo <- shapefile_col %>% left_join(panel_cede, by = "codmpio") 

#Aquí perdí 100 observaciones pero no sé por qué (Pueden ser San Andrés y Providencia del panel CEDE?)

##1.4. Mapa interactivo -----------------------------

datos_mapa <- subset(df_completo, df_completo$ano == 2009)

mapa <- ggplot(data = datos_mapa) +
  geom_sf(aes(fill = pobl_rur), size = 0.1) +  # Color y borde de los municipios
  scale_fill_viridis_c(name = "Población Rural", option = "viridis", labels = scales::comma) +  # Escala de color
  labs(title = "Mapa de Población Rural por Municipio (2009)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom")



mapa

mapa_interactivo <- ggplotly(mapa)

mapa_interactivo 

# Punto 2 --------------------------------------------

rm(list = ls())

##2.1. Books to scrape -------------------------------

books <- "https://books.toscrape.com"

##2.2. Extraer información ---------------------------

libros_df <- data.frame()

for(page in 1:50) {
  #Link
  link <- paste0(books, "/catalogue/page-", page, ".html")
  
  #Nombres de los libros
  nombres_libros <- books %>% 
    read_html() %>%
    html_nodes("article h3 a") %>%
    html_attr("title")
  
  #Precios de los libros
  precio_libros <- books %>%
    read_html() %>%
    html_nodes(".price_color") %>%
    html_text()
  
  #Crear base de datos
  libros_df <- rbind(libros_df, data.frame(
    Titulo = nombres_libros,
    Precio = precio_libros,
    stringsAsFactors = FALSE))
  
  #Registro de progreso
  print(paste("Pagina:", page))
}

##2.3. remover stop words, minusculas, eliminación de caracteres especiales y numeros -------

corpus <- Corpus(VectorSource(libros_df$Titulo))

# procesamos el texto 
corpus <- tm_map(corpus, content_transformer(tolower))  # Convertir a minúsculas
corpus <- tm_map(corpus, removePunctuation)             # Eliminar puntuación
corpus <- tm_map(corpus, removeNumbers)                 # Eliminar números
corpus <- tm_map(corpus, removeWords, stopwords("english"))  # Eliminar palabras vacías (stop words)
corpus <- tm_map(corpus, stripWhitespace)   

# 2.4 creamos base con texto procesado (datos limpios) ---------------
datos_limpios <-  data.frame(titulo = sapply(corpus, as.character))

# 2.5  creamos base count -----------------
count <- data.frame(table(datos_limpios))

# 2.6 nube de palabras ----------------
wordcloud(
  words = count$titulo, 
  freq = count$Freq, 
  min.freq = 1,            # Minimum frequency to include words
  max.words = 100,           # Maximum number of words
  random.order = FALSE,     # Arrange words by frequency
  colors = brewer.pal(8, "Dark2"),  # Set color palette
  scale =c(0.6,2)
)


# 2.7 matriz term frecuency
dtm <- TermDocumentMatrix(corpus)
# Aca ya lo puedo ver. 
tdm <- as.matrix(dtm)


