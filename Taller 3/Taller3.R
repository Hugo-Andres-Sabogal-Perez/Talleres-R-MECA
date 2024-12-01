#Taller 3 
rm(list = ls())

#0.1. Librerías -------------------------------------

install.packages('pacman')

library(pacman)

p_load(
  tidyverse, sf, openxlsx, plotly, rvest, tidytext, rebus,
  tidyr, tm, wordcloud, stringr, ggplot2, htmltools
)

#0.2. Directorio ------------------------------------

# setwd("C:/Users/Natalia/OneDrive - Universidad de los Andes/Documentos/2024-2/R/Talleres-R-MECA/Taller 3")

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

##1.4. Mapa interactivo -----------------------------

# Seleccionar los datos de 2009 para el mapa
datos_mapa <- subset(df_completo, df_completo$ano == 2009)

# Crear una nueva columna para el texto que debe aparecer al seleccionar los municipios en el mapa interactivo
datos_mapa$tooltip_text <- paste("Departamento:", datos_mapa$dpto_cnmbr, "<br>",
                                 "Municipio:", datos_mapa$mpio_cnmbr, "<br>",
                                 "Población Rural:", scales::comma(datos_mapa$pobl_rur))

# Crear el mapa con ggplot
mapa <- ggplot(data = datos_mapa) +
  geom_sf(aes(fill = pobl_rur, 
              # Asignar la columna creada al tooltip
              text = tooltip_text), 
          size = 0.1) +  # Color y borde de los municipios
  scale_fill_viridis_c(name = "Población Rural", option = "viridis", labels = scales::comma) +  # Escala de color
  labs(title = "Mapa de Población Rural por Municipio (2009)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"))

mapa

# Convertir el mapa en un mapa interactivo utilizando ggplotly 
mapa_interactivo <- ggplotly(mapa, tooltip = "text") #Tooltip especifica el texto que aparece al seleccionar los municipios

mapa_interactivo

save_html(mapa_interactivo, "mapa_interactivo.html")

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
  nombres_libros <- link %>% 
    read_html() %>%
    html_nodes("article h3 a") %>%
    html_attr("title")
  
  #Precios de los libros
  precio_libros <- link %>%
    read_html() %>%
    html_nodes(".price_color") %>%
    html_text()
  
  #Crear base de datos
  libros_df <- rbind(libros_df, data.frame(
    Titulo = nombres_libros,
    Precio = precio_libros,
    stringsAsFactors = FALSE))
  
  #Registro de progreso
  print(paste("Página:", page))
}

##2.3. Linpieza de texto -----------------------------

corpus <- Corpus(VectorSource(libros_df$Titulo))

# Procesamos el texto 
corpus <- tm_map(corpus, content_transformer(tolower))  # Convertir a minúsculas
corpus <- tm_map(corpus, removePunctuation)             # Eliminar puntuación
corpus <- tm_map(corpus, removeNumbers)                 # Eliminar números
corpus <- tm_map(corpus, removeWords, stopwords("english"))  # Eliminar palabras vacías (stop words)
corpus <- tm_map(corpus, stripWhitespace)   

# 2.4 Base con texto procesado -----------------------
datos_limpios <-  data.frame(titulo = sapply(corpus, as.character))

# 2.7 matriz term frecuency---------------------------

#De acuerdo con la aclaración que hizo Daniel en clase,
#esto se debe hacer antes la nube de palabras

dtm <- TermDocumentMatrix(corpus)

# Aca ya lo puedo ver. 
tdm <- as.matrix(dtm)
tdm <- t(tdm)

# 2.5  creamos base count ----------------------------
count <- data.frame(colSums(tdm))

# 2.6 nube de palabras -------------------------------
wordcloud(
  words = rownames(count), 
  freq = count$colSums.tdm., 
  min.freq = 2,            # Mínima frecuencia de palabras a incluir
  max.words = 100,           # Máximo número de palabras en la nube
  random.order = FALSE,     # Arrange words by frequency
  colors = brewer.pal(1, "Dark2"),  # Paleta de colores
  scale =c(4,0.6)
)

# 2.8 Añadir columna de precio------------------------

datos_limpios <- data.frame(datos_limpios, libros_df$Precio)

# 2.9  Seleccionar 10 palabras------------------------

# Filtrar las palabras con frecuencia mayor a 10 para escoger las palabras
palabras_frecuentes <- tdm[, colSums(tdm) > 10]

# Escoger 10 palabras aleatorias dentro de las columnas de esta nueva base
set.seed(72)
nombres <- sample(colnames(palabras_frecuentes), 10)

# Seleccionar las columnas correspondientes a las 10 palabras aleatorias
matriz10 <- palabras_frecuentes[, nombres]

# Reemplazar valores mayores a 1 por uno para tener variables dummy
matriz10[matriz10 > 1] <- 1

#Agregar las palabras a datos limpios
datos_limpios <- data.frame(datos_limpios, matriz10)

# 2.10. Agrupar precio por palabras -------------------

# Primero, se aseguran de que los precios estén en formato numérico para poder hacer cálculos
names(datos_limpios)[names(datos_limpios) == "libros_df.Precio"] <- "Precio"
datos_limpios$Precio <- str_remove_all(datos_limpios$Precio, "£")
datos_limpios$Precio <- as.numeric(datos_limpios$Precio)

# Agrupamos por palabra y calculamos el promedio del precio

resultados <- data.frame()

# Iterar sobre cada palabra
for (palabra in nombres) {
  # Convertir el nombre de la palabra en símbolo para usarlo en group_by
  columna_palabra <- ensym(palabra)
  
  # Agrupar por si contiene o no la palabra y calcular el promedio de precio
  resultado <- datos_limpios %>%
    mutate(Contiene = ifelse(grepl(palabra, titulo), 1, 0)) %>%
    group_by(Contiene) %>%
    summarise(Promedio_Precio = mean(Precio, na.rm = TRUE)) %>%
    mutate(Palabra = palabra) %>%
    select(Palabra, Contiene, Promedio_Precio)
  
  # Agregar el resultado al dataframe final
  resultados <- bind_rows(resultados, resultado)
}

# 2.11. Visualización ---------------------------------
resultados <- resultados %>%
  mutate(Contiene = ifelse(Contiene == 1, "Si", "No"))

ggplot(resultados, aes(x = Palabra, y = Promedio_Precio, fill = Contiene)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 1.1) +  
  labs(title = "Precio promedio de libros según \n palabras en el título",
       x = "Palabra",
       y = "Precio promedio",
       fill = "Contiene la palabra") +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  coord_flip()
