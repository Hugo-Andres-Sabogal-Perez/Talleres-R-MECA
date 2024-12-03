#Taller 4 - R MECA ------------------------------------------------
rm(list = ls())

## 0.1. Librerias -------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library("pacman")
p_load(sf, tidyverse, fastDummies, ggplot2, plotly, stringr, 
       rmapshaper, dplyr, shiny)

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

#Cambiar el nombre de la variable de ISO3 en mundo para facilitar la unión
mundo <- mundo %>% rename(PartnerISO3 = shapeGroup)

#Mantener solo las exportaciones de colombia y seleccionar columnas relevantes
exportaciones_col <- exportaciones %>% subset(ReporterISO3 == "COL") %>% #Mantenemos las
  select(PartnerISO3, PartnerName, TradeValue.in.1000.USD, ProductDescription,
         ProductCode) %>%
  rename(TradeValue1000USD = TradeValue.in.1000.USD) %>%
  dummy_cols(select_columns = "ProductCode", remove_first_dummy = FALSE)

#Traducir categorías de productos para la visualización
translations <- c(
  "ORES AND MINERALS; ELECTRICITY, GAS" = "Minería, electricidad y gas",
  "FOOD PRODUCTS, BEVERAGES AND TOBACC" = "Alimentos, bebidas y tabaco",
  "OTHER TRANSPORTABLE GOODS, EXCEPT M" = "Otros bienes transportables",
  "METAL PRODUCTS, MACHINERY AND EQUIP" = "Productos metálicos y maquinaria",
  "AGRICULTURE, FORESTRY AND FISHERY P" = "Agricultura, silvicultura y pesca"
)

exportaciones_col <- exportaciones_col %>%
  mutate(ProductDescEsp = translations[ProductDescription])

# Calcular quintiles de exportación para la visualización 
quintiles <- quantile(exportaciones_col$TradeValue1000USD, probs = seq(0, 1, by = 0.2))

exportaciones_col$TradeValueCateg <- cut(
  exportaciones_col$TradeValue1000USD,
  breaks = quintiles,
  include.lowest = TRUE,  # Incluye el valor mínimo en el primer intervalo
  labels = paste0(
    round(quintiles[-length(quintiles)], 0), 
    " - ", 
    round(quintiles[-1], 0)))

#Unir datos de exportaciones con shapefile
exportaciones_col <- exportaciones_col %>% left_join(mundo, by="PartnerISO3")

## 1.10. Mapa de exportaciones con grilla por tipo de produco ---------------
#Crear el mapa base
mapa_base <- ggplot(exportaciones_col) + 
  geom_sf(aes(fill = TradeValueCateg, geometry = geometry)) + 
  scale_fill_brewer(palette = "Set2", 
                    name = "Quitiles de exportaciones (miles de USD)",
                    na.value = "grey90") +
  facet_wrap(~ ProductDescEsp, ncol = 3) + 
  labs(title = "Exportaciones de Colombia por Categorias de Productos",
       caption = "Fuente: Elaboración propia con datos del World Integrated Trade System (WITS)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8), # Ajusta el tamaño del texto en las facetas
    axis.text.x = element_text(size = 4),    # Etiquetas del eje X más pequeñas
    axis.text.y = element_text(size = 4),     # Etiquetas del eje Y más pequeñas
    legend.title = element_text(size = 8),    # Título de la leyenda más pequeño
    legend.text = element_text(size = 7),     #Texto de la leyenda más pequeño
    panel.spacing = unit(0.5, "lines"),        # Espaciado entre facetas
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Título centrado
  ) 


mapa_interactivo <- ggplotly(mapa_base)

mapa_interactivo

#Mapa para poner en el documento 
mapa_ajustado <- mapa_base + 
  theme(legend.position = "bottom",   # Posición de la leyenda
        legend.direction = "horizontal")+
  guides(
    fill = guide_legend(
      ncol = 3,                             # Número de columnas en la leyenda
      byrow = TRUE                          # Ordenar la leyenda por filas
    )
  )

ggsave(
  filename = "mapa_exportaciones.png", # File name and format
  plot = mapa_ajustado,              # The plot object to save
  width = 10,                         # Width of the image in inches
  height = 8,                         # Height of the image in inches
  dpi = 300                           # Resolution in dots per inch
)

## 1.11. Crear Shiny ----------------------------------------------------

# UI
ui <- fluidPage(
  titlePanel("Exportaciones de Colombia por Categoría de Producto"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_product",
        label = "Selecciona una categoría de producto:",
        choices = unique(exportaciones_col$ProductDescEsp),
        selected = unique(exportaciones_col$ProductDescEsp)[1]
      )
    ),
    
    mainPanel(
      plotlyOutput("export_map")
    )
  )
)

# Server
server <- function(input, output) {
  output$export_map <- renderPlotly({
    # Filter the data based on the selected product
    filtered_data <- exportaciones_col %>%
      filter(ProductDescEsp == input$selected_product)
    
    # Create the map
    mapa_base <- ggplot(filtered_data) +
      geom_sf(aes(
        fill = TradeValueCateg,
        geometry = geometry)) +
      scale_fill_brewer(
        palette = "Set2",
        name = "Quintiles de exportaciones (miles de USD)",
        na.value = "grey90"
      ) +
      labs(
        title = paste("Exportaciones de Colombia:", input$selected_product),
        caption = "Fuente: Elaboración propia con datos del WITS"
      ) +
      theme_minimal()
    
    # Convert the ggplot object to an interactive plot
    ggplotly(mapa_base)
  })
}

# Run the app
shinyApp(ui, server)

#Guardar App
# Automatically save the UI and server to separate R files
capture.output(cat(deparse(ui), sep = "\n"), file = "App/ui.R")
capture.output(cat(deparse(server), sep = "\n"), file = "App/server.R")
saveRDS(exportaciones_col, file = "App/exportaciones_col.rds")

#Miau
