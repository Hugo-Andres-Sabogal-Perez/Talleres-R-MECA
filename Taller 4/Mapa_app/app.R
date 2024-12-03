
#librerías

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library("pacman")
p_load(sf, tidyverse, ggplot2, plotly, stringr, shiny, here)

# Cargar los datos desde el archivo .rds
exportaciones_col <- readRDS(here("exportaciones_col.rds"))

#UI
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

#Run App
shinyApp(ui, server)