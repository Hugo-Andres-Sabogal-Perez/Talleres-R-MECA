#0.1. Librerías ------------------------------------

library(dplyr)
library(readr)

#0.2. Escritorio -----------------------------------

setwd("C:/Users/Natalia/OneDrive - Universidad de los Andes/Documentos/2024-2/R/Talleres/Taller 2")
      
#1.1. Importar datos -------------------------------

ipc <- read_csv("Datos_taller/1.2.5.IPC_Serie_variaciones.csv")
carbon <- read_csv("Datos_taller/precios_carbon.csv")
gas <- read_csv("Datos_taller/precios_gas_natural.csv")
gasolina <- read_csv("Datos_taller/precios_gasolina.csv")
petroleo <- read_csv("Datos_taller/precios_petroleo.csv")

#1.3. Función para agregar fehcas faltantes

agregar_fechas_faltantes <- function(dataframe) {
  fechas_completas <- data.frame(fecha= seq(as.Date("2000-01-01"),as.Date("2024-01-31"),by="1 day"))
  data <- full_join(fechas_completas, dataframe, by=c("fecha"))
  return(data)
}

gasolina2 <- agregar_fechas_faltantes(gasolina)

