#Limpiaar entorno
rm(list = ls())

# 1.Importmos liberias -----------------

library(readr)
library(tidyverse)

# 1. Primer punto ----------------------------------------
## 1.1. importar bases de datos ------------------------------------------
carbon <- read_csv(unzip('Datos_taller.zip', 'precios_carbon.csv'))
brent <- read_csv(unzip('Datos_taller.zip', 'precios_petroleo.csv'))
gasolina <- read_csv(unzip('Datos_taller.zip', 'precios_gasolina.csv'))
gas_natural <- read_csv(unzip('Datos_taller.zip', 'precios_gas_natural.csv'))
ipc <- read_csv(unzip('Datos_taller.zip', '1.2.5.IPC_Serie_variaciones.csv'))


## 1.2. Exploración bases de datos ------------------------------------------
summary(carbon)
summary(brent)
summary(gasolina)
summary(gas_natural)
summary(ipc)

## 1.3. agregar fechas faltantes -------------------

agregar_fechas_faltantes <- function(dataframe) {
  fechas_completas <- data.frame(fecha= seq(as.Date("2000-01-01"),as.Date("2024-01-31"),by="1 day"))
  data <- full_join(fechas_completas, dataframe, by=c("fecha"))
  return(data)
}

## 1.4 aplicar la función del 1.3 -------------------------
brent <- agregar_fechas_faltantes(brent)
gas_natural <- agregar_fechas_faltantes(gas_natural)
carbon <- agregar_fechas_faltantes(carbon)
gasolina <- agregar_fechas_faltantes(gasolina)

## 1.5 Union de las bases de datos

df_unido <- 

