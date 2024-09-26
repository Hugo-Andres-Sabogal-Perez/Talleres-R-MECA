#Limpiaar entorno
rm(list = ls())

# 1.Importmos liberias -----------------

library(readr)

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

## 1.3. Explicitar fechas faltantes



brent[['precio_pretroleo']]