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
  fechas_completas <- data.frame(fecha= seq(as.Date("2000-01-01"),as.Date("2024-01-01"),by="1 day"))
  data <- full_join(fechas_completas, dataframe, by=c("fecha"))
  return(data)
}

## 1.4 aplicar la función del 1.3 -------------------------
brent <- agregar_fechas_faltantes(brent)
gas_natural <- agregar_fechas_faltantes(gas_natural)
carbon <- agregar_fechas_faltantes(carbon)
gasolina <- agregar_fechas_faltantes(gasolina)

## 1.5 Union de las bases de datos -----------------------
df_unido <- left_join(brent, carbon, by='fecha') %>% 
             left_join(., gas_natural, by='fecha') %>% 
              left_join(., gasolina, by='fecha') 
  

## 1.6 añadir variables de año y mes con lubridate -----------------------
df_unido$mes <- month(df_unido$fecha, label=T)
df_unido$año <-year(df_unido$fecha)


## 1.7 tabla % NA y remplazo de NA -----------------------

#Tabal de NA
t(t(as.matrix(colMeans(is.na(df_unido)))))


df_unido$precio_carbon <- df_unido$precio_carbon %>% 
                          replace_na(mean(df_unido$precio_carbon,na.rm=T))

df_unido$precio_gas_natural <- df_unido$precio_gas_natural %>% 
                              replace_na(mean(df_unido$precio_gas_natural,na.rm=T))

df_unido$precio_gasolina <- df_unido$precio_gasolina %>% 
                               replace_na(mean(df_unido$precio_gasolina,na.rm=T))

df_unido$precio_petroleo <- df_unido$precio_petroleo %>% 
  replace_na(mean(df_unido$precio_petroleo,na.rm=T))



## 1.8 agrupar por año-mes  -----------------------
precios_mes <- df_unido %>% group_by(año,mes)  %>%
                summarise(carbon=mean(precio_carbon),
                          gas_natural = mean(precio_gas_natural),
                          petroleo = mean(precio_petroleo),
                          gasolina = mean(precio_gasolina))


