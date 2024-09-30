#Limpiaar entorno
rm(list = ls())

#Directorio 
setwd("C:/Users/Natalia/OneDrive - Universidad de los Andes/Documentos/2024-2/R/Talleres-R-MECA/Taller 2")

# 1.Importmos liberias -----------------

library(readr)
library(tidyverse)
library(stargazer)

# 1. Primer punto ----------------------------------------
## 1.1. importar bases de datos ------------------------------------------
carbon <- read_csv(unzip('Datos_taller.zip', 'precios_carbon.csv'))
brent <- read_csv(unzip('Datos_taller.zip', 'precios_petroleo.csv'))
gasolina <- read_csv(unzip('Datos_taller.zip', 'precios_gasolina.csv'))
gas_natural <- read_csv(unzip('Datos_taller.zip', 'precios_gas_natural.csv'))
ipc <- read.csv(unzip('Datos_taller.zip', '1.2.5.IPC_Serie_variaciones.csv'),  dec=",")


## 1.2. Exploración bases de datos ------------------------------------------

summary(carbon)
summary(brent)
summary(gas_natural)
summary(gasolina)
summary(ipc)

stargazer(as.data.frame(list(carbon, gasolina, gas_natural, brent)), summary=T, type='html', out="views/exploración.html")
stargazer(as.data.frame(ipc[,2:5]), summary=T, type='html', out="views/exploración_ipc.html")


## 1.3. agregar fechas faltantes --------------------------------------------

agregar_fechas_faltantes <- function(dataframe, columna_fecha) {
  #Se crea un dataframe con todas las fechas entre el primero de enero de 2001 y el primero de enero de 2024
  #Se asigna el nombre del argomento columna_fecha a la fecha en el dataframe "fechas_completas"
  fechas_completas <- data.frame(seq(as.Date("2000-01-01"),as.Date("2024-01-01"),by="1 day")) %>%
    setNames(columna_fecha)
  #se unen las bases de datos utilizando la columna_fecha como identificador único de las observaciones
  data <- full_join(fechas_completas, dataframe, by=columna_fecha)
  #retorna la base de datos con las fechas completas
  return(data)
  #Esta función va a servir sin importar el nombre de la columna de precios, por esta razón no se incluye ese parámetro
}


## 1.4 aplicar la función del 1.3 -------------------------
brent <- agregar_fechas_faltantes(brent, "fecha")
gas_natural <- agregar_fechas_faltantes(gas_natural, "fecha")
carbon <- agregar_fechas_faltantes(carbon, "fecha")
gasolina <- agregar_fechas_faltantes(gasolina, "fecha")

## 1.5 Union de las bases de datos -----------------------
df_unido <- left_join(brent, carbon, by='fecha') %>% 
             left_join(., gas_natural, by='fecha') %>% 
              left_join(., gasolina, by='fecha') 
  

## 1.6 añadir variables de año y mes con lubridate -----------------------
df_unido$mes <- month(df_unido$fecha, label=T)
df_unido$año <-year(df_unido$fecha)


## 1.7 tabla % NA y remplazo de NA -----------------------

#Tabla de NA
tabla_NA <- cbind(colMeans(is.na(df_unido[,2:5])*100), colSums(is.na(df_unido[,2:5])))
tabla_NA <- as.matrix(tabla_NA)
colnames(tabla_NA) <- c("Porcentaje", "N")
stargazer(tabla_NA, type="html", out="views/tabla_NAs.html")

#Remplazar NA
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

##### 1.9 Función IPC ------------------------

transform_precios_reales <- function(ind_ipc, año_base, mes_base, bien,datos){
  
  # Añadimos el IPC al datafrane (Se invierte dado que tienen orden opuesto de fechas)
  datos$ipc_mes <- rev(ipc[[ind_ipc]]) 
  
  #Filtramos por año y mes base
  precios_filtrados <- datos %>% filter(año==año_base, mes==mes_base)
  
  # LLamamos el dato de IPC base
  ipc_base <- precios_filtrados[['ipc_mes']]
  
  # Se crea el nombre de la variable
  nombre_variable <- paste0(bien,'_', año_base, '_', mes_base, '_', 'transformada')
  
  #Se crea la columna con el precio real del bien
  datos[[nombre_variable]] <- (datos[[bien]] * ipc_base)/datos[['ipc_mes']]
  
  # Se elimina la colummna de IPC
  datos <- datos %>% select(-ipc_mes)
  
  return(datos)
}

##### 1.10 aplicar función a todos los bienes ------------------------
precios_reales <- transform_precios_reales ('Indice', '2000', 'ene', 'petroleo', precios_mes)
precios_reales <- transform_precios_reales ('Indice', '2000', 'ene', 'carbon', precios_reales)
precios_reales <- transform_precios_reales ('Indice', '2000', 'ene', 'gasolina', precios_reales)
precios_reales <- transform_precios_reales ('Indice', '2000', 'ene', 'gas_natural',precios_reales)

precios_reales$ipc_mes <- rev(ipc[["Indice"]]) 


##### 1.10 exportar bases de datos ------------------------

write.csv(precios_reales, 'precios nomimales y constantes(ene 2000).csv')



### Punto 2 -----------------------

### 2.1 tabla estadisticas descriptivas 

precios_mes <- ungroup(precios_mes)

precios <- ungroup(precios_reales) %>% select(-año, -mes)

stargazer(as.data.frame(precios_reales), summary=T, type='html', out="views/sum_precios_reales.html")

### 2.2 Scatter plot ----------------------------


scatter_plot <- ggplot(precios, aes(x = carbon_2000_ene_transformada, y = gasolina_2000_ene_transformada)) +
  geom_point(color= 'royalblue')+
  geom_smooth(method='lm', color='firebrick') +
  theme_bw() +
  labs(x = "Precio real del Carbón", y = "Precio real de la Gasolina", 
       title='Precios reales (base enero del 2000): Carbón vs Gasolina') 

scatter_plot

ggsave("Views/scatter.png", width = 6, height = 4, plot = scatter_plot)

### 2.2 Grafica serie de tiempo ----------------------------

#Precios reales

precios$fecha= seq(as.Date("2000-01-01"),as.Date("2024-01-01"),by="1 month")

serie_tiempo<-ggplot(data=precios)  +
  geom_line(aes(x = fecha, y = carbon_2000_ene_transformada, color='Carbon')) +
  geom_line(aes(x = fecha, y = gasolina_2000_ene_transformada, color='Gasolina')) + 
  geom_line(aes(x = fecha, y = gas_natural_2000_ene_transformada, color='Gas Natural')) + 
  geom_line(aes(x = fecha, y = petroleo_2000_ene_transformada, color='Petroleo')) + 
  scale_color_manual(values = c('aquamarine3', 'chartreuse3', 'royalblue3', 'darkblue'),
                     limits = c('Carbon', 'Gasolina', 'Gas Natural', 'Petroleo'),
                     labels = c('Carbon', 'Gasolina', 'Gas Natural', 'Petroleo'),
                     guide = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  labs(x='Fecha', y='Precios reales (base: enero del 2000) ',  title='Precios reales históricos de los combustibles (2000-2024)', color='Bien') +
  theme(axis.title = element_text(size = 14))


serie_tiempo

ggsave('views/serie_precios.png',serie_tiempo, dpi=300,width = 8, height = 6) 

#Precios nominales 

serie_tiempo2<-ggplot(data=precios)  +
  geom_line(aes(x = fecha, y = carbon, color='Carbon')) +
  geom_line(aes(x = fecha, y = gasolina, color='Gasolina')) + 
  geom_line(aes(x = fecha, y = gas_natural, color='Gas Natural')) + 
  geom_line(aes(x = fecha, y = petroleo, color='Petroleo')) + 
  scale_color_manual(values = c('aquamarine3', 'chartreuse3', 'royalblue3', 'darkblue'),
                     limits = c('Carbon', 'Gasolina', 'Gas Natural', 'Petroleo'),
                     labels = c('Carbon', 'Gasolina', 'Gas Natural', 'Petroleo'),
                     guide = guide_legend(override.aes = list(size = 4))) +
  theme_bw() +
  labs(x='Fecha', y='Precios nominales ', color='Bien') +
  theme(axis.title = element_text(size = 14))
serie_tiempo2
