# Librerías utilizadas
library(tidyverse)
library(highcharter)
library(lubridate)

# Lectura de datos
#----------------------------
#datos de cada estación meteorológica: id, long, lat, nombre, ciudad, país, etc. Cada estación corresponde a un lugar geográfico.
estaciones <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/estaciones.csv")
locale = readr::locale(encoding = "latin1")
View(estaciones)
#datos meteorologicos de cada estación:t_min, t_max, precipitación,etc.
meteo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/meteo.csv", na = "-99.9")
View(meteo)

#----------------------------
# Procesamiento de datos
#----------------------------
# uno la tabla meteo con estacion mediane el id_estacion
estaciones_meteo <-meteo %>% inner_join(estaciones, by= 'id_estacion')
View(estaciones_meteo)

estaciones_meteo2 <- select(estaciones_meteo, -elevacion, -institucion)%>% filter(pais=='Argentina')%>%
  mutate(fecha = as.Date(fecha), media= ((t_max+t_min)/2)) %>%
  rename(Fecha = fecha)
View(estaciones_meteo2)
#elimino nulos
#estacionesSinNA <-estaciones_meteo2 %>%filter (!is.na(t_max) & !is.na(t_min))  #filter(pais=='Argentina')

#selecciono datos de Argentina del año 2011
mapaA2011 <- estaciones_meteo2%>% filter((year(Fecha)== 2011) & !is.na(t_max) & !is.na(t_min))%>% select(lat, lon,t_max, t_min, media, Fecha, nombre)
View(mapaA2011)

#Datos
hchartdatos <- mapaA2011 %>% select(t_min, t_max, media, Fecha, nombre)%>% mutate(dia=day(fechaGrafico))%>%filter(nombre=='CORRIENTES AERO')
View(hchartdatos)

#highcharter
x <- c("Día","T.Min.", "T.Media", "T.Máx.")
y <- sprintf("{point.%s}", c("dia","t_min", "media", "t_max"))
tltip <- tooltip_table(x, y)

#opción 1: theme monokai
#------------------------
graficoMono <-hchart(hchartdatos, type = "columnrange",
                  hcaes(x = Fecha, low = t_min, high = t_max, color = media)) %>% 
  hc_yAxis(tickPositions = c(-5, 0, 5.0, 10.0,15.0,20.0,25.0,30.0,35.0,40.0, 45.0),
           gridLineColor = "orange",               #B71C1C
           labels = list(format = "{value} Cº", useHTML = TRUE)) %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = as.character(tags$small("{point.x: %Y %b}")),
    pointFormat = tltip
  ) %>% 
  hc_add_theme(hc_theme_monokai())

graficoMono %>% hc_title(text = "Temperaturas máxima y mínima de la ciudad de Corrientes <br /> <br /> Para el período  comprendido entre el 01/01/2011 y el 01/01/2012.") %>% 
  hc_subtitle(text = "La temperatura mínima con -0.4 Cº se registró el 27 de junio, mientras que la máxima con 40.7 Cº se registró el 22 de diciembre. <br /> <br /> Fuente: Claris LPB.")

#------------------------
# opción 2: theme ffx
#------------------------
#highcharter
x <- c("Día","T.Min.", "T.Media", "T.Máx.")
y <- sprintf("{point.%s}", c("dia","t_min", "media", "t_max"))
tltip <- tooltip_table(x, y)


grafico2 <-hchart(hchartdatos, type = "columnrange",
                 hcaes(x = Fecha, low = t_min, high = t_max, color = media)) %>% 
  hc_yAxis(tickPositions = c(-5, 0, 5.0, 10.0,15.0,20.0,25.0,30.0,35.0,40.0, 45.0),
           #gridLineColor = "orange",              
           labels = list(format = "{value} Cº", useHTML = TRUE)) %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = as.character(tags$small("{point.x: %Y %b}")),
    pointFormat = tltip
  ) %>% 
  hc_add_theme(hc_theme_ffx())

grafico2 %>% hc_title(text = "Temperaturas máxima y mínima de la ciudad de Corrientes <br /> <br /> Para el período  comprendido entre el 01/01/2011 y el 01/01/2012.") %>% 
  hc_subtitle(text = "La temperatura mínima con -0.4 Cº se registró el 27 de junio, mientras que la máxima con 40.7 Cº se registró el 22 de diciembre. <br /> <br /> Fuente: Claris LPB.")


#--------------------------
#opción 3: theme flatdark
#--------------------------
#highcharter
x <- c("Día","T.Min.", "T.Media", "T.Máx.")
y <- sprintf("{point.%s}", c("dia","t_min", "media", "t_max"))
tltip <- tooltip_table(x, y)


grafico3 <-hchart(hchartdatos, type = "columnrange",
                  hcaes(x = Fecha, low = t_min, high = t_max, color = media)) %>% 
  hc_yAxis(tickPositions = c(-5, 0, 5.0, 10.0,15.0,20.0,25.0,30.0,35.0,40.0, 45.0),
           gridLineColor = "orange",               #B71C1C
           labels = list(format = "{value} Cº", useHTML = TRUE)) %>% 
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = as.character(tags$small("{point.x: %Y %b}")),
    pointFormat = tltip
  ) %>% 
  hc_add_theme(hc_theme_flatdark())

grafico3 %>% hc_title(text = "Temperaturas máxima y mínima de la ciudad de Corrientes <br /> <br /> Para el período  comprendido entre el 01/01/2011 y el 01/01/2012.") %>% 
  hc_subtitle(text = "La temperatura mínima con -0.4 Cº se registró el 27 de junio, mientras que la máxima con 40.7 Cº se registró el 22 de diciembre. <br /> <br /> Fuente: Claris LPB.")

#--------------------------------------------------------------------------------