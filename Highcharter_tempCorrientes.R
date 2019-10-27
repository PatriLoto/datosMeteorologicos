library(tidyverse)
library(highcharter)
library(lubridate)


#Datos
hchartdatos <- mapaA2011 %>% select(t_min, t_max, media, fechaGrafico, nombre)%>% mutate(Fecha=fechaGrafico, dia=day(fechaGrafico))%>%filter(nombre=='CORRIENTES AERO')
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