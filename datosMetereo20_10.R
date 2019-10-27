
library(tidyverse)
library(highcharter)
library(lubridate)
library(tseries)


#cada estación corresponde a un lugar geográfico
estaciones <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/estaciones.csv")
locale = readr::locale(encoding = "latin1")
View(estaciones)

meteo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-10-09/meteo.csv", na = "-99.9")
View(meteo)
#--------------------------------------------------
porAnio <-meteo%>% mutate(anio=year(fecha))%>%View


# uno la tabla meteo con estacion medainte el id_estacion
estaciones_meteo <-meteo %>% inner_join(estaciones, by= 'id_estacion')
View(estaciones_meteo)

estaciones_meteo2 <- select(estaciones_meteo, -elevacion, -institucion)%>%
mutate(fecha = as.Date(fecha), media= ((t_max+t_min)/2)) %>%
  rename(fechaGrafico = fecha)
View(estaciones_meteo2)
#elimino nulos
estacionesSinNA <-estaciones_meteo2 %>%filter (!is.na(t_max) & !is.na(t_min))

#selecciono datos de Argentina del año 2011
mapaA2011 <- estacionesSinNA%>% filter(pais=='Argentina'& (year(fechaGrafico)== 2011)& !is.na(t_max) & !is.na(t_min))%>% select(lat, lon,t_max, t_min, media, fechaGrafico, nombre)
View(mapaA2011)

Paraheatmap <- mapaA2011 %>% select(t_max, fechaGrafico)%>% mutate(mes=month(mapaA2011$fechaGrafico))
View(Paraheatmap)
Paraheatmap2 <- Paraheatmap %>% summarize(prom=mean(t_max))%>% group_by(Paraheatmap$mes)

heatmap(mapaA2011$t_max, scale = "row")

# mapa de todos los países de todos los años
mapa <- estaciones_meteo2%>%select (everything(), -nombre, -provincia)

#selecciono los datos de Corrientes
mapaCorrientes <- estaciones_meteo2 %>% filter(id_estacion==10470 & !is.na(t_max) & !is.na(t_min))%>%select ()%>%View()
mapaCorrientes

------------------------------------------------------------
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("rgeos")
library(rgeos)
library(plotly)
library(wesanderson)

map <- rnaturalearth::ne_states(country = c("argentina", "Brazil", "Chile", "Uruguay", "Paraguay", "Bolivia"), returnclass = "sf")
map

wes_palettes <- names(wesanderson::wes_palettes)
View(wes_palettes)
# extraigo los colores de todas las paletas de WesAnderson con sus correspondientes nombres (lo tomé del código de @committedtotape)
wes_paleta_func <- function(pal) {
  col_df <- tibble(colores = wes_palette(pal), palette = pal)
}

wes_colores <- map_df(wes_palettes, wes_paleta_func)

#funciona
#-----------------------------------------------------------------------------------
## grafico temperaturas maximas accesible PARA PUBLICAR
#----------------------------------------------------------
map_mAXAccesible <- mapaA2012 %>% 
  ggplot(aes(lon, lat)) +
  geom_sf(data = map, inherit.aes = FALSE) +
  coord_sf(ylim = c(-39, -18), xlim = c(-72, -50)) +
  geom_point(aes(fill = mapaA2012$t_max), shape = 21,  size=3)+
  # scale_fill_divergent("Temperatura máximas") +
  scale_fill_gradient2("Temperatura máximas (C°)", limits = c(0, 50),
                       #rojo  low = "#1B7837", mid = "white", high = "#B10026")+
                       low = "#FFFFCC", mid = "#FEB24C", high = "#B10026")+
  #  low = "#1B7837", mid = "white", high = "#762A83")
  scale_size_area(max_size = 4, guide = "none") +
  labs(title = "Temperaturas máximas", subtitle= "Para el período: 01/2011-31/2012", x = "longitud", y = "Latitud") +
  theme(legend.position = "right",                
        legend.text = element_text(colour ="#446455" , size = 8),
        legend.title = element_text(colour = "#446455", size = 10),   #lila oscuro de RLadies="#446455"
        legend.title.align = 1,
        legend.background = element_rect(fill = "#efe9cc", colour ="#efe9cc"),  
        panel.background = element_rect(fill = "#efe9cc", colour = "#efe9cc"),    
        plot.title = element_text(colour ="#e26241" , size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack", face="bold"),	
        
        plot.subtitle = element_text(colour = "#446455", size = 14, hjust = 0.5,family = "FuturaBT-ExtraBlack", face="italic"),
        plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 10, hjust = 0.5,face="bold", vjust=1))


map_mAXAccesible
ggsave("map_mAXAccesible.png",width = 10, height = 5, dpi = "retina")
#plotly
hover <- with(mapaA2012, paste(t_max, '<br>', "Temp. Máx", media, "Media", "<br>"))
# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)
pMax2<-ggplotly(map_mAXAccesible, hoverformat='2.F', tooltip = "text")
pMax
pMax <-ggplotly(map_anom)%>%
  add_trace(text = ~hover)+l
pMax

#----------------------------------------------------------------------
#temp. mínimas accesible #grafico temperaturas minimas coloresAZULES
#----------------------------------------------------------------------
map_anomMInAccesible <- mapaA2012 %>% 
  ggplot(aes(lon, lat)) +
  geom_sf(data = map, inherit.aes = FALSE) +
  coord_sf(ylim = c(-39, -18), xlim = c(-72, -50)) +
  geom_point(aes(fill = t_min), shape = 23, size=3) +
  scale_fill_continuous ("Temperatura mínimas", limits = c(-5, 22),
                         low = "#40bfc1",mid="#41B6C4", high = "#253494")+
  #low ="#darkblue", mid ="lightblue", high ="1B7837")+
  scale_size_area(max_size = 4, guide = "none") +
  labs(title = "Temperaturas Mínimas", x = "longitud", y = "Latitud") +
  theme(legend.position = "right",                
        legend.text = element_text(colour ="#446455" , size = 8),
        legend.title = element_text(colour = "#446455", size = 10),  
        legend.title.align = 1,
        legend.background = element_rect(fill = wes_palette("Darjeeling2")[4], colour =NA),  
        panel.background = element_rect(fill = wes_palette("Darjeeling2")[4], colour = wes_palette("Darjeeling2")[4]),    
        plot.title = element_text(colour ="#2f416d" , size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack", face="bold"),	
        plot.subtitle = element_text(colour = "blue", size = 14, hjust = 0.5,family = "FuturaBT-ExtraBlack", face="italic"),
        plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 10, hjust = 0.5,face="bold", vjust=1))


map_anomMInAccesible
ggsave("map_anomMInacce.png",width = 10, height = 5, dpi = "retina")

# Para unir dos gràficos utilizo el paquete cowplot
#ejemplo
library(cowplot)
p1 <- ggplot(mtcars, aes(disp, mpg)) + 
  geom_point()
p2 <- ggplot(mtcars, aes(qsec, mpg)) +
  geom_point()

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)

#-----------------------------------------------------------------------------------
##grafico temperaturas maximas colores lilas
map_mAX <- mapaA2012 %>% 
  ggplot(aes(lon, lat)) +
  geom_sf(data = map, inherit.aes = FALSE) +
  coord_sf(ylim = c(-39, -18), xlim = c(-72, -50)) +
  geom_point(aes(fill = mapaA2012$t_max), shape = 21,  size=2)+
 # scale_fill_divergent("Temperatura máximas") +
  scale_fill_gradient2("Temperatura máximas (C°)", limits = c(0, 50),
                       low = "#1B7837", mid = "white", high = "#B10026")+
                     #  low = "#FFFFCC", mid = "#FEB24C", high = "red")+
  #  low = "#1B7837", mid = "white", high = "#762A83")
  scale_size_area(max_size = 4, guide = "none") +
  labs(title = "Temperaturas máximas", subtitle= "Para el período: 01/2011-31/2012", x = "longitud", y = "Latitud") +
  theme(legend.position = "right",                
        legend.text = element_text(colour ="#446455" , size = 8),
        legend.title = element_text(colour = "#446455", size = 10),   #lila oscuro de RLadies="#446455"
        legend.title.align = 1,
        legend.background = element_rect(fill = wes_palette("Darjeeling2")[4], colour =NA),  
        panel.background = element_rect(fill = wes_palette("Darjeeling2")[4], colour = wes_palette("Darjeeling2")[4]),    
        plot.title = element_text(colour ="#446455" , size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack", face="bold"),	
        
        plot.subtitle = element_text(colour = "#446455", size = 14, hjust = 0.5,family = "FuturaBT-ExtraBlack", face="italic"),
        plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 10, hjust = 0.5,face="bold", vjust=1))
  
  
map_mAX
ggsave("map_mAX.png",width = 10, height = 5, dpi = "retina")
#plotly
hover <- with(mapaA2012, paste(t_max, '<br>', "Temp. Máx", media, "Media", "<br>"))
# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)
pMax<-ggplotly(map_anom, hoverformat='2.F', tooltip = "text")

pMax <-ggplotly(map_anom)%>%
  add_trace(text = ~hover)+l
pMax

#solo otro estilo para plotly
# geo styling
estilo2 <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)
pMax <-ggplotly(map_anom)%>%
  add_trace(text = ~hover)+estilo2
pMax

#---------------------------------------------------------------------
#grafico temperaturas minimas colores verdes
map_anomMIn <- mapaA2012 %>% 
  ggplot(aes(lon, lat)) +
  geom_sf(data = map, inherit.aes = FALSE) +
  coord_sf(ylim = c(-39, -18), xlim = c(-72, -50)) +
  geom_point(aes(fill = t_min), shape = 23, size=3) +
  scale_fill_gradient("Temperatura mínimas", limits = c(-5, 22),
                       low = "#762A83", mid = "#F2A11F", high = "#1B7837")+
                       #low ="#darkblue", mid ="lightblue", high ="1B7837")+
  scale_size_area(max_size = 4, guide = "none") +
  labs(title = "Temperaturas Mínimas", x = "longitud", y = "Latitud") +
  theme(legend.position = "right",                
        legend.text = element_text(colour ="#446455" , size = 8),
        legend.title = element_text(colour = "#446455", size = 10),  
        legend.title.align = 1,
        legend.background = element_rect(fill = wes_palette("Darjeeling2")[4], colour =NA),  
        panel.background = element_rect(fill = wes_palette("Darjeeling2")[4], colour = wes_palette("Darjeeling2")[4]),    
        plot.title = element_text(colour ="#562457" , size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack", face="bold"),	
        
        plot.subtitle = element_text(colour = "#446455", size = 14, hjust = 0.5,family = "FuturaBT-ExtraBlack", face="italic"),
        plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 10, hjust = 0.5,face="bold", vjust=1))


map_anomMIn
ggsave("map_anomMIn.png",width = 10, height = 5, dpi = "retina")

#+
 # transition_time(fecha)
pMin <-ggplotly(map_anomMIn)
pMin



#otras paletas
#paletas accesibles
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
display.brewer.pal(n = 8, name = 'YlOrRd')
display.brewer.pal(n = 9, name = 'YlGnBu')
brewer.pal(n = 8, name = "YlOrRd")
brewer.pal(n = 9, name = "YlGnBu")

pal <- colorBin("YlOrRd", domain = mapaA2011$t_max)
colfunc <- colorRampPalette(c("salmon", "orange", "red"))

pMax <-ggplotly(map_anom)%>%
  add_trace(text = ~hover)+estilo2
pMax


#--------------------------------------------------------
heatmap(mapaA2011)
heatmap(mapaA2011, scale = "none")
mapaA20

# 3er intento
df <- subset(mapaA2012, month(fechaGrafico) %in% 6:12)
# ordered factor variable with month abbreviations
df$mes<- ordered(month.abb[df$fechaGrafico], levels = month.abb[6:12])
# September totals
df9 <- subset(df,  month(fechaGrafico) == 9)


g <- list(
  scope = 'argentina',
  showframe = F,
  showland = T,
  landcolor = toRGB("grey90")
)

g1 <- c(
  g,
  resolution = 50,
  showcoastlines = T,
  countrycolor = toRGB("white"),
  coastlinecolor = toRGB("white"),
  projection = list(type = 'Mercator'),
  list(lonaxis = list(range = c(-15, -5))),
  list(lataxis = list(range = c(0, 12))),
  list(domain = list(x = c(0, 1), y = c(0, 1)))
)

g2 <- c(
  g,
  showcountries = F,
  bgcolor = toRGB("white", alpha = 0),
  list(domain = list(x = c(0, .6), y = c(0, .6)))
)
g2
p <- mapaA2012 %>%
  plot_geo(
    locationmode = 'country names', sizes = c(1, 600), color = I("black")
  ) %>%
  add_markers(
    y = ~lat, x = ~lon, 
    size = ~media, color = df$mes, text = ~paste(t_max, "t_max")
  )

#%>%
  add_text(
    x = 21.0936, y = 7.1881, text = 'argentina', showlegend = F, geo = "geo2"
  )%>%
  add_trace(
    data = df9, z = ~month(fechaGrafico),
    showscale = F, geo = "geo2"
  )
p
#---------------------------------------------------------------------------------

library(gganimate)
mapaRladies+
  transition_time(month(fechaGrafico)) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)



install.packages("leaflet")
library("leaflet")

leaflet(data=mapaA2012)%>%
  setView(-96, 37.8, 4) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(t_max), stroke = FALSE)


#ejemplo----------------------------------
m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

#-----------------------------------------
df <- select(df, -year, -decade, -month) %>%
  mutate(year_mon = as.Date(year_mon)) %>%
  rename(date = year_mon)
#-----------------------------------------

#separo fechas en estaciones_meteo
separoFechasMeteo <- estaciones_meteo %>% filter(!is.na(t_max) & !is.na(t_min)) %>% mutate(anio=year(fecha),mes=month(fecha), dia= day(fecha))%>% View()
# elijo los campos con los que voy a trabajar

datosMeteo <-separoFechasMeteo %>% select(everything(),-institucion)
datosMeteoCorrientes<-separoFechasMeteo %>% filter(id_estacion==10470)
#%>%View() 

#la estación 10470 corresponde a Corrientes
# filtro por la estacion de corrientes y los valores distintos a nulos
corrientes <- meteo%>%filter(id_estacion==10470 & !is.na(t_max) & !is.na(t_min))%>% View()
dfprueba <- mapaA2011 %>% select(t_max, t_min)
dfprueba <- scale(dfprueba)
heatmap(dfprueba, scale = "colum")
#----------------------------------------------------------------------
#https://bookdown.org/content/2274/series-temporales.html
#series de tiempo
tsserie <- corrientes %>%select(precipitacion, t_max, t_min)
View(tsserie)
#meteoFecha <- meteo%>%mutate(fechaOrdenada=dmy(fecha))%>%View()

tsSerie.modelo <-ts(tsserie, start = c(1961, 11,1), end = c(2012, 12, 9), frequency = 7)
View(tsSerie.modelo)

min <-tsSerie.modelo %>% select(t_min)
tsmin <-ts(min, start = c(1961, 11,1),frequency = 7)
ts(zoodata,frequency=7,start=c(1968,1))

max <- corrientes%>%select(t_max)
tsmax <-ts(max, start = c(1961, 11,1), end = c(2012, 12, 9), frequency = 1)

hc <- highchart(type = "stock") %>% 
  hc_title(text = "Charting some Symbols") %>% 
  hc_subtitle(text = "Data extracted using quantmod package") %>% 
  hc_add_series(tsSerie.modelo, id = "usdjpy")
#%>% 
 # hc_add_series(t_min, id = "eurkpw")

hc
       
  
#####################################################################
