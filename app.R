library(tidyverse)
library(janitor)
library(readxl)
library(rgdal)
library(sf)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)

options(scipen = 999)

# 0 LEVANTO DATA ####

# https://data.buenosaires.gob.ar/dataset/campanas-verdes
campanas_verdes<-read.csv("./data/campanas-verdes.csv", encoding = "Latin")%>%
  clean_names()%>%
  mutate(dire=paste(calle_nombre, calle_altura, sep = " "))

# https://data.buenosaires.gob.ar/dataset/centros-clasificacion-residuos
centro_clasificacion_residuos<-read_xlsx("./data/centros-de-clasificacion-de-residuos.xlsx")%>%
  clean_names()

# https://data.buenosaires.gob.ar/dataset/puntos-verdes/resource/juqdkmgo-1716-resource
puntos_verdes<-st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/agencia-de-proteccion-ambiental/puntos-verdes/puntos-verdes.geojson")%>%
  mutate(
    longitud = st_coordinates(geometry)[,1],
    latitud = st_coordinates(geometry)[,2]
  )%>%
  clean_names()


# 1 APP ####
## 1.1 UI ####
ui <- fluidPage(
  
  titlePanel("Geolocalización de puntos de reciclaje en CABA"),
  
  mainPanel(tabsetPanel(
      tabPanel("Mapa", leafletOutput("map")),
      tabPanel("Estadísticas", 
               plotlyOutput("estadisticas"),
               actionButton("button1","en construccion")))#,
      #tabPanel("Acerca del mapa",
               #p("Esta página busca mostrar mapa muestra la distribución geográfica de los contenedores 
                 #y ")))
    #p("seleccione el tipo de establecimiento"),
    #actionButton("gobutton", "Estad?sticas"),
    #plotlyOutput("estadisticas")
    
))
    
## 1.2 SERVER ####
server <- function(input, output) {
  
  
  ### Mapa ####
      output$map <- renderLeaflet({
        leaflet()%>%
          addProviderTiles("OpenStreetMap.Mapnik")%>%
          addMarkers(data = campanas_verdes,
                     group = "Contenedores para Reciclables",
                     lng = ~long, lat = ~lat,
                     label = ~dire,
                     clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))%>%
          addMarkers(data = centro_clasificacion_residuos,
                     group = "Centros de Tratamiento",
                     lng = ~longitud, lat = ~latitud,
                     label = ~direccion,
                     clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))%>%
          addMarkers(data = puntos_verdes,
                     group = "Puntos Verdes",
                     lng = ~longitud,
                     lat = ~latitud,
                     label = ~direccion, 
                     clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))%>%
          addLayersControl(position = "bottomleft",
                           overlayGroups = c("Contenedores para Reciclables", 
                                             "Centros de Tratamiento",
                                             "Puntos Verdes"))
      
      
    })
      
  ### PLOT ####
    output$estadisticas<-renderPlotly({
      
      campanas_comunas<-campanas_verdes%>%
        group_by(comuna)%>%
        summarise(cantidad=n())%>%
        arrange(cantidad)%>%
        separate(col = comuna,into = c("tipo", "comuna"), sep = " ")%>%
        mutate(comuna = as.numeric(comuna))
      
      plot<-ggplot(data = campanas_comunas, 
               aes(x= reorder(comuna, cantidad), 
                   y= cantidad,
                   fill = as.character(comuna)))+
          geom_bar(stat = "identity", position = "identity")+
          coord_flip()+
          theme(axis.text.x = element_text(angle = 45))+
          labs(title = "Cantidad de Campanas Verdes Por Comuna",
               x= "comuna",
               fill= "comuna")
      
      ggplotly(p = plot, tooltip = c("comuna", "cantidad"))
      
    })
    
}

shinyApp(ui = ui, server = server)
