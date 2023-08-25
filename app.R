library(shiny)
library(leaflet)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(rgdal)

base<-read.csv("campanas-verdes.csv")%>%
    mutate(dire=paste(calle_nombre, calle_altura, sep = " "))

centro_reciclaje<-read_xlsx("centros-de-clasificacion-de-residuos.xlsx")

#puntos_verdes<-read_xlsx("puntos-verdes.xlsx")%>%
  #mutate(long=Longitud/100000)%>%
  #mutate(lat=Latitud/1000000)
#ojo que hay que arreglar long y lat
puntos_verdes<-readOGR("Puntos_verdes_shp.shp")
puntos_verdes<-read.csv("puntos-verdes.csv")

leaflet()%>%
  addProviderTiles("OpenStreetMap.Mapnik")%>%
  addMarkers(data=puntos_verdes,
             group = "puntos verdes",
             lng=~Longitud, lat=~Latitud, 
             label=~Direccion,
             clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))


campanas_comunas<-base%>%
  group_by(comuna)%>%
  summarise(cantidad=n())%>%
  arrange(cantidad)%>%
  separate(col = comuna,into = c("tipo", "comuna"), sep = " ")


campanas_comunas$comuna<-as.numeric(campanas_comunas$comuna)

campanas_comunas<-campanas_comunas%>%
  arrange(comuna)


  #as.character.numeric_version(campanas_comunas$comuna)
  
str(campanas_comunas)

pal<-15
paleta<-colorRampPalette(brewer.pal(8, "Set1"))(pal)

#----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Geolocalizaci�n de puntos de reciclaje en CABA"),
  mainPanel(tabsetPanel(
      tabPanel("Mapa", leafletOutput("map")),
      tabPanel("Estad�sticas", 
               plotlyOutput("estadisticas"),
               actionButton("button1","en construcción")))#,
      #tabPanel("Acerca del mapa",
               #p("Esta página busca mostrar mapa muestra la distribución geográfica de los contenedores 
                 #y ")))
    #p("seleccione el tipo de establecimiento"),
    #actionButton("gobutton", "Estad�sticas"),
    #plotlyOutput("estadisticas")
    
))
    

server <- function(input, output) {
  
      output$map <- renderLeaflet({
      leaflet()%>%
        addProviderTiles("OpenStreetMap.Mapnik")%>%
        addMarkers(data=base,
                   group = "contenedores para reciclables",
                   lng=~long, lat=~lat, 
                   label=~dire,
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))%>%
        addMarkers(data=centro_reciclaje,
                   group = "centros de tratamiento",
                   lng=~Longitud, lat=~Latitud, 
                   label=~Nombre,
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F))%>%
        addLayersControl(position = "bottomleft",
                         overlayGroups = c("centros de tratamiento", "contenedores para reciclables"))
      
      
    })
    
    #output$gobutton<-eventReactive(input$gobutton, {showTab(output&estadisticas)})
     
      
    
    output$estadisticas<-renderPlotly(ggplotly(ggplot(data = campanas_comunas, aes(x=reorder(factor(comuna),cantidad, sum) , y=cantidad, 
                                                                                   fill=fct_relevel(comuna, "1","2","3","4","5",
                                                                                                    "6","7","8","9","10","11","12",
                                                                                                    "13","14","15")))+
                 theme(axis.text.x = element_text(angle = 45))+
                 labs(title = "cantidad de campanas verdes por comuna",
                      x= "comuna",
                      fill= "comuna")+
                 geom_bar(stat = "identity", position = "identity")+
                 #scale_fill_manual(values = paleta)+
                   coord_flip(), 
                 tooltip = c("comuna", "cantidad"))%>%
                   config(displayModeBar=F))
      
    
}

shinyApp(ui = ui, server = server)


#--------------------------------------------------------------------------


#info de hogares https://data.buenosaires.gob.ar/dataset/informacion-censal-por-radio

info_censal<-read.csv("informacion-censal-por-radio-2010.csv", sep = ";")
info_censal<-info_censal%>%
  mutate(COMUNA= ifelse(COMUNA == "13", "COMUNA 13", COMUNA))%>%
  group_by(COMUNA)%>%
  summarise(T_HOGAR=sum(T_HOGAR))%>%
  separate(COMUNA, c("uno","comuna"))


info_censal$comuna<-as.character(info_censal$comuna)
info_censal <- info_censal[ ,!colnames(info_censal)=="uno"]


str(info_censal)



#info de poblacion por comuna https://data.buenosaires.gob.ar/dataset/estructura-demografica
poblacion_comunas<-read.csv("gcba_pob_comunas_17.csv", sep = ",")%>%
  arrange(COMUNA)%>%
  rename(comuna = COMUNA)

poblacion_comunas$comuna<-as.character(poblacion_comunas$comuna)


str(poblacion_comunas)

#junto toda la info

total1<-inner_join(campanas_comunas, info_censal, by="comuna")
total2<-inner_join(total1, poblacion_comunas, by="comuna")
rm(total1)




#calculo tasas
total2<-total2%>%
  mutate(tasa_camp_hogar = T_HOGAR/cantidad)%>%
  mutate(tasa_camp_poblacion=POBLACION/cantidad)

total2$cantidad<-as.numeric(total2$cantidad)
total2$POBLACION<-as.numeric((total2$POBLACION))
total2$`tasa_camp/hogar`<-as.numeric(total2$`tasa_camp/hogar`)
total2$`tasa_camp/poblacion`<-as.numeric((total2$`tasa_camp/poblacion`))
str(total2)

plot_tasas<-ggplotly(ggplot(total2, aes(x=comuna, y=tasa_camp_hogar))+
  geom_bar(stat = "identity")+
  coord_flip())
plot_tasas
