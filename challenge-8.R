# Challenge #8: Inzidenzzahlen der Hamburger Bezirke

# Lade den Datensatz stadtteile_wsg84.RDS
# Recherchiere die Fallzahlen der letzten sieben Tage für die Hamburger Bezirke
# https://www.hamburg.de/corona-zahlen/
# Erstelle eine leaflet Map und visualisiere die Inzidenzzahlen (Achtung: Nicht die Fallzahlen)
# Nutze dafür Shapes, Legende, Hovereffekte und Labels
# Exportiere die Map als HTML file

# Libraries
library(plotly)
library(leaflet)
library(tidyverse)
library(sf)
library(dplyr)
library(htmltools)

# data
bezirke <- readRDS("data/bezirke_wsg84.RDS") %>% 
  rename(SHAPE_Length = SHP_Length) %>% 
  rename(SHAPE_Area = SHP_Area)

bezirke_info <- readRDS("data/hamburg_districts.rds") %>% 
  select(bezirk, einwohner)

fälle_lst <- tibble(
  key = 1:7,
  fälle_letzte_sieben_Tage = c(298,141,238,571,248,237,455))

bezirke_bj <- tibble(
  key = 1:7,
  Bezirk_Name = c("Altona","Bergedorf","Eimsbüttel","Hamburg-Mitte","Hamburg-Nord","Harburg","Wandsbek"))

bezirke_inzidenz <- bezirke_bj %>% 
  left_join(fälle_lst)

bezirke <- bezirke_inzidenz %>% 
  left_join(bezirke, by = c("Bezirk_Name" = "Bezirk_Name")) %>%
  st_as_sf() %>% 
  st_transform('+proj=longlat +datum=WGS84')

bezirke <- bezirke %>% 
  left_join(bezirke_info, by = c("Bezirk_Name" = "bezirk"))

bezirke <- bezirke %>% 
  mutate(
    inzidenz = round(fälle_letzte_sieben_Tage / einwohner *100000, 0)
    )

###Corona-Map###
cuts2 <- c(0, 25, 50, 75, 100, 125, 150, 200)
pal2 <- colorBin("YlOrRd", domain = bezirke$inzidenz, bins = cuts2)

labels <- sprintf("<strong>%s</strong><br>Inzidenz: %g<br>Neuinfektionen: %g",
                  bezirke$Bezirk_Name, bezirke$inzidenz, bezirke$fälle_letzte_sieben_Tage) %>% 
  map(HTML)
coronamap <- leaflet(data = bezirke) %>%
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(lng = 9.993682, lat =53.551086, zoom = 10) %>% 
  addPolygons(fillColor = ~pal2(inzidenz),
              weight = 1,
              opacity = 0.75,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 3,
                color = "black",
                bringToFront = TRUE,
                fillOpacity = 0.8),
              label = labels) %>%
  addLegend(pal = pal2, 
            values = ~inzidenz, 
            title = "Inzidenzwert",
            position = "bottomright",
            opacity = 0.7)
coronamap

htmlwidgets::saveWidget(as_widget(coronamap), "coronamap_hamburg.html")