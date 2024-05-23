library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(bsicons)
library(dplyr)
proyectos_mineros = st_read('./proyectos-mineros-ubicacin-aproximada-/proyectos-mineros-ubicaciăn-aproximada--shp.shp')
limite_interprovincial = st_read('./linea_de_limite_070111/linea_de_limite_070111.shp')
pm_point = st_cast(proyectos_mineros, "POINT")
minerales <- unique(proyectos_mineros$MINERAL_PR)

item_counts <- table(pm_point$MINERAL_PR)

mineral_mas_frecuente <- names(which.max(item_counts))
cantidad_mineral_mas_frecuente <- max(item_counts)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Proyecto final DatGeo",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    selectInput(
      "mineral",
      "Seleccione un mineral:",
      choices = append(minerales, "Todos", after = 0),
      selected = 1
    )
  ),
  # Output: Histogram ----
  card(leafletOutput("myMap", width = "100%", height = 600)),
  
  layout_column_wrap(
    width = 1/3,
    height = 300,
    value_box(
          title = "Total Proyectos",
          value = nrow(pm_point),
          showcase = bs_icon("bar-chart"),
          theme = "purple",
      ),
     value_box(
          title = "Total Minerales",
          value = length(minerales),
          showcase = bs_icon("bar-chart"),
          theme = "red",
      ),
      value_box(
           title = "Mineral con mas proyectos",
           value = paste(mineral_mas_frecuente, cantidad_mineral_mas_frecuente, sep = " - "),
           showcase = bs_icon("bar-chart"),
           theme = "purple",
         )
    ),
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })


  filteredData <- reactive({
    if(input$mineral == "Todos")
      {pm_point}
    else
      {subset(pm_point, MINERAL_PR == input$mineral)}
  })

  
  output$myMap <- renderLeaflet({
    mapa_base_IGNG <- leaflet()%>% 
      addTiles('https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_topo@EPSG%3A3857@png/{z}/{x}/{-y}.png') %>%
      setView(lng =-66.113,lat =-27.591, zoom = 7) %>%  
      addWMSTiles(
        "https://wms.ign.gob.ar/geoserver/capabaseargenmap/gwc/service/wmts?request=GetCapabilities",
        layers= "Capa base Argenmap",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "") %>%
      addMarkers(data = filteredData(), label = ~paste(NOMBRE, MINERAL_PR, sep="-"))%>%
      addPolylines(data = limite_interprovincial)
  })
  
  observe({
    leafletProxy("myMap", session = shiny::getDefaultReactiveDomain())
  })
}

shinyApp(ui = ui, server = server)