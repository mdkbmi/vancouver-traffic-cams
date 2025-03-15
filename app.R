options(shiny.port = 8050, shiny.autoreload = TRUE)

library(tidyverse)
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)
library(DT)

ZOOM_WIDE <- 12
ZOOM_AREA <- 14
ZOOM_POINT <- 17

# load data
url_webcam <- "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/web-cam-url-links/exports/csv"

data_webcam <- read_delim(url_webcam, delim = ";") |> 
  separate_wider_delim(geo_point_2d, ", ",
                       names = c("lat", "lng")) |> 
  select(-c(geom)) |> 
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng)) |> 
  arrange(name)

data_webcam$geo_local_area <- replace_na(data_webcam$geo_local_area, "Other")

url_areas <- "https://opendata.vancouver.ca/api/explore/v2.1/catalog/datasets/local-area-boundary/exports/csv"

data_areas <- read_delim(url_areas, delim = ";") |> 
  separate_wider_delim(geo_point_2d, ", ",
                       names = c("lat", "lng")) |> 
  select(-c(geom)) |> 
  arrange(name) |> 
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng)) |> 
  add_row(name = "All within City of Vancouver",
          lat = 49.2577354, lng = -123.123904, .before = 1)

# cards
title <- titlePanel(HTML("<b>Traffic Cameras in the City of Vancouver</b>"),
                    "Traffic Cameras in the City of Vancouver")

filter_area <- card(
  card_header("Area"),
  selectInput("area", label = NULL, choices = data_areas$name, 
              selected = data_areas$name[1]
))

css <- HTML("
.card {
  overflow: visible !important;
}
.card-body {
  overflow: visible !important;
}
.selectize-control .selectize-dropdown {
  position: absolute !important;
  z-index: 1000 !important;
}
")

output_count <- card(
 card_header("No. of intersections with cameras:"),
 h3(textOutput("CAM_COUNT"), align = "center")
)

output_list <- card(
  card_header("List of intersections:"),
  dataTableOutput("CAM_LIST")
)

sidebar <- layout_columns(
  filter_area,
  output_count,
  output_list,
  col_widths = c(12, 12, 12)
)

output_map <- card(
  leafletOutput("CAM_MAP", height = "70vh"),
  card_footer(p("Data source: ", a("City of Vancouver", href = "https://opendata.vancouver.ca/pages/home/")))
)

# Define UI
ui <- page_fillable(
  tags$head(tags$style(css)),
  
  layout_columns(
    card(title), # row 1
    card(sidebar),
    output_map,
    col_widths = c(12, 3, 9)
    
  ),
  
  theme = bs_theme(base_font = "'Helvetica Neue'",
                   )
  
)

# Define server logic
server <- function(input, output, session) {
  datatable_webcam <- reactive({
    if (input$area != data_areas$name[1]) {
      data_webcam |> 
        filter(geo_local_area == input$area)
    } else {
      data_webcam
    }
  })
  
  output$CAM_COUNT <- renderText({
    nrow(datatable_webcam())
  })
  
  output$CAM_LIST <- renderDataTable(
    datatable(
      datatable_webcam(), 
      colnames = NULL, rownames = FALSE, selection = 'single',
      fillContainer = TRUE,
      options = list(
        columnDefs = list(
          list(targets = 0, visible = FALSE),
          list(targets = 2, visible = FALSE),
          list(targets = 3, visible = FALSE),
          list(targets = 4, visible = FALSE),
          list(targets = 5, visible = FALSE)
          ), paging = FALSE, ordering = FALSE, searching = FALSE,
        info = FALSE)
      )
    )
  
  output$CAM_MAP <- renderLeaflet({
    leaflet() |>
      addTiles()
  })
  
  proxy_CAM_MAP <- leafletProxy(mapId = "CAM_MAP", session)
  proxy_CAM_LIST <- dataTableProxy("CAM_LIST", session)
  
  observeEvent(input$area, {
    select_area <- which(data_areas$name == input$area)
    
    if (select_area != 1) {
    proxy_CAM_MAP |>
      setView(lat = data_areas$lat[select_area], 
              lng = data_areas$lng[select_area], 
              zoom = ZOOM_AREA)
    } else {
      proxy_CAM_MAP |>
        setView(lat = data_areas$lat[select_area], 
                lng = data_areas$lng[select_area], 
                zoom = ZOOM_WIDE)
    }
  })
  
  observeEvent(input$CAM_LIST_rows_selected, {
    selected_row <- input$CAM_LIST_rows_selected
    
    if (!is.null(selected_row)) {
      proxy_CAM_MAP |>
        setView(lng = datatable_webcam()$lng[selected_row], 
                lat = datatable_webcam()$lat[selected_row], 
                zoom = ZOOM_POINT)
    } else {
      proxy_CAM_MAP |>
        clearMarkers() |>
        addMarkers(data = datatable_webcam(),
                   ~lng, ~lat, label = ~htmlEscape(name),
                   popup = ~paste0('<a href="', url, '" target="_blank">', htmlEscape(name), '</a>')) |>
        setView(lat = data_areas$lat[1], lng = data_areas$lng[1], zoom = ZOOM_WIDE)
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(datatable_webcam(), {
    proxy_CAM_MAP |>
      clearMarkers() |>
      addMarkers(data = datatable_webcam(),
                 ~lng, ~lat, label = ~htmlEscape(name),
                 popup = ~paste0('<a href="', url, '" target="_blank">', htmlEscape(name), '</a>'))
  })
  
  observeEvent(input$CAM_MAP_marker_click, {
    click <- input$CAM_MAP_marker_click
    temp <- datatable_webcam()
    select_row <- which(temp$lat == click$lat & temp$lng == click$lng)
    selectRows(proxy_CAM_LIST, select_row)
    
    proxy_CAM_MAP |>
      setView(lng = click$lng, lat = click$lat, zoom = ZOOM_POINT)
  })
  
  observeEvent(input$CAM_MAP_click, {
    selectRows(proxy_CAM_LIST, NULL)
    
    proxy_CAM_MAP |>
      setView(lat = data_areas$lat[1], lng = data_areas$lng[1], zoom = ZOOM_WIDE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
