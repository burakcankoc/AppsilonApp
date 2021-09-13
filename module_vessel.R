source("utils.R")

# Module UI
uiVessel <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    div(class = "ui center aligned header", "Marine Route Analysis by Ship Type"),
    uiOutput(ns("type_selector")), ## Vessel type selector
    uiOutput(ns("vessel_selector")), ## Vessel selector
    actionButton(ns("Run"), label = "Check Vessel Route"),
    segment(
      div(
        class = "ui three column stackable grid container",
        div(
          class = "column",
          custom_ui_message(
            textOutput(ns("longest_distance")),
            "Distance Between Points",
            color = "orange",
            icon_name = "running"
          )
        ),
        div(
          class = "column",
          custom_ui_message(
            textOutput(ns("time1")),
            "Start Date & Time",
            color = "olive",
            icon_name = "calendar"
          )
        ),
        div(
          class = "column",
          custom_ui_message(
            textOutput(ns("time2")),
            "End Date & Time",
            color = "blue",
            icon_name = "calendar"
          )
        )
      )
    ),
    br(),
    segment(
      class = "basic",
      a(class = "ui blue ribbon label", "Map of the Vessel"),
      leafletOutput(ns("map"))
    )
  )
}

# Module Server
vesselServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    output$type_selector = renderUI({
      selectInput(ns("VesselType"),
                  "Select a vessel type:",
                  c(unique(df$ship_type)),
                  selected = "Fishing")
    })
    
    output$vessel_selector = renderUI({
      data_available = df[df$ship_type == req(input$VesselType), ]
      
      selectizeInput(
        ns("ShipName"),
        label = "Select Vessel",
        #label displayed in ui
        choices = c(unique(data_available$ship_codename)),
        #calls list of available ships
        selected = "HEL-111 | 315804"
      )
    })
    
    vessel_data <- eventReactive(input$Run, {
      validate(need(c(input$VesselType, input$ShipName), "Fetching data..."))
      toast("Wait while data is loading...",
            class = "warning",
            duration = 1.5)
      
      # Data to be displayed
      df %>%
        filter(ship_type == input$VesselType) %>%
        filter(input$ShipName == ship_codename) %>%
        arrange(DATETIME) %>%
        mutate(
          LON2 = lead(LON),
          LAT2 = lead(LAT),
          DATETIME2 = lead(DATETIME)
        ) %>%
        mutate(distance = distHaversine(cbind(LON, LAT), cbind(LON2, LAT2)))
    })
    
    # Get Max Distance between points
    output$longest_distance <- renderText({
      get_max_distance(vessel_data())
    })
    
    # Get Start Time
    output$time1 <- renderText({
      get_time1(vessel_data())
    })
    
    # Get End Time
    output$time2 <- renderText({
      get_time2(vessel_data())
    })
    
    # Render Map
    output$map <- renderLeaflet({
      
      highlight_df <- vessel_data() %>%
        arrange(desc(DATETIME)) %>%
        slice_max(distance, n = 1)
      
      m <- leaflet() %>% addTiles()
      m <-
        m %>% setView(
          lat = mean(highlight_df$LAT),
          lng = mean(highlight_df$LON),
          zoom = 8
        ) %>%
        fitBounds( ## Define boundaries of the map
          lng1 = min(highlight_df$LON),
          lat1 = min(highlight_df$LAT),
          lng2 = max(highlight_df$LON2),
          lat2 = max(highlight_df$LAT2)
        ) %>%
        addPolylines( ## Add lines between coordinates
          data = highlight_df,
          lng = c(highlight_df$LON, highlight_df$LON2),
          lat = c(highlight_df$LAT, highlight_df$LAT2),
          group = 1,
          weight = 1,
          opacity = 1
        ) %>%
        addCircleMarkers( ## Add circle points to the coordinates
          data = highlight_df,
          lng = c(highlight_df$LON, highlight_df$LON2),
          lat = c(highlight_df$LAT, highlight_df$LAT2),
          group = 1,
          radius = 0.5
        ) %>% 
        addAwesomeMarkers(
          data = highlight_df,
          lng = c(highlight_df$LON, highlight_df$LON2),
          lat = c(highlight_df$LAT, highlight_df$LAT2),
          icon=icons, label=~as.character(mag))
      m
    })
  })
}