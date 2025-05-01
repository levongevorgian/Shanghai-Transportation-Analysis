library(shiny)
library(colourpicker)
library(shinydashboard)
library(leaflet.extras)
library(shinyWidgets)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(viridis)  
library(RColorBrewer)
library(data.table)
library(sf)
library(DT)
library(lubridate)
library(tidyr)
library(leaflet)
library(hexbin)
library(rmarkdown)
library(glue)
library(colourpicker)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
      }
      
      .navbar {
        background-color: #f39c12 !important; 
      }
      
      .nav-tabs > li > a {
        background-color: #f39c12 !important;
        color: white !important;
        margin-right: 4px;
        border-radius: 4px 4px 0 0;
      }
      
      .nav-tabs > li.active > a {
        background-color: #e67e22 !important;
        color: white !important;
        font-weight: bold;
      }
      
      .nav-tabs > li > a:hover {
        background-color: #e67e22 !important;
      }
      
      .about-container {
        padding: 20px;
      }
      .about-text {
        background-color: rgba(255, 255, 255, 0.9);
        padding: 25px;
        border-radius: 5px;
        margin-top: 20px;
        border-left: 4px solid #f39c12;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .about-text h3 {
        margin-bottom: 20px;
        color: #f39c12;
        text-align: center;
      }
      .about-text p {
        line-height: 1.6;
        margin-bottom: 15px;
      }
      .about-text ul {
        margin-left: 20px;
        margin-bottom: 20px;
      }
      .about-text li {
        margin-bottom: 8px;
        line-height: 1.5;
      }
      
      .dataTables_wrapper {
        width: 100%;
        position: relative;
      }
      .dataTables_wrapper .dataTables_scroll {
        clear: both;
        width: 100%;
      }
      .dataTables_scrollBody {
        width: 100% !important;
      }
      
      .banner-image {
        width: 100%;
        height: 1000px;
        object-fit: cover;
        margin-bottom: 20px;
        border-radius: 5px;
      }
      
      .tab-content {
        padding: 20px 0;
      }
      
      .btn-primary {
        background-color: #f39c12 !important;
        border-color: #e67e22 !important;
      }
      
      .well-orange {
        background-color: #f8f9fa;
        border: 1px solid #f39c12;
        border-left: 4px solid #f39c12;
        border-radius: 4px;
      }
      
      .app-header {
        background-color: #f39c12;
        color: white;
        padding: 15px;
        margin-bottom: 20px;
        text-align: center;
        font-size: 24px;
        font-weight: bold;
      }
    "))
  ),
  
  div(class = "app-header", "Shanghai Transportation"),
  
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    tabPanel(
      title = "About",
      value = "about",
      div(
        class = "about-container",
        fluidRow(
          column(
            width = 12,
            tags$img(src = "taxi_bus.jpg", class = "banner-image", alt = "Shanghai Transportation"),
            div(
              class = "about-text",
              h3("Shanghai Transportation System"),
              hr(),
              p("Shanghai is renowned for its vast, efficient, and highly integrated public transportation network, 
              serving over 24 million residents. From its massive metro system with over 400 stations, to a bus network 
              comprising more than 1,000 routes, and a fleet of 50,000 taxis, the city exemplifies urban mobility at scale. 
              It also boasts a widely adopted bike-sharing system and the world’s first high-speed commercial maglev line 
              connecting Pudong Airport to the city center."),
              
              p("To understand and analyze this complex network, we worked with a structured dataset containing detailed 
              information on taxi, bus, and bike-sharing services. The data was initially clean, with no missing values 
              or duplicates, though some sparse and irrelevant entries—such as incomplete trajectories or zero-activity 
              segments—were filtered out and saved separately to maintain traceability. The final dataset enabled us to 
              extract meaningful insights into traffic flow, usage frequency, geographic patterns, and travel behavior."),
              
              p("All data analysis and visualization were performed using R and Python, utilizing their robust data wrangling 
              and statistical toolsets to uncover trends across different districts and time periods. These insights form the 
              foundation of this interactive platform, which allows users to explore Shanghai's transportation dynamics in detail."),
              
              p("Efficient urban transportation is vital for quality of life, economic development, and environmental sustainability. 
              Our findings aim to support stakeholders such as city planners, transportation authorities, and policymakers 
              in improving infrastructure, optimizing services, and reducing congestion. Furthermore, the analysis provides 
              actionable information for private operators and environmental advocates to push for more sustainable solutions 
              like cycling programs and electric vehicle adoption."),
              
              p("This application provides an interactive interface for exploring Shanghai’s transportation data, with visual tools 
              that reveal how people move through the city and how various modes of transport interact. Users can examine patterns 
              in taxi pickups and drop-offs, bus routing, bike-sharing behavior, travel times, and vehicle speeds."),
              
              p(style = "text-align: right; font-style: italic; margin-top: 30px;",
                "By students Albert Hakobyan and Levon Gevorgyan")
              
            )
          )
        )
      )
    ),
    
    tabPanel(
      title = "Bus Visualization",
      value = "bus_viz",
      tabsetPanel(
        tabPanel("Bus Routes and Stops",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         selectizeInput(
                           "route_select",
                           "Select Bus Routes",
                           choices = NULL,
                           multiple = TRUE,
                           options = list(plugins = list("remove_button"))
                         ),
                         checkboxInput("show_all_routes", "Show All Routes", value = TRUE),
                         sliderInput("route_line_width", "Line Width",
                                     min = 0.5, max = 3, value = 1, step = 0.1),
                         colourInput("background_color", "Background Color", value = "#FFFFFF")
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("bus_routes_plot", height = "600px")
                   )
                 )
        ),
        tabPanel("Route Map",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         radioGroupButtons(
                           inputId = "bus_route_display",
                           label = "Display Options",
                           choices = c("Both Directions", "Direction 1", "Direction 2"),
                           selected = "Both Directions",
                           status = "warning"
                         ),
                         checkboxInput("show_stations", "Show Stations", TRUE),
                         sliderInput("line_thickness", "Line Thickness",
                                     min = 0.5, max = 3, value = 1.2, step = 0.1)
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("bus_route_map", height = "600px")
                   )
                 )
        ),
        tabPanel("Route Analysis",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         selectInput("route_metric", "Metric to Visualize",
                                     choices = c("Segment Length", "Travel Time", "Turn Angles"),
                                     selected = "Travel Time"),
                         checkboxInput("show_points", "Show Data Points", TRUE)
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("route_analysis_plot", height = "600px")
                   )
                 )
        ),
        tabPanel("Time Estimation",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         selectInput("start_station", "Starting Station",
                                     choices = c("Yanan Road Station", "Shenkun Station"),
                                     selected = "Yanan Road Station"),
                         selectInput("end_station", "Ending Station",
                                     choices = c("Yanan Road Station", "Shenkun Station"),
                                     selected = "Shenkun Station"),
                         sliderInput("time_of_day", "Time of Day (24-hour)",
                                     min = 0, max = 23, value = 12)
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("time_estimation_plot", height = "400px"),
                     verbatimTextOutput("time_estimation_text")
                   )
                 )
        ),
        tabPanel("Arrival Times",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         sliderInput(
                           "arrival_bins", 
                           "Number of Bins:",
                           min = 5, 
                           max = 30, 
                           value = 24,
                           step = 1
                         ),
                         colourInput(
                           "arrival_color", 
                           "Histogram Color", 
                           value = "#40E0D0"
                         ),
                         checkboxInput(
                           "show_density", 
                           "Show Density Line", 
                           value = FALSE
                         ),
                         hr(),
                         helpText("This chart shows the hourly distribution of bus GPS records, indicating when buses are most active throughout the day.")
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("bus_arrival_plot", height = "600px")
                   )
                 )
        )
      )
    ),
    
    tabPanel(
      title = "Taxi Visualization",
      value = "taxi_viz",
      tabsetPanel(
        tabPanel("Activity Patterns",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         selectInput("activity_metric", "Visualization Type",
                                     choices = c("Hourly Activity", "Pickup vs Dropoff", "Speed Distribution"),
                                     selected = "Hourly Activity"),
                         checkboxInput("filter_passenger", "Filter by Passenger Status", TRUE),
                         sliderInput("time_range", "Time Range",
                                     min = 0, max = 24, value = c(0, 24), step = 1)
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("taxi_activity_plot", height = "600px")
                   )
                 )
        ),
        tabPanel("Spatial Analysis",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         selectInput("spatial_view", "View Type",
                                     choices = c("Pickup/Dropoff Heatmap", "Speed by Region", "Pickup-Dropoff Ratio"),
                                     selected = "Pickup/Dropoff Heatmap"),
                         sliderInput("bin_size", "Spatial Resolution",
                                     min = 20, max = 100, value = 50),
                         checkboxInput("show_district_boundaries", "Show District Boundaries", FALSE)
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("taxi_spatial_plot", height = "600px")
                   )
                 )
        ),
        tabPanel("Vehicle Analysis",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         selectInput("vehicle_metric", "Analysis Type",
                                     choices = c("Speed Distribution", "Activity Levels", "Direction Ratio"),
                                     selected = "Speed Distribution"),
                         sliderInput("top_n_vehicles", "Top N Vehicles",
                                     min = 5, max = 20, value = 10)
                     )
                   ),
                   column(
                     width = 9,
                     plotOutput("vehicle_analysis_plot", height = "600px")
                   )
                 )
        )
      )
    ),
    
    tabPanel(
      title = "Bike Visualization",
      value = "bike_viz",
      tabsetPanel(
        tabPanel("Activity Patterns",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         radioButtons(
                           "bike_activity_view",
                           "View Option:",
                           choices = c(
                             "Lock/Unlock Activity" = "lock_unlock",
                             "Net Availability" = "net_availability"
                           ),
                           selected = "lock_unlock"
                         ),
                         conditionalPanel(
                           condition = "input.bike_activity_view == 'lock_unlock'",
                           checkboxInput("stack_lock_unlock", "Stack Bars", FALSE),
                           selectInput("lock_color", "Lock Color (Status 1):", 
                                       choices = c("Orange" = "#F79054", 
                                                   "Blue" = "#157EFB", 
                                                   "Green" = "#28a745",
                                                   "Purple" = "#6f42c1"), 
                                       selected = "#F79054"),
                           selectInput("unlock_color", "Unlock Color (Status 0):", 
                                       choices = c("Navy" = "#0D4262", 
                                                   "Red" = "#dc3545", 
                                                   "Teal" = "#20c997",
                                                   "Yellow" = "#ffc107"), 
                                       selected = "#0D4262")
                         ),
                         conditionalPanel(
                           condition = "input.bike_activity_view == 'net_availability'",
                           selectInput("net_line_color", "Line Color:", 
                                       choices = c("Brown" = "#57190F", 
                                                   "Blue" = "#0D4262", 
                                                   "Green" = "#28a745",
                                                   "Purple" = "#6f42c1"), 
                                       selected = "#57190F"),
                           sliderInput("line_size", "Line Size:", 
                                       min = 0.5, max = 3, value = 1.5, step = 0.1)
                         ),
                         hr(),
                         helpText("This visualization shows bike sharing activity patterns throughout the day.")
                     )
                   ),
                   column(
                     width = 9,
                     plotlyOutput("bike_activity_plot", height = "600px")
                   )
                 )
        ),
        
        tabPanel("Trip Duration",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         sliderInput("duration_bins", "Number of Bins:", 
                                     min = 10, max = 100, value = 50, step = 5),
                         sliderInput("max_duration", "Max Duration (minutes):", 
                                     min = 60, max = 1440, value = 240, step = 30),
                         selectInput("duration_color", "Histogram Color:", 
                                     choices = c("Blue" = "#095370", 
                                                 "Orange" = "#F79054", 
                                                 "Purple" = "#6f42c1",
                                                 "Green" = "#28a745"), 
                                     selected = "#095370"),
                         checkboxInput("show_duration_kde", "Show Density Curve", TRUE),
                         hr(),
                         helpText("This visualization shows the distribution of bike trip durations, calculated as the time between unlock and lock events.")
                     )
                   ),
                   column(
                     width = 9,
                     plotlyOutput("bike_duration_plot", height = "600px")
                   )
                 )
        ),
        
        tabPanel("Movement Heatmap",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         selectInput("heatmap_type", "Heatmap Type:", 
                                     choices = c("Bike Pickups (Status 1)" = "pickups",
                                                 "Bike Returns (Status 0)" = "returns"), 
                                     selected = "pickups"),
                         sliderInput("heatmap_radius", "Heat Radius:", 
                                     min = 5, max = 20, value = 8, step = 1),
                         sliderInput("heatmap_blur", "Blur:", 
                                     min = 5, max = 25, value = 15, step = 1),
                         selectInput("heatmap_gradient", "Color Gradient:",
                                     choices = c("Default" = "default",
                                                 "Viridis" = "viridis",
                                                 "Magma" = "magma",
                                                 "Inferno" = "inferno"),
                                     selected = "default"),
                         hr(),
                         helpText("This heatmap displays locations where bike trips began or ended, with brighter areas indicating higher activity.")
                     )
                   ),
                   column(
                     width = 9,
                     leafletOutput("bike_heatmap", height = "600px")
                   )
                 )
        ),
        
        tabPanel("Bike Usage",
                 fluidRow(
                   column(
                     width = 3,
                     div(class = "well well-orange",
                         sliderInput("top_n_bikes", "Top N Bikes:", 
                                     min = 5, max = 30, value = 10, step = 5),
                         selectInput("usage_color", "Bar Color:", 
                                     choices = c("Olive" = "#4D4F3C", 
                                                 "Blue" = "#095370", 
                                                 "Orange" = "#F79054",
                                                 "Teal" = "#20c997"), 
                                     selected = "#4D4F3C"),
                         checkboxInput("sort_by_usage", "Sort by Usage", TRUE),
                         hr(),
                         helpText("This visualization shows the most frequently used bikes in the system.")
                     )
                   ),
                   column(
                     width = 9,
                     plotlyOutput("bike_usage_plot", height = "600px")
                   )
                 )
        )
      )
    ),
    
    tabPanel(
      title = "Summary Table",
      value = "summary_table",
      fluidRow(
        column(
          width = 3,
          div(class = "well well-orange",
              selectInput(
                inputId = "data_source",
                label = "Data Source",
                choices = c("Bus GPS", "Taxi Data", "Bike Data"),
                selected = "Taxi Data"
              ),
              conditionalPanel(
                condition = "input.data_source == 'Taxi Data'",
                selectInput(
                  inputId = "taxi_summary_var",
                  label = "Variable to Summarize",
                  choices = c("Speed", "OpenStatus", "Hour"),
                  selected = "Speed"
                ),
                selectInput(
                  inputId = "taxi_group_var",
                  label = "Group By",
                  choices = c("TimePeriod", "PassengerStatus", "Hour"),
                  selected = "TimePeriod"
                )
              ),
              conditionalPanel(
                condition = "input.data_source == 'Bus GPS'",
                selectInput(
                  inputId = "bus_summary_var",
                  label = "Variable to Summarize",
                  choices = c("ToDir", "hour_of_day"),
                  selected = "hour_of_day"
                ),
                selectInput(
                  inputId = "bus_group_var",
                  label = "Group By",
                  choices = c("VehicleId", "ToDir"),
                  selected = "VehicleId"
                )
              ),
              conditionalPanel(
                condition = "input.data_source == 'Bike Data'",
                selectInput(
                  inputId = "bike_summary_var",
                  label = "Variable to Summarize",
                  choices = c("LOCK_STATUS", "Hour", "Duration"),
                  selected = "Hour"
                ),
                selectInput(
                  inputId = "bike_group_var",
                  label = "Group By",
                  choices = c("LOCK_STATUS", "Hour", "BIKE_ID"),
                  selected = "Hour"
                )
              )
          )
        ),
        column(
          width = 9,
          dataTableOutput("summary_statistics")
        )
      )
    ),
    
    tabPanel(
      title = "Traffic Analysis",
      value = "model",
      fluidRow(
        column(
          width = 3,
          div(class = "well well-orange",
              selectInput(
                "analysis_type",
                "Analysis Type",
                c("Taxi Speed Analysis", "Bus Activity Patterns", 
                  "Pickup/Dropoff Analysis", "Vehicle Comparison",
                  "Bike Activity Analysis", "Multi-Modal Comparison"),
                selected = "Taxi Speed Analysis"
              ),
              
              conditionalPanel(
                condition = "input.analysis_type == 'Taxi Speed Analysis'",
                checkboxGroupInput(
                  "time_periods",
                  "Time Periods",
                  choices = c("Morning Rush (6-10)", "Midday (10-16)", 
                              "Evening Rush (16-20)", "Night (20-6)"),
                  selected = c("Morning Rush (6-10)", "Evening Rush (16-20)") 
                ),
                checkboxInput(
                  "split_by_passenger",
                  "Split by Passenger Status",
                  value = TRUE
                )
              ),
              
              conditionalPanel(
                condition = "input.analysis_type == 'Bus Activity Patterns'",
                sliderInput(
                  "top_n_vehicles",
                  "Top N Vehicles",
                  min = 1,
                  max = 20,
                  value = 10
                ),
                sliderInput(
                  "hour_range",
                  "Hour Range",
                  min = 0,
                  max = 23,
                  value = c(6, 20),
                  step = 1
                )
              ),
              
              conditionalPanel(
                condition = "input.analysis_type == 'Pickup/Dropoff Analysis'",
                sliderInput(
                  "bin_resolution",
                  "Spatial Resolution",
                  min = 20,
                  max = 100,
                  value = 50
                ),
                selectInput(
                  "pickup_viz_type",
                  "Visualization Type",
                  choices = c("Heatmap", "Scatter"),
                  selected = "Heatmap"
                )
              ),
              
              conditionalPanel(
                condition = "input.analysis_type == 'Vehicle Comparison'",
                selectInput(
                  "vehicle_data",
                  "Data Source",
                  choices = c("Taxi", "Bus"),
                  selected = "Taxi"
                ),
                selectInput(
                  "comparison_metric",
                  "Comparison Metric",
                  choices = c("Speed", "Activity", "Patterns"),
                  selected = "Speed"
                )
              ),
              
              conditionalPanel(
                condition = "input.analysis_type == 'Vehicle Comparison' && input.vehicle_data == 'Taxi' && input.comparison_metric == 'Speed'",
                checkboxInput("exclude_zero_speed", "Exclude Zero Speed", value = FALSE)
              ),
              
              conditionalPanel(
                condition = "input.analysis_type == 'Bike Activity Analysis'",
                checkboxGroupInput(
                  "bike_time_periods",
                  "Time Periods",
                  choices = c("Morning (6-10)", "Midday (10-16)", 
                              "Evening (16-20)", "Night (20-6)"),
                  selected = c("Morning (6-10)", "Evening (16-20)") 
                ),
                sliderInput(
                  "max_bike_duration",
                  "Max Trip Duration (min)",
                  min = 30,
                  max = 240,
                  value = 120
                )
              ),
              
              conditionalPanel(
                condition = "input.analysis_type == 'Multi-Modal Comparison'",
                checkboxGroupInput(
                  "transport_modes",
                  "Transport Modes",
                  choices = c("Bus", "Taxi", "Bike"),
                  selected = c("Bus", "Taxi", "Bike")
                ),
                selectInput(
                  "comparison_period",
                  "Time Period",
                  choices = c("Full Day", "Morning Rush", "Evening Rush"),
                  selected = "Full Day"
                )
              ),
              
              downloadButton("report", "Generate report", style = "background-color: #f39c12; color: white;")
          )
        ),
        column(
          width = 9,
          plotOutput("analysis_plot", height = "500px"),
          verbatimTextOutput("analysis_summary")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  bus_routes <- reactive({
    st_read("busline.json", quiet = TRUE)
  })
  
  busgps_df <- reactive({
    raw_data <- read.csv("busgps.csv", header = FALSE, stringsAsFactors = FALSE)
    colnames(raw_data) <- c('GPSDateTime', 'LineId', 'LineName', 'NextLevel', 'PrevLevel',
                            'Strlatlon', 'ToDir', 'VehicleId', 'VehicleNo', 'unknow')
    raw_data$GPSDateTime <- as.POSIXct(raw_data$GPSDateTime)
    
    raw_data <- raw_data %>%
      mutate(jp_vehicleID = VehicleId) %>%
      group_by(VehicleId) %>%
      mutate(temp_id = cur_group_id()) %>%
      ungroup() %>%
      mutate(VehicleId = paste0("Vehicle_", temp_id)) %>%
      select(-temp_id)
    
    raw_data <- raw_data %>%
      mutate(
        coordinates = strsplit(as.character(Strlatlon), ","),
        latitude = as.numeric(sapply(coordinates, function(x) x[1])),
        longitude = as.numeric(sapply(coordinates, function(x) x[2])),
        hour_of_day = as.numeric(format(GPSDateTime, "%H")))
    
    return(raw_data)
  })
  
  taxi_data <- reactive({
    raw_data <- read.csv("TaxiData-Sample.csv", header = FALSE)
    names(raw_data) <- c("VehicleNum", "Time", "Lng", "Lat", "OpenStatus", "Speed")
    
    raw_data <- raw_data %>%
      mutate(
        DateTime = as.POSIXct(Time, format = "%H:%M:%S"),
        Hour = hour(DateTime),
        Minute = minute(DateTime),
        TimePeriod = case_when(
          Hour >= 6 & Hour < 10 ~ "Morning Rush (6-10)",
          Hour >= 10 & Hour < 16 ~ "Midday (10-16)",
          Hour >= 16 & Hour < 20 ~ "Evening Rush (16-20)",
          TRUE ~ "Night (20-6)"
        ),
        PassengerStatus = ifelse(OpenStatus == 1, "With Passenger", "Empty")
      )
    
    raw_data$TimePeriod <- factor(raw_data$TimePeriod, 
                                  levels = c("Morning Rush (6-10)", 
                                             "Midday (10-16)", 
                                             "Evening Rush (16-20)", 
                                             "Night (20-6)"))
    
    return(raw_data)
  })
  
  bike_data <- reactive({
    raw_data <- read.csv("bikedata-sample.csv")
    
    raw_data <- raw_data %>%
      mutate(
        DATA_TIME = as.POSIXct(DATA_TIME, format = "%Y-%m-%d %H:%M:%S"),
        Hour = hour(DATA_TIME),
        Date = as.Date(DATA_TIME),
        TimePeriod = case_when(
          Hour >= 6 & Hour < 10 ~ "Morning (6-10)",
          Hour >= 10 & Hour < 16 ~ "Midday (10-16)",
          Hour >= 16 & Hour < 20 ~ "Evening (16-20)",
          TRUE ~ "Night (20-6)"
        )
      )
    
    return(raw_data)
  })
  
  bike_trips <- reactive({
    df <- bike_data()
    
    df_sorted <- df %>% 
      arrange(BIKE_ID, DATA_TIME)
    
    df_diff <- df_sorted %>%
      group_by(BIKE_ID) %>%
      mutate(
        prev_status = lag(LOCK_STATUS),
        status_change = ifelse(is.na(prev_status) | LOCK_STATUS != prev_status, 1, 0)
      ) %>%
      filter(status_change == 1) %>%
      select(-status_change) %>%
      ungroup()
    
    # Calculate trip durations (time between unlock and lock events)
    df_diff <- df_diff %>%
      group_by(BIKE_ID) %>%
      mutate(
        time_diff = as.numeric(difftime(lead(DATA_TIME), DATA_TIME, units = "mins")),
        duration = ifelse(LOCK_STATUS == 1 & lead(LOCK_STATUS) == 0, time_diff, NA)
      ) %>%
      ungroup()
    
    # Filter for valid durations
    df_trips <- df_diff %>%
      filter(!is.na(duration), duration > 0, duration < 1440) # Max 24 hours
    
    return(df_trips)
  })
  
  # Extract route information
  route_data <- reactive({
    routes <- bus_routes()
    
    # Extract route information for bus routes
    route1 <- routes %>% filter(Id == 0) # Direction 1
    route2 <- routes %>% filter(Id == 1) # Direction 2
    
    # Getting route coordinates
    route1_coords <- st_coordinates(route1)[, 1:2]
    route2_coords <- st_coordinates(route2)[, 1:2]
    
    # Extracting start and end points for each route
    route1_start <- route1_coords[1, ] %>% as.numeric()
    route1_end <- route1_coords[nrow(route1_coords), ] %>% as.numeric()
    route2_start <- route2_coords[1, ] %>% as.numeric()
    route2_end <- route2_coords[nrow(route2_coords), ] %>% as.numeric()
    
    # Define route information
    route_info <- data.frame(
      direction = c("方向1", "方向2"),
      start_station = c("延安东路外滩", "申昆路枢纽站"),
      end_station = c("申昆路枢纽站", "延安东路外滩"),
      route_id = c(0, 1))
    
    # Updating to English names
    route_info <- route_info %>%
      mutate(start_station_en = case_when(
        start_station == "延安东路外滩" ~ "Yanan Road Station",
        start_station == "申昆路枢纽站" ~ "Shenkun Station",
        TRUE ~ start_station),
        end_station_en = case_when(
          end_station == "申昆路枢纽站" ~ "Shenkun Station",
          end_station == "延安东路外滩" ~ "Yanan Road Station",
          TRUE ~ end_station),
        direction_en = case_when(
          route_id == 0 ~ "Yanan Road-Shenkun",
          route_id == 1 ~ "Shenkun-Yanan Road",
          TRUE ~ as.character(direction)))
    
    # Create a combined routes dataset for analysis
    # First, extract the coordinates and add route identifiers
    route1_df <- data.frame(st_coordinates(route1)[, 1:2]) %>%
      mutate(route = "Direction 1", point_id = 1:n())
    
    route2_df <- data.frame(st_coordinates(route2)[, 1:2]) %>%
      mutate(route = "Direction 2", point_id = 1:n())
    
    # Combine the routes
    combined_routes <- bind_rows(route1_df, route2_df)
    
    return(list(
      route1 = route1,
      route2 = route2,
      route1_coords = route1_coords,
      route2_coords = route2_coords,
      route1_start = route1_start,
      route1_end = route1_end,
      route2_start = route2_start,
      route2_end = route2_end,
      route_info = route_info,
      route1_df = route1_df,
      route2_df = route2_df,
      combined_routes = combined_routes
    ))
  })
  
  # Bus Visualization outputs
  output$bus_route_map <- renderPlot({
    rd <- route_data()
    route1_df <- rd$route1_df
    route2_df <- rd$route2_df
    
    # Get route display selection
    display_selection <- input$bus_route_display
    show_stations <- input$show_stations
    line_thickness <- input$line_thickness
    
    # Create plot with direct chaining and conditional components
    ggplot() +
      # Add routes based on selection
      {if(display_selection %in% c("Both Directions", "Direction 1"))
        geom_path(data = route1_df, aes(x = X, y = Y), 
                  color = "blue", size = line_thickness, alpha = 0.8)
      } +
      {if(display_selection %in% c("Both Directions", "Direction 2"))
        geom_path(data = route2_df, aes(x = X, y = Y), 
                  color = "red", size = line_thickness, alpha = 0.8)
      } +
      # Add stations if selected
      {if(show_stations) {
        # Create start and end points dataframe
        start_end_points <- rbind(
          data.frame(name = "Direction 1 Start", x = rd$route1_start[1], y = rd$route1_start[2], 
                     type = "start", route = "Direction 1"),
          data.frame(name = "Direction 1 End", x = rd$route1_end[1], y = rd$route1_end[2], 
                     type = "end", route = "Direction 1"),
          data.frame(name = "Direction 2 Start", x = rd$route2_start[1], y = rd$route2_start[2], 
                     type = "start", route = "Direction 2"),
          data.frame(name = "Direction 2 End", x = rd$route2_end[1], y = rd$route2_end[2], 
                     type = "end", route = "Direction 2")
        )
        
        list(
          geom_point(data = start_end_points, 
                     aes(x = x, y = y, color = type, shape = route), size = 4),
          scale_color_manual(values = c("start" = "green", "end" = "purple"),
                             labels = c("End Point", "Start Point")),
          scale_shape_manual(values = c("Direction 1" = 16, "Direction 2" = 17))
        )
      }} +
      # Add titles and theme
      theme_bw() +
      labs(title = "Bus Route 71 - Shanghai",
           subtitle = paste0("Blue: Yanan Road-Shenkun Direction\nRed: Shenkun-Yanan Road Direction"),
           x = "Longitude", y = "Latitude",
           color = "Point Type",
           shape = "Route") +
      coord_sf(datum = NA)
  })
  
  output$route_analysis_plot <- renderPlot({
    # Get route data
    rd <- route_data()
    route1_df <- rd$route1_df
    route2_df <- rd$route2_df
    
    # Function to calculate segment properties
    calculate_segment_properties <- function(route_df) {
      # Calculate the length and direction of each segment
      segments <- route_df %>%
        mutate(
          next_X = lead(X),
          next_Y = lead(Y),
          segment_length = sqrt((next_X - X)^2 + (next_Y - Y)^2) * 111.32, # km
          segment_direction = atan2(next_Y - Y, next_X - X) * 180/pi,
          # Normalize directions to 0-360
          segment_direction = ifelse(segment_direction < 0, segment_direction + 360, segment_direction)
        ) %>%
        filter(!is.na(segment_length)) # Remove the last point which has no next point
      return(segments)
    }
    
    # Calculate properties for both routes
    route1_segments <- calculate_segment_properties(route1_df) %>% 
      mutate(route = rd$route_info$direction_en[1], 
             segment_id = row_number(),
             cumulative_distance = cumsum(segment_length))
    
    route2_segments <- calculate_segment_properties(route2_df) %>% 
      mutate(route = rd$route_info$direction_en[2], 
             segment_id = row_number(),
             cumulative_distance = cumsum(segment_length))
    
    # Combine segments
    all_segments <- bind_rows(route1_segments, route2_segments)
    
    # Generate plot based on selected metric
    if(input$route_metric == "Segment Length") {
      # Direct chaining without intermediate variable storage
      ggplot(all_segments, aes(x = segment_id, y = segment_length, color = route)) +
        labs(title = "Segment Length Analysis",
             subtitle = "Comparing segment lengths along each route",
             x = "Segment ID", y = "Segment Length (km)",
             color = "Route") +
        # Conditionally add points based on input
        {if(input$show_points) geom_point(alpha = 0.5)} +
        geom_line() +
        scale_color_manual(values = c("blue", "red")) +
        theme_bw()
      
    } else if(input$route_metric == "Travel Time") {
      # Assign speeds based on segment properties
      all_timed_segments <- all_segments %>%
        group_by(route) %>%
        mutate(
          # Calculating turn sharpness from previous segment
          prev_direction = lag(segment_direction),
          turn_angle = if_else(is.na(prev_direction), 0, 
                               abs(((segment_direction - prev_direction + 180) %% 360) - 180)),
          
          # Assigning speeds based on segment characteristics
          segment_speed_kmh = case_when(
            turn_angle > 45 ~ 10,  # Very sharp turns: 10 km/h
            turn_angle > 30 ~ 15,  # Sharp turns: 15 km/h
            turn_angle > 15 ~ 20,  # Moderate turns: 20 km/h
            TRUE ~ 25              # Straight segments: 25 km/h
          ),
          
          # Calculate time for each segment (in minutes)
          segment_time_min = (segment_length / segment_speed_kmh) * 60,
          
          # Calculate cumulative time
          cumulative_time_min = cumsum(segment_time_min)
        ) %>%
        ungroup()
      
      ggplot(all_timed_segments, aes(x = cumulative_distance, y = cumulative_time_min, color = route)) +
        geom_line(size = 1.2) +
        {if(input$show_points) 
          geom_point(data = all_timed_segments %>% filter(segment_id %% 20 == 0), 
                     size = 3, alpha = 0.7)
        } +
        scale_color_manual(values = c("blue", "red")) +
        theme_bw() +
        labs(title = "Estimated Travel Time Along Bus Route 71",
             subtitle = "Based on segment lengths and estimated speeds",
             x = "Distance Along Route (km)",
             y = "Cumulative Travel Time (minutes)",
             color = "Route")
      
    } else if(input$route_metric == "Turn Angles") {
      # Analyze the directional changes along the routes
      direction_changes <- all_segments %>%
        group_by(route) %>%
        mutate(
          prev_direction = lag(segment_direction),
          turn_angle = abs(((segment_direction - prev_direction + 180) %% 360) - 180),
          significant_turn = turn_angle > 25 # Define significant turns as >25 degrees
        ) %>%
        filter(!is.na(turn_angle)) %>%
        mutate(cumulative_turns = cumsum(turn_angle))
      
      ggplot(direction_changes, aes(x = cumulative_distance, y = cumulative_turns, color = route)) +
        geom_line(size = 1) +
        {if(input$show_points) geom_point(alpha = 0.5)} +
        scale_color_manual(values = c("blue", "red")) +
        theme_bw() +
        labs(title = "Cumulative Turn Analysis",
             subtitle = "Higher slopes indicate sections with more turning",
             x = "Distance Along Route (km)",
             y = "Cumulative Turn Angle (degrees)",
             color = "Route")
    }
  })
  
  observe({
    bus_data <- busgps_df()
    vehicle_ids <- unique(bus_data$VehicleId)
    
    updateSelectizeInput(
      session, 
      "route_select",
      choices = vehicle_ids,
      selected = if(input$show_all_routes) vehicle_ids else NULL,
      server = TRUE
    )
  })
  
  observeEvent(input$show_all_routes, {
    if(input$show_all_routes) {
      bus_data <- busgps_df()
      vehicle_ids <- unique(bus_data$VehicleId)
      updateSelectizeInput(
        session, 
        "route_select",
        selected = vehicle_ids
      )
    } else {
      updateSelectizeInput(
        session, 
        "route_select",
        selected = NULL
      )
    }
  })
  
  output$bus_routes_plot <- renderPlot({
    bus_data <- busgps_df()
    
    selected_routes <- input$route_select
    if(length(selected_routes) == 0 && input$show_all_routes) {
      selected_routes <- unique(bus_data$VehicleId)
    }
    
    filtered_data <- bus_data %>%
      filter(VehicleId %in% selected_routes)
    
    p <- ggplot() +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = input$background_color),
        panel.grid.major = element_line(color = "#DDDDDD"),
        panel.grid.minor = element_line(color = "#EEEEEE")
      ) +
      labs(
        title = "Bus Routes and Stops in Shanghai",
        subtitle = paste(length(selected_routes), "routes displayed"),
        x = "Longitude",
        y = "Latitude"
      )
    
    if(nrow(filtered_data) > 0) {
      for(vehicle in selected_routes) {
        vehicle_data <- filtered_data %>% 
          filter(VehicleId == vehicle) %>%
          arrange(GPSDateTime)
        
        p <- p + 
          geom_path(
            data = vehicle_data,
            aes(x = longitude, y = latitude, group = VehicleId, color = VehicleId),
            size = input$route_line_width,
            alpha = 0.7
          )
      }
      
      p <- p + 
        scale_color_viridis_d(name = "Bus Route") +
        guides(color = guide_legend(override.aes = list(size = 2, alpha = 1)))
      
      p <- p + 
        coord_cartesian(
          xlim = c(min(filtered_data$longitude, na.rm = TRUE) - 0.01, 
                   max(filtered_data$longitude, na.rm = TRUE) + 0.01),
          ylim = c(min(filtered_data$latitude, na.rm = TRUE) - 0.01,
                   max(filtered_data$latitude, na.rm = TRUE) + 0.01)
        )
    } else {
      p <- p + 
        annotate("text", x = 0.5, y = 0.5, label = "No routes selected", size = 8) +
        theme_void()
    }
    
    p
  })
  
  output$bus_arrival_plot <- renderPlot({
    # Get the bus data
    bus_data <- busgps_df()
    
    # Extract hour of day from the GPS timestamp
    bus_hours <- bus_data$hour_of_day
    
    # Create the histogram plot
    p <- ggplot(data = data.frame(hour = bus_hours), aes(x = hour)) +
      geom_histogram(
        bins = input$arrival_bins,
        fill = input$arrival_color,
        color = "black",
        alpha = 0.7
      ) +
      labs(
        title = "Distribution of Bus Arrivals and Departures by Hour",
        x = "Hour of Day",
        y = "Frequency"
      ) +
      scale_x_continuous(breaks = seq(0, 23, by = 2)) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    
    # Add density line if requested
    if(input$show_density) {
      p <- p + geom_density(aes(y = ..count..), color = "red", size = 1.2)
    }
    
    p
  })
  
  output$time_estimation_plot <- renderPlot({
    rd <- route_data()
    route1_df <- rd$route1_df
    route2_df <- rd$route2_df
    
    calculate_segment_properties <- function(route_df) {
      segments <- route_df %>%
        mutate(
          next_X = lead(X),
          next_Y = lead(Y),
          segment_length = sqrt((next_X - X)^2 + (next_Y - Y)^2) * 111.32, # km
          segment_direction = atan2(next_Y - Y, next_X - X) * 180/pi,
          segment_direction = ifelse(segment_direction < 0, segment_direction + 360, segment_direction)
        ) %>%
        filter(!is.na(segment_length))
      return(segments)
    }
    
    route1_segments <- calculate_segment_properties(route1_df) %>% 
      mutate(route = rd$route_info$direction_en[1], 
             segment_id = row_number(),
             cumulative_distance = cumsum(segment_length))
    
    route2_segments <- calculate_segment_properties(route2_df) %>% 
      mutate(route = rd$route_info$direction_en[2], 
             segment_id = row_number(),
             cumulative_distance = cumsum(segment_length))
    
    selected_route <- NULL
    if(input$start_station == "Yanan Road Station" && input$end_station == "Shenkun Station") {
      selected_route <- route1_segments
      direction_text <- "Yanan Road → Shenkun"
    } else if(input$start_station == "Shenkun Station" && input$end_station == "Yanan Road Station") {
      selected_route <- route2_segments
      direction_text <- "Shenkun → Yanan Road"
    } else {
      plot(1:10, 1:10, type = "n", xlab = "Distance (km)", ylab = "Time (minutes)",
           main = "Please select different start and end stations")
      text(5, 5, "Start and end stations must be different", col = "red")
      return()
    }
    
    time_multiplier <- case_when(
      input$time_of_day >= 7 && input$time_of_day <= 9 ~ 1.3,  # Morning rush hour
      input$time_of_day >= 17 && input$time_of_day <= 19 ~ 1.4,  # Evening rush hour
      input$time_of_day >= 22 || input$time_of_day <= 5 ~ 0.8,  # Night time
      TRUE ~ 1.0)  # Normal time
    
    
    timed_route <- selected_route %>%
      mutate(
        prev_direction = lag(segment_direction),
        turn_angle = if_else(is.na(prev_direction), 0, 
                             abs(((segment_direction - prev_direction + 180) %% 360) - 180)),
        
        segment_speed_kmh = case_when(
          turn_angle > 45 ~ 10 / time_multiplier,  # Very sharp turns
          turn_angle > 30 ~ 15 / time_multiplier,  # Sharp turns
          turn_angle > 15 ~ 20 / time_multiplier,  # Moderate turns
          TRUE ~ 25 / time_multiplier             # Straight segments
        ),
        
        segment_time_min = (segment_length / segment_speed_kmh) * 60,
        
        cumulative_time_min = cumsum(segment_time_min)
      )
    
    target_times <- seq(5, max(timed_route$cumulative_time_min), by = 5)
    time_markers <- data.frame()
    
    for(target_time in target_times) {
      idx <- which.min(abs(timed_route$cumulative_time_min - target_time))
      
      marker <- timed_route[idx, ] %>%
        mutate(time_marker = paste0(round(cumulative_time_min), " min"))
      
      time_markers <- bind_rows(time_markers, marker)
    }
    
    ggplot() +
      geom_path(data = selected_route, aes(x = X, y = Y), 
                color = ifelse(direction_text == "Yanan Road → Shenkun", "blue", "red"), 
                alpha = 0.6, size = 1.2) +
      geom_point(data = time_markers, aes(x = X, y = Y), 
                 color = ifelse(direction_text == "Yanan Road → Shenkun", "blue", "red"), 
                 size = 3) +
      geom_text(data = time_markers, aes(x = X, y = Y, label = time_marker),
                color = "black", vjust = -1.2, size = 3.5) +
      geom_point(data = selected_route[c(1, nrow(selected_route)), ], 
                 aes(x = X, y = Y),
                 color = c("green", "purple"), size = 5) +
      labs(title = paste("Estimated Travel Time:", direction_text),
           subtitle = paste("At", input$time_of_day, ":00, total time:", 
                            round(max(timed_route$cumulative_time_min)), "minutes"),
           x = "Longitude", y = "Latitude") +
      theme_bw()
  })
  
  output$time_estimation_text <- renderText({
    rd <- route_data()
    
    if(input$start_station == "Yanan Road Station" && input$end_station == "Shenkun Station") {
      direction_text <- "Yanan Road → Shenkun"
      route_id <- 0
    } else if(input$start_station == "Shenkun Station" && input$end_station == "Yanan Road Station") {
      direction_text <- "Shenkun → Yanan Road"
      route_id <- 1
    } else {
      return("Please select different start and end stations.")
    }
    
    if(route_id == 0) {
      route_length <- sum(sqrt(diff(rd$route1_coords[,1])^2 + diff(rd$route1_coords[,2])^2)) * 111.32
    } else {
      route_length <- sum(sqrt(diff(rd$route2_coords[,1])^2 + diff(rd$route2_coords[,2])^2)) * 111.32
    }
    
    time_multiplier <- case_when(
      input$time_of_day >= 7 && input$time_of_day <= 9 ~ 1.3,
      input$time_of_day >= 17 && input$time_of_day <= 19 ~ 1.4,
      input$time_of_day >= 22 || input$time_of_day <= 5 ~ 0.8,
      TRUE ~ 1.0
    )
    
    avg_speed <- 22 / time_multiplier
    
    travel_time_hours <- route_length / avg_speed
    travel_time_minutes <- round(travel_time_hours * 60)
    
    time_description <- case_when(
      input$time_of_day >= 7 && input$time_of_day <= 9 ~ "during morning rush hour",
      input$time_of_day >= 17 && input$time_of_day <= 19 ~ "during evening rush hour",
      input$time_of_day >= 22 || input$time_of_day <= 5 ~ "during night time (reduced traffic)",
      TRUE ~ "during normal traffic conditions"
    )
    
    paste0(
      "Route: ", direction_text, "\n",
      "Distance: ", round(route_length, 2), " km\n",
      "Estimated travel time ", time_description, ": ", travel_time_minutes, " minutes\n",
      "Average speed: ", round(avg_speed, 1), " km/h\n",
      "Note: Estimates include typical stops and traffic conditions at ", input$time_of_day, ":00."
    )
  })
  
  output$bike_activity_plot <- renderPlotly({
    df <- bike_data()
    
    if(input$bike_activity_view == "lock_unlock") {
      p <- df %>%
        group_by(Hour, LOCK_STATUS) %>%
        summarise(Count = n(), .groups = "drop") %>%
        mutate(Status = ifelse(LOCK_STATUS == 1, "Lock (Bike Taken)", "Unlock (Bike Returned)")) %>%
        ggplot(aes(x = Hour, y = Count, fill = Status)) +
        theme_minimal() +
        labs(
          title = "Lock/Unlock Activity by Hour",
          x = "Hour of Day",
          y = "Number of Events"
        ) +
        scale_x_continuous(breaks = seq(0, 23, 2))
      
      if(input$stack_lock_unlock) {
        p <- p + geom_bar(stat = "identity")
      } else {
        p <- p + geom_bar(stat = "identity", position = "dodge")
      }
      
      p <- p + scale_fill_manual(values = c(
        "Lock (Bike Taken)" = input$lock_color,
        "Unlock (Bike Returned)" = input$unlock_color
      ))
      
    } else {
      lock_counts <- df[df$LOCK_STATUS == 1,] %>% 
        group_by(Hour) %>% 
        summarise(Count = n(), .groups = "drop")
      
      unlock_counts <- df[df$LOCK_STATUS == 0,] %>% 
        group_by(Hour) %>% 
        summarise(Count = n(), .groups = "drop")
      
      net_data <- full_join(lock_counts, unlock_counts, by = "Hour", suffix = c("_lock", "_unlock")) %>%
        replace_na(list(Count_lock = 0, Count_unlock = 0)) %>%
        mutate(NetAvailability = Count_unlock - Count_lock)
      
      p <- ggplot(net_data, aes(x = Hour, y = NetAvailability)) +
        geom_line(color = input$net_line_color, size = input$line_size) +
        geom_point(color = input$net_line_color, size = input$line_size * 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        theme_minimal() +
        labs(
          title = "Hourly Net Bike Availability",
          subtitle = "Positive values indicate more bikes being returned than taken",
          x = "Hour of Day",
          y = "Net Availability"
        ) +
        scale_x_continuous(breaks = seq(0, 23, 2))
    }
    
    ggplotly(p) %>% 
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  output$bike_duration_plot <- renderPlotly({
    trips <- bike_trips()
    
    filtered_trips <- trips %>%
      filter(duration <= input$max_duration)
    
    p <- ggplot(filtered_trips, aes(x = duration)) +
      geom_histogram(
        bins = input$duration_bins,
        fill = input$duration_color,
        color = "black",
        alpha = 0.7
      ) +
      theme_bw() +
      labs(
        title = "Bike Trip Duration Distribution",
        subtitle = paste0("Trips up to ", input$max_duration, " minutes"),
        x = "Duration (minutes)",
        y = "Number of Trips"
      )
    
    if (input$show_duration_kde) {
      durations <- na.omit(filtered_trips$duration)
      if (length(durations) > 1) {
        dens <- density(durations, from = min(durations), to = max(durations))
        scale_factor <- nrow(filtered_trips) * mean(diff(dens$x))
        dens_df <- data.frame(x = dens$x, y = dens$y * scale_factor)
        
        p <- p + geom_line(
          data = dens_df,
          aes(x = x, y = y),
          color = "red",
          size = 1.1
        )
        
      }
    }
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  output$bike_heatmap <- renderLeaflet({
    df <- bike_data()
    
    if(input$heatmap_type == "pickups") {
      filtered_data <- df %>% filter(LOCK_STATUS == 1)
      title <- "Bike Pickup Locations"
    } else {
      filtered_data <- df %>% filter(LOCK_STATUS == 0)
      title <- "Bike Return Locations"
    }
    
    heat_data <- filtered_data %>%
      select(LATITUDE, LONGITUDE) %>%
      filter(!is.na(LATITUDE), !is.na(LONGITUDE))
    
    if(input$heatmap_gradient == "viridis") {
      gradient <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
    } else if(input$heatmap_gradient == "magma") {
      gradient <- c("#000004", "#3b0f70", "#8c2981", "#de4968", "#fe9f6d", "#fcfdbf")
    } else if(input$heatmap_gradient == "inferno") {
      gradient <- c("#000004", "#420a68", "#932667", "#dd513a", "#fca50a", "#fcffa4")
    } else {
      gradient <- c("blue", "green", "yellow", "red")
    }
    
    leaflet(heat_data) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        blur = input$heatmap_blur,
        radius = input$heatmap_radius,
        gradient = gradient
      ) %>%
      setView(
        lng = mean(heat_data$LONGITUDE, na.rm = TRUE),
        lat = mean(heat_data$LATITUDE, na.rm = TRUE),
        zoom = 12
      ) %>%
      addControl(
        html = paste("<strong>", title, "</strong>"),
        position = "topright"
      )
  })
  
  output$bike_usage_plot <- renderPlotly({
    df <- bike_data()
    
    usage_counts <- df %>%
      filter(LOCK_STATUS == 1) %>%
      count(BIKE_ID, sort = TRUE) %>%
      head(input$top_n_bikes)
    
    if(input$sort_by_usage) {
      usage_counts <- usage_counts %>%
        mutate(BIKE_ID = reorder(factor(BIKE_ID), n))
    } else {
      usage_counts <- usage_counts %>%
        mutate(BIKE_ID = factor(BIKE_ID))
    }
    
    p <- ggplot(usage_counts, aes(x = BIKE_ID, y = n)) +
      geom_bar(stat = "identity", fill = input$usage_color) +
      theme_minimal() +
      labs(
        title = paste0("Top ", input$top_n_bikes, " Most Used Bikes"),
        x = "Bike ID",
        y = "Number of Times Taken"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(
        hovermode = "closest"
      )
  })
  
  output$taxi_activity_plot <- renderPlot({
    td <- taxi_data()
    selected_metric <- input$activity_metric
    
    if(selected_metric == "Hourly Activity") {
      hourly_activity <- td %>%
        group_by(Hour, OpenStatus) %>%
        summarise(
          TaxiCount = length(unique(VehicleNum)),
          PointCount = n(),
          AvgSpeed = mean(Speed, na.rm = TRUE),
          .groups = "drop"
        )
      
      hourly_activity <- hourly_activity %>%
        filter(Hour >= input$time_range[1], Hour <= input$time_range[2])
      
      if(!input$filter_passenger) {
        hourly_activity <- hourly_activity %>%
          group_by(Hour) %>%
          summarise(
            TaxiCount = sum(TaxiCount),
            PointCount = sum(PointCount),
            AvgSpeed = weighted.mean(AvgSpeed, PointCount, na.rm = TRUE),
            .groups = "drop"
          )
        
        ggplot(hourly_activity, aes(x = Hour, y = PointCount)) +
          geom_bar(stat = "identity", fill = "#f39c12") +
          scale_x_continuous(breaks = seq(input$time_range[1], input$time_range[2], by = 2)) +
          labs(
            title = "Taxi Activity Patterns Throughout the Day",
            subtitle = "GPS point frequency by hour",
            x = "Hour of Day",
            y = "Number of GPS Points"
          ) +
          theme_bw()
      } else {
        ggplot(hourly_activity, aes(x = Hour, y = factor(OpenStatus), fill = PointCount)) +
          geom_tile() +
          scale_fill_gradient2(
            low = "blue", mid = "#f39c12", high = "yellow",
            midpoint = median(hourly_activity$PointCount),
            name = "GPS Points"
          ) +
          scale_x_continuous(breaks = seq(input$time_range[1], input$time_range[2], by = 2)) +
          scale_y_discrete(labels = c("Empty", "With Passenger")) +
          labs(
            title = "Taxi Activity Patterns Throughout the Day",
            subtitle = "GPS point frequency by hour and passenger status",
            x = "Hour of Day",
            y = "Passenger Status"
          ) +
          theme_bw()
      }
    } else if(selected_metric == "Pickup vs Dropoff") {
      status_changes <- td %>%
        filter(Hour >= input$time_range[1], Hour <= input$time_range[2]) %>%
        arrange(VehicleNum, Time) %>%
        group_by(VehicleNum) %>%
        mutate(
          PrevStatus = lag(OpenStatus),
          StatusChange = case_when(
            is.na(PrevStatus) ~ "None",
            OpenStatus == 1 & PrevStatus == 0 ~ "Pickup",
            OpenStatus == 0 & PrevStatus == 1 ~ "Dropoff",
            TRUE ~ "None"
          )
        ) %>%
        filter(StatusChange %in% c("Pickup", "Dropoff")) %>%
        ungroup()
      
      ggplot(status_changes, aes(x = Lng, y = Lat)) +
        stat_bin_hex(aes(fill = ..count..), bins = 30) +
        facet_wrap(~StatusChange, ncol = 2) +
        scale_fill_gradient(low = "#e67e22", high = "yellow", trans = "log10", name = "Count") +
        labs(
          title = "Spatial Distribution of Taxi Pickups and Dropoffs",
          subtitle = "Hexbin density visualization of passenger transitions",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_bw()
    } else if(selected_metric == "Speed Distribution") {
      speed_data <- td %>%
        filter(Hour >= input$time_range[1], Hour <= input$time_range[2], Speed > 0)
      
      if(input$filter_passenger) {
        ggplot(speed_data, aes(x = TimePeriod, y = Speed, fill = PassengerStatus)) +
          geom_violin(position = position_dodge(width = 0.8), alpha = 0.7, trim = TRUE, scale = "width") +
          geom_boxplot(position = position_dodge(width = 0.8), width = 0.2, alpha = 0.4, outlier.shape = NA) +
          coord_flip() +
          scale_fill_manual(values = c("With Passenger" = "#5c6bc0", "Empty" = "#26a69a")) +
          scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
          labs(
            title = "Taxi Speed Distributions by Time of Day",
            subtitle = "Comparing speeds when empty vs. with passengers",
            y = "Speed (km/h)",
            x = "Time Period",
            fill = "Status"
          ) +
          theme_bw()
      } else {
        ggplot(speed_data, aes(x = TimePeriod, y = Speed, fill = TimePeriod)) +
          geom_violin(alpha = 0.8, trim = TRUE) +
          geom_boxplot(width = 0.2, alpha = 0.4, outlier.shape = NA) +
          coord_flip() +
          scale_fill_manual(values = c(
            "Morning Rush (6-10)" = "#42a5f5",   # Blue
            "Midday (10-16)" = "#66bb6a",        # Green
            "Evening Rush (16-20)" = "#ffca28",  # Amber
            "Night (20-6)" = "#5e35b1"           # Deep purple
          )) +
          scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
          labs(
            title = "Taxi Speed Distributions by Time of Day",
            y = "Speed (km/h)",
            x = "Time Period"
          ) +
          theme_bw()
      }
    }
  })
  
  output$taxi_spatial_plot <- renderPlot({
    td <- taxi_data()
    
    filtered_data <- td %>%
      filter(Hour >= input$time_range[1], Hour <= input$time_range[2])
    
    if(input$spatial_view == "Pickup/Dropoff Heatmap") {
      status_changes <- filtered_data %>%
        arrange(VehicleNum, Time) %>%
        group_by(VehicleNum) %>%
        mutate(
          PrevStatus = lag(OpenStatus),
          StatusChange = case_when(
            is.na(PrevStatus) ~ "None",
            OpenStatus == 1 & PrevStatus == 0 ~ "Pickup",
            OpenStatus == 0 & PrevStatus == 1 ~ "Dropoff",
            TRUE ~ "None"
          )
        ) %>%
        filter(StatusChange %in% c("Pickup", "Dropoff")) %>%
        ungroup()
      
      ggplot(status_changes, aes(x = Lng, y = Lat)) +
        stat_bin_hex(aes(fill = ..count..), bins = input$bin_size) +
        facet_wrap(~StatusChange, ncol = 2) +
        scale_fill_viridis(option = "plasma", trans = "log10", name = "Count") +
        labs(
          title = "Spatial Distribution of Taxi Pickups and Dropoffs",
          subtitle = "Hexbin density visualization",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_bw() +
        {if(input$show_district_boundaries) 
          geom_sf(data = NULL, fill = NA, color = "black", size = 0.5)
        }
      
    } else if(input$spatial_view == "Speed by Region") {
      filtered_data_clean <- filtered_data %>%
        filter(!(Lng < 109 & Lat < 15)) 
      
      ggplot(filtered_data_clean, aes(x = Lng, y = Lat, color = Speed)) +
        stat_summary_hex(aes(z = Speed), fun = "mean", bins = input$bin_size) +
        scale_fill_viridis(option = "viridis", name = "Avg Speed (km/h)") +
        coord_fixed(ratio = 1) +  
        labs(
          title = "Average Taxi Speed by Region",
          subtitle = "Shanghai metropolitan area",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_bw() +
        {if(input$show_district_boundaries) 
          geom_sf(data = NULL, fill = NA, color = "black", size = 0.5)
        }
    } else if(input$spatial_view == "Pickup-Dropoff Ratio") {
      status_changes <- filtered_data %>%
        arrange(VehicleNum, Time) %>%
        group_by(VehicleNum) %>%
        mutate(
          PrevStatus = lag(OpenStatus),
          StatusChange = case_when(
            is.na(PrevStatus) ~ "None",
            OpenStatus == 1 & PrevStatus == 0 ~ "Pickup",
            OpenStatus == 0 & PrevStatus == 1 ~ "Dropoff",
            TRUE ~ "None"
          )
        ) %>%
        filter(StatusChange %in% c("Pickup", "Dropoff")) %>%
        ungroup()
      
      hex_data <- status_changes %>%
        mutate(
          bin_lng = round(Lng * input$bin_size) / input$bin_size,
          bin_lat = round(Lat * input$bin_size) / input$bin_size,
          is_pickup = StatusChange == "Pickup"
        ) %>%
        group_by(bin_lng, bin_lat) %>%
        summarise(
          pickup_count = sum(is_pickup),
          dropoff_count = sum(!is_pickup),
          total_count = n(),
          ratio = ifelse(dropoff_count > 0, pickup_count / dropoff_count, NA),
          Lng = bin_lng,
          Lat = bin_lat,
          .groups = "drop"
        ) %>%
        filter(total_count >= 5)
      
      ggplot(hex_data, aes(x = Lng, y = Lat, fill = ratio)) +
        geom_tile() +
        scale_fill_gradient2(
          low = "#1e3799", mid = "yellow", high = "#b71c1c",
          midpoint = 1, 
          name = "Pickup/Dropoff Ratio",
          na.value = "grey50") +
        labs(
          title = "Taxi Pickup to Dropoff Ratio by Area",
          subtitle = "Blue areas have more dropoffs, red areas more pickups",
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_bw()
    }
  })
  
  output$vehicle_analysis_plot <- renderPlot({
    td <- taxi_data()
    
    if(input$vehicle_metric == "Speed Distribution") {
      top_vehicles <- td %>%
        group_by(VehicleNum) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count)) %>%
        head(input$top_n_vehicles) %>%
        pull(VehicleNum)
      
      vehicle_data <- td %>%
        filter(VehicleNum %in% top_vehicles, Speed > 0)
      
      ggplot(vehicle_data, aes(x = reorder(as.factor(VehicleNum), Speed, FUN = median), y = Speed, fill = PassengerStatus)) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_manual(values = c("With Passenger" = "salmon", "Empty" = "green")) +
        coord_flip() +
        labs(
          title = "Speed Distribution by Vehicle",
          subtitle = paste("Top", input$top_n_vehicles, "vehicles by data points"),
          x = "Vehicle ID",
          y = "Speed (km/h)",
          fill = "Passenger Status"
        ) +
        theme_bw()
      
    } else if(input$vehicle_metric == "Activity Levels") {
      activity_data <- td %>%
        group_by(VehicleNum, Hour) %>%
        summarise(
          point_count = n(),
          avg_speed = mean(Speed, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(VehicleNum) %>%
        mutate(total_points = sum(point_count)) %>%
        ungroup() %>%
        arrange(desc(total_points))
      
      top_vehicles <- unique(activity_data$VehicleNum)[1:input$top_n_vehicles]
      
      top_activity <- activity_data %>%
        filter(VehicleNum %in% top_vehicles)
      
      ggplot(top_activity, aes(x = Hour, y = reorder(as.factor(VehicleNum), total_points), fill = point_count)) +
        geom_tile() +
        scale_fill_viridis(option = "plasma", name = "GPS Points") +
        scale_x_continuous(breaks = 0:23) +
        labs(
          title = "Taxi Activity Patterns by Hour of Day",
          subtitle = paste("Top", input$top_n_vehicles, "most active vehicles"),
          x = "Hour of Day",
          y = "Vehicle ID"
        ) +
        theme_bw()
      
    } else if(input$vehicle_metric == "Direction Ratio") {
      direction_data <- td %>%
        arrange(VehicleNum, Time) %>%
        group_by(VehicleNum) %>%
        mutate(
          prev_lng = lag(Lng),
          prev_lat = lag(Lat),
          delta_lng = Lng - prev_lng,
          delta_lat = Lat - prev_lat,
          direction = case_when(
            is.na(delta_lng) | is.na(delta_lat) ~ NA_character_,
            abs(delta_lng) > abs(delta_lat) & delta_lng > 0 ~ "East",
            abs(delta_lng) > abs(delta_lat) & delta_lng < 0 ~ "West",
            abs(delta_lat) > abs(delta_lng) & delta_lat > 0 ~ "North",
            abs(delta_lat) > abs(delta_lng) & delta_lat < 0 ~ "South",
            TRUE ~ "Stationary"
          )
        ) %>%
        filter(!is.na(direction), direction != "Stationary") %>%
        ungroup()
      
      top_vehicles <- direction_data %>%
        group_by(VehicleNum) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count)) %>%
        head(input$top_n_vehicles) %>%
        pull(VehicleNum)
      
      vehicle_directions <- direction_data %>%
        filter(VehicleNum %in% top_vehicles) %>%
        group_by(VehicleNum, direction) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(VehicleNum) %>%
        mutate(
          proportion = count / sum(count),
          total = sum(count)
        ) %>%
        ungroup()
      
      ggplot(vehicle_directions, aes(x = reorder(as.factor(VehicleNum), total), y = proportion, fill = direction)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Set2") +
        coord_flip() +
        labs(
          title = "Movement Direction Distribution by Vehicle",
          subtitle = paste("Top", input$top_n_vehicles, "vehicles by movement data points"),
          x = "Vehicle ID",
          y = "Proportion of Movements",
          fill = "Direction"
        ) +
        theme_bw()
    }
  })
  
  output$summary_statistics <- renderDT({
    if(input$data_source == "Taxi Data") {
      td <- taxi_data()
      
      if(input$taxi_summary_var == "Speed") {
        summary_data <- td %>%
          filter(Speed > 0) %>%
          group_by(!!sym(input$taxi_group_var)) %>%
          summarise(
            Mean = mean(Speed, na.rm = TRUE),
            Median = median(Speed, na.rm = TRUE),
            `Standard Deviation` = sd(Speed, na.rm = TRUE),
            `First Quantile` = quantile(Speed, 0.25, na.rm = TRUE),
            `Third Quantile` = quantile(Speed, 0.75, na.rm = TRUE),
            Minimum = min(Speed, na.rm = TRUE),
            Maximum = max(Speed, na.rm = TRUE),
            Count = n(),
            .groups = "drop"
          )
      } else if(input$taxi_summary_var == "OpenStatus") {
        summary_data <- td %>%
          group_by(!!sym(input$taxi_group_var)) %>%
          summarise(
            `With Passenger (%)` = mean(OpenStatus) * 100,
            `Empty (%)` = (1 - mean(OpenStatus)) * 100,
            `Total Records` = n(),
            .groups = "drop"
          )
      } else if(input$taxi_summary_var == "Hour") {
        summary_data <- td %>%
          group_by(!!sym(input$taxi_group_var), Hour) %>%
          summarise(Count = n(), .groups = "drop") %>%
          group_by(!!sym(input$taxi_group_var)) %>%
          summarise(
            `Peak Hour` = Hour[which.max(Count)],
            `Minimum Hour` = Hour[which.min(Count)],
            `Peak Count` = max(Count),
            `Minimum Count` = min(Count),
            `Total Records` = sum(Count),
            .groups = "drop"
          )
      }
    } else if(input$data_source == "Bus GPS") {
      bd <- busgps_df()
      
      if(input$bus_summary_var == "hour_of_day") {
        summary_data <- bd %>%
          group_by(!!sym(input$bus_group_var), hour_of_day) %>%
          summarise(Count = n(), .groups = "drop") %>%
          group_by(!!sym(input$bus_group_var)) %>%
          summarise(
            `Peak Hour` = hour_of_day[which.max(Count)],
            `Minimum Hour` = hour_of_day[which.min(Count)],
            `Peak Count` = max(Count),
            `Minimum Count` = min(Count),
            `Total Records` = sum(Count),
            .groups = "drop"
          )
      } else if(input$bus_summary_var == "ToDir") {
        summary_data <- bd %>%
          group_by(!!sym(input$bus_group_var), ToDir) %>%
          summarise(Count = n(), .groups = "drop") %>%
          group_by(!!sym(input$bus_group_var)) %>%
          summarise(
            `Direction 0 Count` = sum(Count[ToDir == 0]),
            `Direction 1 Count` = sum(Count[ToDir == 1]),
            `Direction 0 (%)` = sum(Count[ToDir == 0]) / sum(Count) * 100,
            `Direction 1 (%)` = sum(Count[ToDir == 1]) / sum(Count) * 100,
            `Total Records` = sum(Count),
            .groups = "drop")
      }
    } else {
      bike_df <- bike_data()
      trips_df <- bike_trips()
      
      if(input$bike_summary_var == "Hour") {
        summary_data <- bike_df %>%
          group_by(!!sym(input$bike_group_var), Hour) %>%
          summarise(Count = n(), .groups = "drop") %>%
          group_by(!!sym(input$bike_group_var)) %>%
          summarise(
            `Peak Hour` = Hour[which.max(Count)],
            `Minimum Hour` = Hour[which.min(Count)],
            `Peak Count` = max(Count),
            `Minimum Count` = min(Count),
            `Total Records` = sum(Count),
            .groups = "drop"
          )
      } else if(input$bike_summary_var == "LOCK_STATUS") {
        summary_data <- bike_df %>%
          group_by(!!sym(input$bike_group_var)) %>%
          summarise(
            `Locks (Status 1) Count` = sum(LOCK_STATUS == 1),
            `Unlocks (Status 0) Count` = sum(LOCK_STATUS == 0),
            `Locks (%)` = mean(LOCK_STATUS) * 100,
            `Unlocks (%)` = (1 - mean(LOCK_STATUS)) * 100,
            `Total Records` = n(),
            .groups = "drop"
          )
      } else if(input$bike_summary_var == "Duration") {
        if(input$bike_group_var == "BIKE_ID") {
          summary_data <- trips_df %>%
            group_by(!!sym(input$bike_group_var)) %>%
            summarise(
              `Mean Duration (min)` = mean(duration, na.rm = TRUE),
              `Median Duration (min)` = median(duration, na.rm = TRUE),
              `Min Duration (min)` = min(duration, na.rm = TRUE),
              `Max Duration (min)` = max(duration, na.rm = TRUE),
              `Trip Count` = n(),
              .groups = "drop"
            ) %>%
            arrange(desc(`Trip Count`))
        } else {
          summary_data <- trips_df %>%
            group_by(!!sym(input$bike_group_var)) %>%
            summarise(
              `Mean Duration (min)` = mean(duration, na.rm = TRUE),
              `Median Duration (min)` = median(duration, na.rm = TRUE),
              `Min Duration (min)` = min(duration, na.rm = TRUE),
              `Max Duration (min)` = max(duration, na.rm = TRUE),
              `Trip Count` = n(),
              .groups = "drop"
            )
        }
      }
    }
    
    datatable(summary_data, 
              options = list(
                pageLength = 10, 
                autoWidth = FALSE,
                scrollX = TRUE,
                fixedColumns = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE,
              class = 'cell-border stripe'
    ) %>%
      formatRound(columns = names(summary_data)[2:ncol(summary_data)], digits = 2)
  })
  
  output$analysis_plot <- renderPlot({
    if(input$analysis_type == "Taxi Speed Analysis") {
      td <- taxi_data()
      
      filtered_data <- td %>%
        filter(TimePeriod %in% input$time_periods)
      
      if(input$split_by_passenger) {
        ggplot(filtered_data, aes(x = TimePeriod, y = Speed, fill = PassengerStatus)) +
          geom_violin(position = position_dodge(0.8), alpha = 0.7, trim = TRUE) +
          geom_boxplot(position = position_dodge(0.8), width = 0.2, alpha = 0.4, outlier.shape = NA) +
          coord_flip() +
          theme_bw() +
          labs(
            title = "Taxi Speed Analysis by Time Period",
            subtitle = "Comparing speeds when empty vs. with passengers",
            y = "Speed (km/h)",
            x = "Time Period",
            fill = "Passenger Status"
          )
      } else {
        ggplot(filtered_data, aes(x = TimePeriod, y = Speed)) +
          geom_violin(fill = "turquoise", alpha = 0.7, trim = TRUE) +
          geom_boxplot(width = 0.2, alpha = 0.4, outlier.shape = NA) +
          coord_flip() +
          theme_bw() +
          labs(
            title = "Taxi Speed Analysis by Time Period",
            y = "Speed (km/h)",
            x = "Time Period"
          )
      }
      
    } else if(input$analysis_type == "Bus Activity Patterns") {
      bd <- busgps_df()
      
      filtered_data <- bd %>%
        filter(hour_of_day >= input$hour_range[1], hour_of_day <= input$hour_range[2])
      
      top_vehicles <- filtered_data %>%
        group_by(VehicleId) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count)) %>%
        head(input$top_n_vehicles) %>%
        pull(VehicleId)
      
      vehicle_hours <- filtered_data %>%
        filter(VehicleId %in% top_vehicles) %>%
        group_by(VehicleId, hour_of_day) %>%
        summarise(count = n(), .groups = "drop")
      
      ggplot(vehicle_hours, aes(x = hour_of_day, y = VehicleId, fill = count)) +
        geom_tile() +
        scale_fill_viridis(option = "plasma", name = "GPS Points") +
        scale_x_continuous(breaks = seq(input$hour_range[1], input$hour_range[2], by = 2)) +
        theme_bw() +
        labs(
          title = "Bus Activity Patterns by Hour of Day",
          subtitle = paste("Top", input$top_n_vehicles, "most active vehicles"),
          x = "Hour of Day",
          y = "Vehicle ID"
        )
      
    } else if(input$analysis_type == "Pickup/Dropoff Analysis") {
      td <- taxi_data()
      
      status_changes <- td %>%
        arrange(VehicleNum, Time) %>%
        group_by(VehicleNum) %>%
        mutate(
          PrevStatus = lag(OpenStatus),
          StatusChange = case_when(
            is.na(PrevStatus) ~ "None",
            OpenStatus == 1 & PrevStatus == 0 ~ "Pickup",
            OpenStatus == 0 & PrevStatus == 1 ~ "Dropoff",
            TRUE ~ "None"
          )
        ) %>%
        filter(StatusChange %in% c("Pickup", "Dropoff")) %>%
        ungroup()
      
      if(input$pickup_viz_type == "Heatmap") {
        ggplot(status_changes, aes(x = Lng, y = Lat)) +
          stat_bin_hex(aes(fill = ..count..), bins = input$bin_resolution) +
          facet_wrap(~StatusChange, ncol = 2) +
          scale_fill_viridis(option = "inferno", trans = "log10", name = "Count") +
          theme_bw() +
          labs(
            title = "Spatial Distribution of Taxi Pickups and Dropoffs",
            subtitle = "Hexbin density visualization",
            x = "Longitude",
            y = "Latitude"
          )
      } else if(input$pickup_viz_type == "Scatter") {
        set.seed(123)
        
        sample_size <- min(nrow(status_changes), input$bin_resolution * 20)
        
        sampled_data <- status_changes %>%
          sample_n(sample_size)
        
        ggplot(sampled_data, aes(x = Lng, y = Lat, color = StatusChange)) +
          geom_point(alpha = 0.6, size = 2) +
          scale_color_manual(values = c("Pickup" = "#40E0D0", "Dropoff" = "#FA8072")) +
          theme_bw() +
          labs(
            title = "Taxi Pickup and Dropoff Locations",
            subtitle = paste0("Sample of ", sample_size, " points (Resolution: ", input$bin_resolution, ")"),
            x = "Longitude",
            y = "Latitude",
            color = "Event Type"
          )
      } else if(input$pickup_viz_type == "Ratio Analysis") {
        ratio_by_time <- status_changes %>%
          group_by(TimePeriod, StatusChange) %>%
          summarise(count = n(), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = StatusChange, values_from = count) %>%
          mutate(
            ratio = Pickup / Dropoff,
            difference = Pickup - Dropoff
          )
        
        ggplot(ratio_by_time, aes(x = TimePeriod, y = ratio, fill = TimePeriod)) +
          geom_bar(stat = "identity") +
          geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
          scale_color_manual(values = c("Morning Rush (6-10)" = "#f1c40f",
                                        "Midday (10-16)" = "#e67e22",
                                        "Evening Rush (16-20)" = "#e74c3c",
                                        "Night (20-6)" = "#3498db")) +
          theme_bw() +
          labs(
            title = "Pickup to Dropoff Ratio by Time Period",
            subtitle = "Values > 1 indicate more pickups than dropoffs",
            x = "Time Period",
            y = "Pickup/Dropoff Ratio"
          )
      }
    } else if(input$analysis_type == "Vehicle Comparison") {
      if(input$vehicle_data == "Taxi") {
        td <- taxi_data()
        
        if(input$comparison_metric == "Speed") {
          filtered_td <- td
          if(input$exclude_zero_speed) {
            filtered_td <- td %>% filter(Speed > 0)
          }
          
          ggplot(filtered_td, aes(x = Speed, fill = TimePeriod)) +
            geom_histogram(bins = 30, color = "black", alpha = 0.7) +
            facet_wrap(~TimePeriod) +
            scale_fill_manual(values = c(
              "Morning Rush (6-10)" = "#f9d71c",
              "Midday (10-16)" = "#f39c12",
              "Evening Rush (16-20)" = "#e74c3c",
              "Night (20-6)" = "#2c3e50"
            )) +
            theme_bw() +
            labs(
              title = "Taxi Speed Distribution by Time Period",
              subtitle = ifelse(input$exclude_zero_speed, "Excluding zero speeds", "Including all speeds"),
              x = "Speed (km/h)",
              y = "Count"
            )
        } else if(input$comparison_metric == "Activity") {
          activity_by_time <- td %>%
            group_by(TimePeriod, Hour) %>%
            summarise(count = n(), .groups = "drop")
          
          ggplot(activity_by_time, aes(x = Hour, y = count, color = TimePeriod)) +
            geom_line(size = 1.2) +
            geom_point(size = 3) +
            scale_color_manual(values = c("Morning Rush (6-10)" = "#f1c40f",
                                          "Midday (10-16)" = "#e67e22",
                                          "Evening Rush (16-20)" = "#e74c3c",
                                          "Night (20-6)" = "#3498db")) +
            theme_bw() +
            labs(
              title = "Taxi Activity by Hour and Time Period",
              x = "Hour of Day",
              y = "Number of GPS Points",
              color = "Time Period"
            )
        } else if(input$comparison_metric == "Patterns") {
          passenger_patterns <- td %>%
            group_by(TimePeriod, Hour, PassengerStatus) %>%
            summarise(count = n(), .groups = "drop") %>%
            group_by(TimePeriod, Hour) %>%
            mutate(proportion = count / sum(count)) %>%
            filter(PassengerStatus == "With Passenger")
          
          ggplot(passenger_patterns, aes(x = Hour, y = proportion, color = TimePeriod)) +
            geom_line(size = 1.2) +
            geom_point(size = 2) +
            scale_color_manual(values = c("Morning Rush (6-10)" = "#f1c40f",
                                          "Midday (10-16)" = "#e67e22",
                                          "Evening Rush (16-20)" = "#e74c3c",
                                          "Night (20-6)" = "#3498db")) +
            scale_y_continuous(labels = scales::percent) +
            theme_bw() +
            labs(
              title = "Proportion of Taxis with Passengers by Hour",
              subtitle = "Comparison across time periods",
              x = "Hour of Day",
              y = "Proportion with Passengers",
              color = "Time Period")
        }
      } else {
        bd <- busgps_df()
        
        if(input$comparison_metric == "Speed") {
          filtered_bd <- bd %>%
            filter(ToDir %in% c(0, 1))
          
          ggplot(filtered_bd, aes(x = hour_of_day, fill = factor(ToDir))) +
            geom_histogram(bins = 24, alpha = 0.7, position = "dodge") +
            scale_fill_manual(values = c("0" = "#1e88e5", "1" = "#f44336"), name = "Direction") +
            theme_bw() +
            labs(
              title = "Bus Activity by Hour and Direction",
              subtitle = "Comparing directions 0 and 1 only",
              x = "Hour of Day",
              y = "Number of GPS Points"
            )
        } else if(input$comparison_metric == "Activity") {
          top_vehicles <- bd %>%
            group_by(VehicleId) %>%
            summarise(count = n(), .groups = "drop") %>%
            arrange(desc(count)) %>%
            head(10) %>%
            pull(VehicleId)
          
          vehicle_activity <- bd %>%
            filter(VehicleId %in% top_vehicles) %>%
            group_by(VehicleId, hour_of_day) %>%
            summarise(count = n(), .groups = "drop")
          
          ggplot(vehicle_activity, aes(x = hour_of_day, y = count, color = VehicleId)) +
            geom_line(size = 1) +
            scale_color_viridis_d(option = "turbo") +
            theme_bw() +
            labs(
              title = "Bus Activity Patterns by Vehicle",
              subtitle = "Top 10 most active vehicles",
              x = "Hour of Day",
              y = "Number of GPS Points",
              color = "Vehicle ID"
            )
        } else if(input$comparison_metric == "Patterns") {
          direction_patterns <- bd %>%
            group_by(hour_of_day, ToDir) %>%
            summarise(count = n(), .groups = "drop") %>%
            group_by(hour_of_day) %>%
            mutate(proportion = count / sum(count))
          
          ggplot(direction_patterns, aes(x = hour_of_day, y = proportion, fill = factor(ToDir))) +
            geom_area(alpha = 0.7) +
            scale_fill_manual(values = c("0" = "turquoise", "1" = "#e74c3c"), name = "Direction") +
            scale_y_continuous(labels = scales::percent) +
            theme_bw() +
            labs(
              title = "Proportion of Buses by Direction Throughout the Day",
              x = "Hour of Day",
              y = "Proportion",
              fill = "Direction"
            )
        }
      }
    } else if(input$analysis_type == "Bike Activity Analysis") {
      bike_df <- bike_data()
      trips_df <- bike_trips()
      
      filtered_trips <- trips_df %>%
        filter(duration <= input$max_bike_duration)
      
      filtered_bike_df <- bike_df %>%
        filter(TimePeriod %in% input$bike_time_periods)
      
      p1 <- filtered_bike_df %>%
        group_by(Hour, LOCK_STATUS) %>%
        summarise(Count = n(), .groups = "drop") %>%
        mutate(Status = ifelse(LOCK_STATUS == 1, "Lock (Bike Taken)", "Unlock (Bike Returned)")) %>%
        ggplot(aes(x = Hour, y = Count, fill = Status)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Lock (Bike Taken)" = "#F79054", "Unlock (Bike Returned)" = "#0D4262")) +
        scale_x_continuous(breaks = seq(0, 23, 3)) +
        theme_bw() +
        labs(
          title = "Bike Lock/Unlock Activity by Hour",
          x = "Hour of Day",
          y = "Number of Events"
        )
      
      p2 <- ggplot(filtered_trips, aes(x = duration)) +
        geom_histogram(bins = 30, fill = "#095370", color = "black", alpha = 0.7) +
        geom_density(aes(y = ..count..), color = "red", size = 1) +
        theme_bw() +
        labs(
          title = "Trip Duration Distribution",
          subtitle = paste0("Trips up to ", input$max_bike_duration, " minutes"),
          x = "Duration (minutes)",
          y = "Number of Trips"
        )
      
      gridExtra::grid.arrange(p1, p2, ncol = 1, 
                              heights = c(1.2, 1),
                              top = grid::textGrob("Bike Activity Analysis", 
                                                   gp = grid::gpar(fontsize = 16, font = 2)))
    } else if(input$analysis_type == "Multi-Modal Comparison") {
      selected_modes <- input$transport_modes
      selected_period <- input$comparison_period
      
      combined_activity <- data.frame()
      
      if("Taxi" %in% selected_modes) {
        td <- taxi_data()
        if(selected_period != "Full Day") {
          period_mapping <- c(
            "Morning Rush" = "Morning Rush (6-10)",
            "Evening Rush" = "Evening Rush (16-20)"
          )
          td <- td %>% filter(TimePeriod %in% period_mapping[[selected_period]])
        }
        if(nrow(td) > 0) {
          taxi_hourly <- td %>%
            group_by(Hour) %>%
            summarise(Count = n(), .groups = "drop") %>%
            mutate(Mode = "Taxi")
          combined_activity <- bind_rows(combined_activity, taxi_hourly)
        }
      }
      
      if("Bus" %in% selected_modes) {
        bd <- busgps_df()
        if(selected_period != "Full Day") {
          hour_ranges <- list(
            "Morning Rush" = 6:10,
            "Evening Rush" = 16:20,
            "Off-Peak" = c(0:6, 10:16, 20:23)
          )
          bd <- bd %>% filter(hour_of_day %in% hour_ranges[[selected_period]])
        }
        if(nrow(bd) > 0) {
          bus_hourly <- bd %>%
            group_by(hour_of_day) %>%
            summarise(Count = n(), .groups = "drop") %>%
            rename(Hour = hour_of_day) %>%
            mutate(Mode = "Bus")
          combined_activity <- bind_rows(combined_activity, bus_hourly)
        }
      }
      
      if("Bike" %in% selected_modes) {
        bike_df <- bike_data()
        if(selected_period != "Full Day") {
          period_mapping <- c(
            "Morning Rush" = "Morning (6-10)",
            "Evening Rush" = "Evening (16-20)",
            "Off-Peak" = c("Midday (10-16)", "Night (20-6)")
          )
          bike_df <- bike_df %>% filter(TimePeriod %in% period_mapping[[selected_period]])
        }
        if(nrow(bike_df) > 0) {
          bike_hourly <- bike_df %>%
            group_by(Hour) %>%
            summarise(Count = n(), .groups = "drop") %>%
            mutate(Mode = "Bike")
          combined_activity <- bind_rows(combined_activity, bike_hourly)
        }
      }
      
      validate(
        need(nrow(combined_activity) > 0,
             paste0("Selected transport mode(s) do not have activity data for the time period '", selected_period, "' in the dataset."))
      )
      
      activity_normalized <- combined_activity %>%
        group_by(Mode) %>%
        mutate(
          Normalized = Count / max(Count),
          Total = sum(Count)
        ) %>%
        ungroup()
      
      ggplot(activity_normalized, aes(x = Hour, y = Normalized, color = Mode)) +
        geom_line(size = 1.2) +
        geom_point(size = 2.5, alpha = 0.7) +
        scale_color_manual(values = c("Taxi" = "#f39c12", "Bus" = "#3498db", "Bike" = "#2ecc71")) +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        scale_y_continuous(labels = scales::percent) +
        theme_bw() +
        theme(legend.position = "top") +
        labs(
          title = "Transportation Mode Activity Comparison",
          subtitle = paste0("Time period: ", selected_period),
          x = "Hour of Day",
          y = "Normalized Activity Level",
          color = "Transport Mode"
        )
    }
  })
  
  output$analysis_summary <- renderText({
    if(input$analysis_type == "Taxi Speed Analysis") {
      periods <- paste(input$time_periods, collapse = ", ")
      if(input$split_by_passenger) {
        "The analysis compares taxi speeds between passenger status (empty vs. with passengers) across different time periods. Note how speeds tend to be higher during night hours and when taxis are empty."
      } else {
        "The analysis shows the distribution of taxi speeds across different time periods. Note how speeds tend to be higher during night hours due to reduced traffic congestion."
      }
    } else if(input$analysis_type == "Bus Activity Patterns") {
      paste0("This visualization shows activity patterns for the top ", input$top_n_vehicles, 
             " most active buses between hours ", input$hour_range[1], " and ", input$hour_range[2], 
             ". Darker colors indicate more GPS points recorded, suggesting higher activity.")
    } else if(input$analysis_type == "Pickup/Dropoff Analysis") {
      if(input$pickup_viz_type == "Heatmap") {
        "The heatmap shows the spatial distribution of taxi pickup and dropoff events. Brighter colors indicate higher concentration of events. Notice that pickup and dropoff patterns tend to cluster in different areas."
      } else if(input$pickup_viz_type == "Scatter") {
        "This scatter plot shows individual pickup and dropoff locations. The visualization helps identify areas with high taxi activity and potential imbalances between pickups and dropoffs."
      } else {
        "The ratio analysis compares the number of pickups to dropoffs across time periods. A ratio above 1 indicates more pickups than dropoffs in that time period, suggesting passenger flow into those areas."
      }
    } else if(input$analysis_type == "Vehicle Comparison") {
      if(input$vehicle_data == "Taxi") {
        if(input$comparison_metric == "Speed") {
          "This analysis shows how taxi speeds are distributed across different time periods. Note the variations in speed patterns between peak hours and off-peak hours."
        } else if(input$comparison_metric == "Activity") {
          "The activity analysis shows how taxi activity levels vary throughout the day across different time periods. Notice the clear peaks during morning and evening rush hours."
        } else {
          "This pattern analysis shows how the proportion of taxis with passengers changes throughout the day. Higher percentages indicate higher utilization of taxis."
        }
      } else {
        if(input$comparison_metric == "Speed") {
          "Since speed data is not available for buses, this visualization shows bus activity patterns by hour for different directions. Note how activity varies between directions throughout the day."
        } else if(input$comparison_metric == "Activity") {
          "This analysis compares activity patterns across different bus vehicles. Some buses show consistent activity throughout the day, while others have clear peak periods."
        } else {
          "This visualization shows how the proportion of buses traveling in each direction changes throughout the day, indicating passenger flow patterns."
        }
      }
    } else if(input$analysis_type == "Bike Activity Analysis") {
      paste0("This analysis examines bike-sharing patterns across ", paste(input$bike_time_periods, collapse=", "), 
             " periods. The top chart shows hourly lock/unlock activity, with peaks during commute hours. ",
             "The bottom chart displays the distribution of trip durations up to ", input$max_bike_duration, 
             " minutes, with most trips lasting 5-15 minutes, suggesting bikes are primarily used for short-distance travel.")
    } else if(input$analysis_type == "Multi-Modal Comparison") {
      paste0("This visualization compares the normalized hourly activity patterns of ", 
             paste(input$transport_modes, collapse=", "), " during ", input$comparison_period, 
             ". Each mode's activity is normalized to its peak to show relative usage patterns throughout the day. ",
             "Note how different transportation modes show varying peak usage times, reflecting their different roles in Shanghai's transportation ecosystem.")
    }
  })
  
  
  output$report <- downloadHandler(
    filename = function() {
      paste0("shanghai-transportation-report-", Sys.Date(), ".html")
    },
    content = function(file) {
      library(glue)
      library(rmarkdown)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      report_template <- '
---
title: "Shanghai Transportation Analysis Report"
date: \'`r format(Sys.time(), "%d %B, %Y")`\'
output: 
  html_document:
    theme: united
    highlight: tango
    toc: true
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(viridis)
```

# Shanghai Transportation System Analysis

This report presents an analysis of transportation system data of Shanghai, focusing on patterns and insights from the <<mode>> data.

## Analysis Methodology

The analysis focuses on <<method>>, examining how <<metrics>> vary across different <<dimensions>>.

## Key Findings

<<findings>>

## Recommendations

1. <<rec1>>
2. <<rec2>>
3. <<rec3>>

## Conclusion

<<conclusion>>

---

*Report generated on `r format(Sys.time(), "%d %B, %Y")` by the analysis of Levon Gevorgyan and Albert Hakobyan Transportation Dashboard*
'
      
      report_content <- list(
        mode = "transportation",
        method = "usage patterns and efficiency metrics",
        metrics = "transportation metrics",
        dimensions = "times and locations",
        findings = "The analysis reveals clear patterns in transportation usage across Shanghai, with distinct temporal and spatial variations that can inform service optimization.",
        rec1 = "Develop data-driven approaches to transportation planning",
        rec2 = "Implement real-time monitoring and response systems",
        rec3 = "Focus on improving intermodal connectivity",
        conclusion = "Shanghai's transportation system shows both strengths and opportunities for improvement. Continued data analysis and evidence-based planning will be essential for addressing future mobility challenges in this rapidly evolving urban environment."
      )
      
      if (input$analysis_type == "Taxi Speed Analysis") {
        report_content <- list(
          mode = "taxi",
          method = "speed distributions across time periods",
          metrics = "taxi speeds",
          dimensions = "time periods and passenger statuses",
          findings = "Taxi speeds show significant variation throughout the day, with highest average speeds during night hours and lowest during morning and evening rush hours. Taxis without passengers tend to move faster than those with passengers.",
          rec1 = "Optimize taxi allocations during peak hours to high-demand areas",
          rec2 = "Consider dedicated taxi lanes in congested areas during rush hours",
          rec3 = "Monitor and analyze taxi speed patterns to identify traffic bottlenecks",
          conclusion = "Understanding taxi speed patterns is crucial for optimizing the taxi fleet and improving service efficiency. The analysis reveals clear patterns in how traffic conditions and passenger status affect taxi speeds across different times of the day."
        )
      } else if (input$analysis_type == "Bus Activity Patterns") {
        report_content <- list(
          mode = "bus",
          method = "bus activity patterns",
          metrics = "bus frequencies and schedules",
          dimensions = "times of day and routes",
          findings = "Bus activity shows clear peaks during morning and evening rush hours. Some routes show more consistent activity throughout the day, while others are mainly active during peak hours.",
          rec1 = "Adjust bus schedules to better match demand patterns across different times of day",
          rec2 = "Increase bus frequency on high-demand routes during peak hours",
          rec3 = "Consider express bus services for the busiest corridors",
          conclusion = "Bus service optimization based on activity patterns can significantly improve the efficiency of Shanghai's public transportation system. The data suggests opportunities for schedule adjustments to better match demand patterns."
        )
      } else if (input$analysis_type == "Bike Activity Analysis") {
        report_content <- list(
          mode = "bike-sharing",
          method = "bike usage patterns and trip durations",
          metrics = "bike pickups and returns",
          dimensions = "locations and times",
          findings = "Bike sharing shows distinct usage patterns, with peak activity during morning and evening commute hours. Most trips are short-duration (5–15 minutes), suggesting bikes are primarily used for first/last mile connections.",
          rec1 = "Increase bike availability in high-demand areas during peak hours",
          rec2 = "Implement dynamic rebalancing strategies based on usage patterns",
          rec3 = "Expand bike parking facilities near transportation hubs",
          conclusion = "The bike-sharing system serves as an important component of Shanghai's transportation ecosystem, particularly for short-distance travel and connections to other transit modes. Understanding spatial and temporal patterns in bike usage can inform system optimization and expansion strategies."
        )
      } else if (input$analysis_type == "Multi-Modal Comparison") {
        report_content <- list(
          mode = "multi-modal transportation",
          method = "comparative usage patterns across transportation modes",
          metrics = "activity levels of different transport modes",
          dimensions = "times of day",
          findings = "Different transportation modes show complementary usage patterns throughout the day. Taxis show more consistent usage across all hours, while buses and bikes show stronger peaks during commute hours.",
          rec1 = "Develop integrated planning strategies that leverage the complementary nature of different transport modes",
          rec2 = "Implement coordinated scheduling across modes to facilitate transfers",
          rec3 = "Create multi-modal transportation hubs at key locations",
          conclusion = "A comprehensive understanding of how different transportation modes work together is essential for effective urban mobility planning. The analysis highlights the complementary nature of Shanghai's transportation options and the importance of integrated planning approaches."
        )
      }
      
      filled_report <- glue::glue_data(report_content, report_template, .open = "<<", .close = ">>")
      
      writeLines(filled_report, tempReport)
      
      rmarkdown::render(tempReport, output_file = file, envir = new.env())
    }
  )
  
}

shinyApp(ui = ui, server = server)