# ==========================================
# 1. SETUP & LIBRARIES
# ==========================================
library(shiny)
library(leaflet)
library(plotly)
library(shinyWidgets) 
library(sf)
library(dplyr)
library(tidyr)

# ==========================================
# 2. DATA LOADING (Runs once at startup)
# ==========================================
# Load the master dataset
fitness_data <- read.csv("final_global_fitness_data.csv")

# Level 1 = Bottom 25% of countries, Level 4 = Top 25% of countries
fitness_data$urban_level <- ntile(fitness_data$Urbanization_., 4)

# New Math (Gym vs Running)
fitness_data$gym_vs_run_metric <- fitness_data$Gym - fitness_data$Running

# Collapse the 5,000+ rows into your ~80 target rows to eliminate map lag
country_summary <- fitness_data %>%
  group_by(Country) %>% 
  summarise(
    gym_vs_run_metric = mean(gym_vs_run_metric, na.rm = TRUE),
    urban_level = first(urban_level),
    .groups = 'drop'
  )

# Load the optimized global map we built
world_shapes <- readRDS("clean_global_map.rds")

# Merge the map with the collapsed data using the 2-letter codes
map_data <- left_join(world_shapes, country_summary, by = c("ISO_A2_EH" = "Country")) %>%
  st_transform(4326)

valid_names <- sort(unique(map_data$NAME[!is.na(map_data$gym_vs_run_metric)]))

# ==========================================
# 3. THE UI
# ==========================================
ui <- fluidPage(
  tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-top: 15px; margin-bottom: 20px;",
    h2("Global Fitness Trends: How Exercise Habits Are Changing Worldwide", style = "margin: 0px; font-weight: bold;"),
    actionButton("help_btn", "Help", icon = icon("question-circle"), class = "btn-custom-blue", style = "font-weight: bold; padding: 6px 15px;")
  ),
  
  tags$style(HTML("
    .recalculating { opacity: 1 !important; }
    
    @keyframes bounce {
      0%, 100% { transform: translateY(0); }
      50% { transform: translateY(-8px); }
    }
    
    /* Custom button colors matching the map palette */
    .btn-custom-blue {
      background-color: #f8f9fa;
      color: #333333;
      border-color: #dcdcdc;
      box-shadow: none !important;
    }
    
    /* Active and Hover states */
    .btn-custom-blue.active, .btn-custom-blue:active, .btn-custom-blue:hover {
      background-color: #3182bd !important;
      color: #ffffff !important;
      border-color: #3182bd !important;
    }
    
    /* Strips the hidden Bootstrap margins to fix vertical alignment */
    .perfect-align .form-group {
      margin-bottom: 0px !important;
    }
  ")),
  
  fluidRow(
    # ---------------- LEFT PANEL: MAP ----------------
    column(width = 7, style = "padding-right: 5px;",
           tags$div(
             style = "border: 1px solid #dcdcdc; border-radius: 6px; padding: 15px; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
             
             h4("The Global Fitness Landscape: Gym vs. Running", style = "margin-top: 0px;"), 
             
             leafletOutput("world_map", height = "600px"),
             
             # Custom External Legend & Controls Area
             tags$div(
               style = "margin-top: 15px; display: flex; justify-content: space-between; align-items: flex-start;",
               
               # LEFT SIDE OF ROW: The Legends
               tags$div(
                 style = "display: flex; gap: 30px;", 
                 
                 # Legend 1: Diverging Gradient Bar
                 tags$div(
                   style = "display: flex; flex-direction: column;",
                   tags$span(style = "font-weight: bold; font-size: 13px; margin-bottom: 4px;", "Gym vs Running"),
                   tags$div(style = "width: 150px; height: 12px; background: linear-gradient(to right, #e6550d, #ffffff, #3182bd); border: 1px solid #ccc;"),
                   tags$div(style = "display: flex; justify-content: space-between; font-size: 11px; margin-top: 2px;",
                            tags$span("More Running"), tags$span("More Gym"))
                 ),
                 
                 # Legend 2: Categorical Grey Boxes
                 tags$div(
                   style = "display: flex; flex-direction: column; gap: 5px; margin-top: 2px;",
                   tags$div(
                     style = "display: flex; align-items: center;",
                     tags$div(style = "width: 14px; height: 14px; background-color: #cccccc; border: 1px solid #fff; box-shadow: 0 0 2px #888; margin-right: 6px;"),
                     tags$span(style = "font-size: 12px;", "Not in Sample")
                   ),
                   tags$div(
                     style = "display: flex; align-items: center;",
                     tags$div(style = "width: 14px; height: 14px; background-color: #9e9e9e; border: 1px solid #fff; box-shadow: 0 0 2px #888; margin-right: 6px;"),
                     tags$span(style = "font-size: 12px;", "Unselected")
                   )
                 ),
                 
                 # Legend 3: Dynamic Tour Controls
                 tags$div(
                   style = "display: flex; flex-direction: column; margin-top: 2px;",
                   tags$span(style = "font-weight: bold; font-size: 13px; margin-bottom: 4px;", "Guided Tour"),
                   uiOutput("tour_controls") # Replaces the materialSwitch
                 )
               ),
               
               # RIGHT SIDE OF ROW: The Filters
               tags$div(
                 class = "perfect-align", # Applies the same margin-stripping fix we used on the right panel
                 checkboxGroupButtons(
                   inputId = "urban_level", 
                   label = tags$span(style = "font-weight: bold; font-size: 13px;", "Urbanisation Levels:"), 
                   # Translates the underlying 1-4 data into highly readable UI labels
                   choices = c("Low" = "1", "Mid-Low" = "2", "Mid-High" = "3", "High" = "4"),
                   selected = c("1", "2", "3", "4"),   
                   status = "custom-blue",
                   size = "sm"
                 )
               )
             )
           )
    ),
    # ---------------- RIGHT PANEL: SEARCH & SUMMARY ----------------
    column(width = 5, style = "padding-left: 5px; position: relative;",
           
           # Main Right Pane Bounding Box
           tags$div(
             id = "scroll_container", 
             style = "height: 735px; overflow-y: scroll; overflow-x: hidden; padding: 15px; border: 2px solid #e0e0e0; border-radius: 8px; background-color: #f8f9fa;",
             
             selectizeInput("region_search", label = NULL, 
                            choices = c("All Countries", valid_names), 
                            selected = "All Countries", 
                            options = list(
                              placeholder = 'SEARCH region or country...'
                            )
             ),
             
             # ---------------------------------------------------------
             # DYNAMIC INSIGHTS PANEL
             # ---------------------------------------------------------
             uiOutput("dynamic_insights"),
             
             # BOX ONE: SEARCH TRENDS
             tags$div(
               style = "border: 1px solid #dcdcdc; border-radius: 6px; padding: 15px; margin-bottom: 20px; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
               
               tags$div(
                 style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                 
                 h5("Search Trends", style = "margin: 0px; font-weight: bold;"),
                 
                 tags$div(
                   class = "perfect-align",
                   style = "display: flex; align-items: center; gap: 10px;",
                   
                   conditionalPanel(
                     condition = "input.time_res == 'Monthly'",
                     tags$div(style = "width: 80px;", 
                              selectInput("trend_year", label = NULL, choices = NULL)) 
                   ),
                   
                   radioGroupButtons(
                     inputId = "time_res",
                     label = NULL,
                     choices = c("Yearly", "Monthly"),
                     selected = "Yearly",
                     size = "sm",
                     status = "custom-blue" 
                   )
                 )
               ),
               
               plotlyOutput("trend_chart", height = "250px")
             ),
             
             # BOX TWO: HEALTH OUTCOMES & GENDER GAP
             tags$div(
               style = "border: 1px solid #dcdcdc; border-radius: 6px; padding: 15px; margin-bottom: 10px; background-color: #ffffff; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
               
               tags$div(
                 class = "perfect-align", 
                 style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                 
                 h5("Health Outcomes Impact", style = "margin: 0px; font-weight: bold;"),
                 
                 radioGroupButtons(
                   inputId = "health_target",
                   label = NULL,
                   choices = c("Obesity", "Inactivity"), 
                   selected = "Obesity",
                   size = "sm",
                   status = "custom-blue" 
                 )
               ),
               
               plotlyOutput("health_metric_chart", height = "280px"),
               
               hr(style = "border-top: 1px solid #eeeeee; margin-top: 25px; margin-bottom: 15px;"), 
               
               
               plotlyOutput("gender_gap_chart", height = "200px")
             )
           ), # End of the main scrollable div
           
           # ---------------------------------------------------------
           # THE SCROLL INDICATOR ARROW 
           # ---------------------------------------------------------
           tags$div(
             id = "scroll_arrow",
             style = "position: absolute; bottom: 35px; right: 35px; background-color: rgba(0, 0, 0, 0.4); color: white; width: 40px; height: 40px; border-radius: 50%; display: flex; justify-content: center; align-items: center; cursor: pointer; transition: opacity 0.4s ease; animation: bounce 2s infinite; z-index: 100;",
             icon("chevron-down", style = "margin-top: 3px;") 
           ),
           
           # ---------------------------------------------------------
           # THE SCROLL LISTENER SCRIPT
           # ---------------------------------------------------------
           tags$script(HTML("
             // Wait for the app to finish loading
             $(document).on('shiny:connected', function() {
               const scrollBox = document.getElementById('scroll_container');
               const arrow = document.getElementById('scroll_arrow');
               
               // 1. The original scroll listener (fade out at bottom)
               scrollBox.addEventListener('scroll', function() {
                 if (scrollBox.scrollHeight - scrollBox.scrollTop <= scrollBox.clientHeight + 15) {
                   arrow.style.opacity = '0'; 
                   arrow.style.pointerEvents = 'none'; // Disable clicks when invisible
                 } else {
                   arrow.style.opacity = '1'; 
                   arrow.style.pointerEvents = 'auto'; // Re-enable clicks when visible
                 }
               });
               
               // 2. CLICK LISTENER
               arrow.addEventListener('click', function() {
                 // Smoothly scroll the container to its absolute maximum height
                 scrollBox.scrollTo({
                   top: scrollBox.scrollHeight,
                   behavior: 'smooth'
                 });
               });
             });
           "))
    )
  ),
  # =========================================================
  # FULL WIDTH DATA SOURCES FOOTER
  # =========================================================
  fluidRow(
    column(width = 12,
           tags$div(
             style = "margin-top: 15px; margin-bottom: 15px; border: 1px solid #dcdcdc; border-radius: 6px; padding: 15px; background-color: #f8f9fa; font-size: 12px; color: #555;",
             
             p(strong("Original Data Source Information"), style = "margin-bottom: 10px; text-align: center; color: #333; font-size: 14px;"),
             
             tags$ul(style = "list-style-type: none; padding: 0; margin: 0; text-align: center; line-height: 1.8;",
                     
                     tags$li(tags$b("WHO:"), " Prevalence of Insufficient Physical Activity | ", 
                             tags$span(style="color:#08519c;", "Licensor: World Health Organization"), " | ",
                             tags$a(href="https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-insufficient-physical-activity-among-adults-aged-18-years-(age-standardized-estimate)-(-)", target="_blank", "Source Link")),
                     
                     tags$li(tags$b("WHO:"), " Prevalence of Obesity among Adults | ", 
                             tags$span(style="color:#08519c;", "Licensor: World Health Organization"), " | ",
                             tags$a(href="https://www.who.int/data/gho/data/indicators/indicator-details/GHO/prevalence-of-obesity-among-adults-bmi--30-(crude-estimate)-(-)", target="_blank", "Source Link")),
                     
                     tags$li(tags$b("Google Trends:"), " Global Search Volume Indices | ", 
                             tags$span(style="color:#08519c;", "Licensor: Google LLC"), " | ",
                             tags$a(href="https://trends.google.com/trends/", target="_blank", "Source Link")),
                     
                     tags$li(tags$b("World Bank:"), " Urban Population (% of total) | ", 
                             tags$span(style="color:#08519c;", "Licensor: The World Bank"), " | ",
                             tags$a(href="https://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS", target="_blank", "Source Link")),
                     
                     tags$li(tags$b("Natural Earth:"), " Admin 0 – Countries (Shapefile) | ", 
                             tags$span(style="color:#08519c;", "Licensor: Natural Earth (Public Domain)"), " | ",
                             tags$a(href="https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/", target="_blank", "Source Link"))
             )
           )
    )
  )
)

# ==========================================
# 4. THE SERVER
# ==========================================
server <- function(input, output, session) {
  
  selected_country <- reactiveVal(NULL)
  
  # ---------------------------------------------------------
  # HELP MODAL
  # ---------------------------------------------------------
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = tags$h3("Dashboard Navigation Guide", style = "margin: 0; font-weight: bold; color: #08519c;"),
      size = "l", # Large modal
      easyClose = TRUE, # Allows clicking outside the box to close it
      footer = modalButton("Got it!"),
      
      tags$div(style = "padding: 10px;",
               
               tags$h4(icon("globe"), " 1. The Interactive Map", style = "color: #e6550d; font-weight: bold; margin-top: 10px;"),
               tags$ul(style = "line-height: 1.6;",
                       tags$li("Hover over any colored country to see its specific Gym vs. Running preference score."),
                       tags$li(tags$b("Cross-Filtering:"), " Click on a country to isolate its data. The charts and summaries on the right will instantly update to reflect your selection. Click on the same country or the ocean to reset."),
                       tags$li(tags$b("Take a Tour:"), " Click the guided tour button in the legend for a cinematic journey through global fitness anomalies.")
               ),
               
               tags$hr(style = "border-top: 1px solid #eeeeee;"),
               
               tags$h4(icon("filter"), " 2. Global Filters", style = "color: #e6550d; font-weight: bold; margin-top: 20px;"),
               tags$ul(style = "line-height: 1.6;",
                       tags$li(tags$b("Urbanisation Levels:"), " Use the toggle buttons under the map to filter the world by development tiers (1 = Low Urbanisation, 4 = High Urbanisation)."),
                       tags$li(tags$b("Search Bar:"), " Use the dropdown on the right panel to quickly jump to and select any specific country.")
               ),
               
               tags$hr(style = "border-top: 1px solid #eeeeee;"),
               
               tags$h4(icon("chart-bar"), " 3. Analytics & Insights", style = "color: #e6550d; font-weight: bold; margin-top: 20px;"),
               tags$ul(style = "line-height: 1.6;",
                       tags$li(tags$b("Dynamic Insights:"), " Read the blue callout box for an instant, plain-text summary of the data currently being viewed."),
                       tags$li(tags$b("Search Trends:"), " Toggle between 'Yearly' and 'Monthly' to drill down into seasonal fitness habits."),
                       tags$li(tags$b("Health Outcomes:"), " Toggle between 'Obesity' and 'Inactivity'. This master switch controls both the scatterplot and the gender gap chart below it.")
               )
      )
    ))
  })
  
  last_shape_click <- reactiveVal(as.numeric(Sys.time()) - 10)
  
  # A tracker to remember the previous state of the checkboxes
  prev_urban <- reactiveVal(c("1", "2", "3", "4"))
  
  # ---------------------------------------------------------
  # REACTIVE DATA: The Global Master Filter
  # ---------------------------------------------------------
  active_global_data <- reactive({
    if (is.null(input$urban_level)) {
      return(fitness_data %>% filter(FALSE)) 
    }
    fitness_data %>% filter(urban_level %in% as.numeric(input$urban_level))
  })
  
  # ---------------------------------------------------------
  # THE BASE MAP (Minimalist 'Void' Theme)
  # ---------------------------------------------------------
  output$world_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      minZoom = 2,               
      maxBoundsViscosity = 1.0   
    )) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) 
  })
  
  # ---------------------------------------------------------
  # DYNAMIC MAP UPDATES (The Painter)
  # ---------------------------------------------------------
  observe({
    selected_levels <- if(is.null(input$urban_level)) c() else input$urban_level
    clicked <- selected_country()
    
    background_data <- map_data %>% filter(is.na(gym_vs_run_metric))
    
    if (is.null(clicked) || clicked == "All Countries" || clicked == "") {
      inactive_data <- map_data %>% filter(!is.na(gym_vs_run_metric) & !(urban_level %in% selected_levels))
      active_data   <- map_data %>% filter(!is.na(gym_vs_run_metric) & (urban_level %in% selected_levels))
    } else {
      inactive_data <- map_data %>% filter(!is.na(gym_vs_run_metric) & NAME != clicked)
      active_data   <- map_data %>% filter(!is.na(gym_vs_run_metric) & NAME == clicked)
    }
    
    max_limit <- max(abs(map_data$gym_vs_run_metric), na.rm = TRUE)
    
    pal <- colorNumeric(
      palette = c("#e6550d", "#ffffff", "#3182bd"), 
      domain = c(-max_limit, max_limit)
    )
    
    map_proxy <- leafletProxy("world_map") %>% clearShapes()
    
    if(nrow(background_data) > 0) {
      map_proxy <- map_proxy %>% addPolygons(
        data = background_data, fillColor = "#cccccc", weight = 1.2, color = "white", fillOpacity = 0.8, smoothFactor = 0.2
      )
    }
    
    if(nrow(inactive_data) > 0) {
      map_proxy <- map_proxy %>% addPolygons(
        data = inactive_data, layerId = ~NAME, fillColor = "#9e9e9e", weight = 1, color = "white", fillOpacity = 0.5, smoothFactor = 0.2,
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      )
    }
    
    if(nrow(active_data) > 0) {
      # 1. Logic to create readable preference text
      pref_text <- ifelse(active_data$gym_vs_run_metric > 0, 
                          paste("Gym preference:", abs(round(active_data$gym_vs_run_metric, 2))),
                          ifelse(active_data$gym_vs_run_metric < 0, 
                                 paste("Running preference:", abs(round(active_data$gym_vs_run_metric, 2))),
                                 "Balanced preference"))
      
      # 2. Creating the formatted tooltip with <b> tags (Plotly/Leaflet friendly)
      active_labels <- sprintf("<b>%s</b><br/>%s", active_data$NAME, pref_text) %>% lapply(htmltools::HTML)
      
      map_proxy <- map_proxy %>% addPolygons(
        data = active_data, layerId = ~NAME, 
        fillColor = ~pal(gym_vs_run_metric), 
        weight = 1, color = "black", fillOpacity = 0.9, smoothFactor = 0.2, label = active_labels,
        highlightOptions = highlightOptions(weight = 3, color = "#000000", bringToFront = TRUE)
      )
    }
  })
  
  # ---------------------------------------------------------
  # THE GUIDED TOUR (Cinematic Annotation Engine)
  # ---------------------------------------------------------
  tour_state <- reactiveVal("stopped") # Can be "stopped", "playing", or "paused"
  tour_step <- reactiveVal(1)
  
  # 1. Dynamically render the buttons based on the tour state
  output$tour_controls <- renderUI({
    state <- tour_state()
    
    if (state == "stopped") {
      actionButton("btn_start_tour", "Take a Tour", class = "btn-custom-blue btn-sm", style = "font-weight: bold;")
      
    } else if (state == "playing") {
      tags$div(style = "display: flex; gap: 5px;",
               actionButton("btn_pause_tour", icon("pause"), class = "btn-custom-blue btn-sm", title = "Pause Tour"),
               actionButton("btn_stop_tour", icon("stop"), class = "btn-custom-blue btn-sm", title = "End Tour")
      )
      
    } else if (state == "paused") {
      tags$div(style = "display: flex; gap: 5px;",
               actionButton("btn_resume_tour", icon("play"), class = "btn-custom-blue btn-sm", title = "Resume Tour"),
               actionButton("btn_stop_tour", icon("stop"), class = "btn-custom-blue btn-sm", title = "End Tour")
      )
    }
  })
  
  # 2. Button Click Listeners
  observeEvent(input$btn_start_tour, {
    selected_country(NULL)
    updateSelectizeInput(session, "region_search", selected = "All Countries")
    
    leafletProxy("world_map") %>% clearMarkers()
    tour_step(1)
    tour_state("playing")
  })
  
  observeEvent(input$btn_pause_tour, {
    tour_state("paused")
  })
  
  observeEvent(input$btn_resume_tour, {
    tour_state("playing")
  })
  
  observeEvent(input$btn_stop_tour, {
    tour_state("stopped")
    # Clean up the map and fly home
    leafletProxy("world_map") %>% clearMarkers() %>% flyTo(lng = 0, lat = 20, zoom = 2)
  })
  
  # 3. The Cinematic Tour Loop
  observe({
    # Only run the loop if the state is exactly "playing"
    req(tour_state() == "playing")
    
    # Wait 4.5 seconds before running this observer again
    invalidateLater(4500, session)
    
    isolate({
      step <- tour_step()
      
      # The 6 stops on our journey
      stops <- list(
        list(target_lat = 7.8, target_lng = 80.7, lat = 5.0, lng = 86.0, zoom = 4, 
             html = "<div style='width: 180px; white-space: normal; background: rgba(255,255,255,0.95); padding: 10px; border-radius: 4px; border-left: 4px solid #3182bd; box-shadow: 0px 2px 5px rgba(0,0,0,0.15); line-height: 1.4;'><strong style='color: #08519c; font-size: 12px;'>📍 Sri Lanka</strong><br/><span style='font-size: 11px; color: #444;'><b>Gym Stronghold</b><br/>Rapid urbanization drives indoor fitness routines.</span></div>"),
        list(target_lat = 56.2, target_lng = 9.5, lat = 57.5, lng = 3.0, zoom = 5, 
             html = "<div style='width: 180px; white-space: normal; background: rgba(255,255,255,0.95); padding: 10px; border-radius: 4px; border-left: 4px solid #e6550d; box-shadow: 0px 2px 5px rgba(0,0,0,0.15); line-height: 1.4;'><strong style='color: #a63603; font-size: 12px;'>📍 Denmark</strong><br/><span style='font-size: 11px; color: #444;'><b>Running Capital</b><br/>Deep cultural roots in outdoor cardio and recreation.</span></div>"),
        list(target_lat = 50.8, target_lng = 4.3, lat = 52.0, lng = -0.5, zoom = 5, 
             html = "<div style='width: 180px; white-space: normal; background: rgba(255,255,255,0.95); padding: 10px; border-radius: 4px; border-left: 4px solid #FFC107; box-shadow: 0px 2px 5px rgba(0,0,0,0.15); line-height: 1.4;'><strong style='color: #b38705; font-size: 12px;'>📍 Belgium</strong><br/><span style='font-size: 11px; color: #444;'><b>Cycling Hub</b><br/>Dominated by outdoor cycling interest and robust infrastructure.</span></div>"),
        list(target_lat = 51.1, target_lng = 10.4, lat = 55.0, lng = 15.0, zoom = 5, 
             html = "<div style='width: 180px; white-space: normal; background: rgba(255,255,255,0.95); padding: 10px; border-radius: 4px; border-left: 4px solid #333333; box-shadow: 0px 2px 5px rgba(0,0,0,0.15); line-height: 1.4;'><strong style='color: #000000; font-size: 12px;'>📍 Germany</strong><br/><span style='font-size: 11px; color: #444;'><b>Mindfulness Peak</b><br/>Highest global interest in Yoga, alongside a balanced fitness routine.</span></div>"),
        list(target_lat = 46.8, target_lng = 8.2, lat = 42.0, lng = 6.0, zoom = 5, 
             html = "<div style='width: 180px; white-space: normal; background: rgba(255,255,255,0.95); padding: 10px; border-radius: 4px; border-left: 4px solid #607d8b; box-shadow: 0px 2px 5px rgba(0,0,0,0.15); line-height: 1.4;'><strong style='color: #37474f; font-size: 12px;'>📍 Switzerland</strong><br/><span style='font-size: 11px; color: #444;'><b>Perfect Balance</b><br/>Almost exactly a 50/50 split in search volume between Gym and Running.</span></div>"),
        list(target_lat = 38.0, target_lng = -95.0, lat = 35.0, lng = -65.0, zoom = 4, 
             html = "<div style='width: 180px; white-space: normal; background: rgba(255,255,255,0.95); padding: 10px; border-radius: 4px; border-left: 4px solid #dd1c77; box-shadow: 0px 2px 5px rgba(0,0,0,0.15); line-height: 1.4;'><strong style='color: #980043; font-size: 12px;'>📍 United States</strong><br/><span style='font-size: 11px; color: #444;'><b>The Urban Paradox</b><br/>Massive Gym search interest, yet among the highest obesity rates globally.</span></div>")
      )
      
      # If there are still stops left on the tour...
      if (step <= length(stops)) {
        current <- stops[[step]]
        map_proxy <- leafletProxy("world_map")
        
        # Fly the camera
        map_proxy %>% flyTo(lng = current$target_lng, lat = current$target_lat, zoom = current$zoom)
        
        # Spawn the floating annotation card
        ann_data <- data.frame(lat = current$lat, lng = current$lng, label = current$html)
        map_proxy %>% addLabelOnlyMarkers(
          data = ann_data, lng = ~lng, lat = ~lat, label = ~lapply(label, htmltools::HTML),
          labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, 
                                      style = list("background" = "transparent", "border" = "none", "box-shadow" = "none"))
        )
        
        # Queue up the next stop
        tour_step(step + 1)
        
        # If the tour is finished...
      } else {
        # Auto-clean!
        tour_state("stopped")
        leafletProxy("world_map") %>% clearMarkers() %>% flyTo(lng = 0, lat = 20, zoom = 2)
      }
    })
  })
  
  # ---------------------------------------------------------
  # CAMERA CONTROLS (The Director)
  # ---------------------------------------------------------
  observeEvent(selected_country(), {
    clicked <- selected_country()
    map_proxy <- leafletProxy("world_map")
    
    if (is.null(clicked) || clicked == "All Countries" || clicked == "") {
      map_proxy %>% flyTo(lng = 0, lat = 20, zoom = 2)
    } else {
      active_data <- map_data %>% filter(!is.na(gym_vs_run_metric) & NAME == clicked)
      if (nrow(active_data) > 0) {
        bounds <- sf::st_bbox(active_data)
        map_proxy %>% flyToBounds(
          lng1 = as.numeric(bounds["xmin"]), lat1 = as.numeric(bounds["ymin"]),
          lng2 = as.numeric(bounds["xmax"]), lat2 = as.numeric(bounds["ymax"])
        )
      }
    }
  }, ignoreNULL = FALSE) 
  
  # ---------------------------------------------------------
  # CROSS-FILTERING: Smart Checkbox Auto-Ticking
  # ---------------------------------------------------------
  observeEvent(selected_country(), {
    clicked <- selected_country()
    
    if (!is.null(clicked) && clicked != "All Countries" && clicked != "") {
      target_urban <- map_data$urban_level[map_data$NAME == clicked][1]
      
      if (!is.na(target_urban)) {
        current_levels <- input$urban_level
        target_char <- as.character(target_urban)
        
        if (!(target_char %in% current_levels)) {
          # Changed from updateCheckboxGroupInput to updateCheckboxGroupButtons
          updateCheckboxGroupButtons(session, "urban_level", selected = c("1", "2", "3", "4"))
        }
      }
    }
  })
  
  # ---------------------------------------------------------
  # Unchecking a box clears the selected country
  # ---------------------------------------------------------
  observeEvent(input$urban_level, {
    curr_urban <- input$urban_level
    old_urban <- prev_urban()
    
    # Update the tracker for the next click
    prev_urban(curr_urban)
    
    # Safely handle NULLs (if the user unchecks the last box)
    len_curr <- if (is.null(curr_urban)) 0 else length(curr_urban)
    len_old <- if (is.null(old_urban)) 0 else length(old_urban)
    
    clicked <- selected_country()
    
    # If the user UNCHECKED a box (the length decreased) AND a country is actively selected
    if (len_curr < len_old && !is.null(clicked) && clicked != "All Countries" && clicked != "") {
      # Clear the active country so the map drops back to the global filtered view
      selected_country(NULL)
      updateSelectizeInput(session, "region_search", selected = "All Countries")
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # ---------------------------------------------------------
  # CROSS-FILTERING: Map Click updates the Search Bar
  # ---------------------------------------------------------
  observeEvent(input$world_map_shape_click, {
    last_shape_click(as.numeric(Sys.time()))
    
    click <- input$world_map_shape_click
    
    if (is.null(click$id)) {
      selected_country(NULL)
      updateSelectizeInput(session, "region_search", selected = "All Countries")
      return()
    }
    
    current_selected <- selected_country()
    
    if (!is.null(current_selected) && current_selected == click$id) {
      selected_country(NULL)
      updateSelectizeInput(session, "region_search", selected = "All Countries")
    } else {
      selected_country(click$id)
      updateSelectizeInput(session, "region_search", selected = click$id)
    }
  })
  
  observeEvent(input$world_map_click, {
    time_since_shape_click <- as.numeric(Sys.time()) - last_shape_click()
    
    if (time_since_shape_click > 0.3) {
      selected_country(NULL)
      updateSelectizeInput(session, "region_search", selected = "All Countries")
    }
  })
  
  observeEvent(input$region_search, {
    if (input$region_search == "All Countries" || input$region_search == "") {
      selected_country(NULL)
    } else {
      selected_country(input$region_search)
    }
  })
  
  # ---------------------------------------------------------
  # RIGHT PANEL CHARTS: Search Trends (Dynamic Drill-down)
  # ---------------------------------------------------------
  observe({
    years <- sort(unique(as.numeric(substr(fitness_data$Date, 1, 4))))
    updateSelectInput(session, "trend_year", choices = years, selected = 2025)
  })
  
  output$dynamic_insights <- renderUI({
    clicked <- selected_country()
    
    obesity_f <- if("Obesity_Female" %in% colnames(fitness_data)) "Obesity_Female" else "Obesity.Female"
    obesity_m <- if("Obesity_Male" %in% colnames(fitness_data)) "Obesity_Male" else "Obesity.Male"
    
    if (is.null(clicked) || clicked == "All Countries" || clicked == "") {
      global_df <- active_global_data()
      
      if(nrow(global_df) == 0) return(tags$div("No data available for these filters."))
      
      gap_f <- mean(global_df[[obesity_f]], na.rm = TRUE)
      gap_m <- mean(global_df[[obesity_m]], na.rm = TRUE)
      
      return(
        tags$div(
          style = "background-color: #e3f2fd; padding: 15px; border-radius: 6px; margin-bottom: 20px; border-left: 4px solid #3182bd; font-size: 13px;",
          tags$h5(style = "margin-top: 0px; color: #08519c; font-weight: bold;", "🌍 Global Insights"),
          tags$ul(style = "padding-left: 20px; margin-bottom: 0px;",
                  tags$li(tags$strong("Activity Preference: "), "Globally, interest in indoor 'Gym' workouts has steadily surged post-2021, overtaking outdoor 'Running' and 'Cycling'."),
                  tags$li(tags$strong("The Urbanization Paradox: "), "Surprisingly, higher gym search volume correlates positively with higher national obesity rates, highlighting the sedentary nature of highly urbanized/developed nations."),
                  tags$li(tags$strong("The Gender Gap: "), sprintf("Across the current selection, female obesity averages %.1f%% compared to %.1f%% for males, reflecting a persistent global health divide.", gap_f, gap_m))
          )
        )
      )
      
    } else {
      target_code <- map_data$ISO_A2_EH[map_data$NAME == clicked][1]
      country_df <- fitness_data %>% filter(Country == target_code)
      
      if(nrow(country_df) == 0) return(tags$div("No data available."))
      
      gym_vs_run <- mean(country_df$gym_vs_run_metric, na.rm = TRUE)
      pref_text <- if(gym_vs_run > 5) "a strong preference for Gym workouts" else if(gym_vs_run < -5) "a strong preference for Running" else "a balanced interest between Gym and Running"
      
      c_obesity_f <- mean(country_df[[obesity_f]], na.rm = TRUE)
      c_obesity_m <- mean(country_df[[obesity_m]], na.rm = TRUE)
      gap_diff <- abs(c_obesity_f - c_obesity_m)
      
      return(
        tags$div(
          style = "background-color: #e3f2fd; padding: 15px; border-radius: 6px; margin-bottom: 20px; border-left: 4px solid #3182bd; font-size: 13px;",
          tags$h5(style = "margin-top: 0px; color: #08519c; font-weight: bold;", paste("📍 Insights for", clicked)),
          tags$ul(style = "padding-left: 20px; margin-bottom: 0px;",
                  tags$li(tags$strong("Activity Preference: "), paste("This region shows", pref_text, "relative to the global baseline.")),
                  tags$li(tags$strong("Gender Health Gap: "), sprintf("There is a %.1f%% gap in obesity rates between women (%.1f%%) and men (%.1f%%) in this country.", gap_diff, c_obesity_f, c_obesity_m))
          )
        )
      )
    }
  })
  
  output$trend_chart <- renderPlotly({
    req(input$time_res) 
    clicked <- selected_country()
    
    if (is.null(clicked) || clicked == "All Countries" || clicked == "") {
      base_data <- active_global_data() 
      title_prefix <- "Global Average Trends"
    } else {
      target_code <- map_data$ISO_A2_EH[map_data$NAME == clicked][1]
      base_data <- fitness_data %>% filter(Country == target_code)
      title_prefix <- paste("Trends for", clicked)
    }
    
    if(nrow(base_data) == 0) return(plotly_empty() %>% layout(title = "No data selected"))
    
    base_data <- base_data %>% mutate(Year = as.numeric(substr(Date, 1, 4)))
    
    if (input$time_res == "Yearly") {
      plot_data <- base_data %>%
        group_by(Year) %>%
        summarise(across(c(Running, Cycling, Yoga, Gym, Aerobics), mean, na.rm = TRUE))
      
      x_var <- "Year"
      chart_title <- paste(title_prefix, "(Yearly)")
      
    } else {
      req(input$trend_year) 
      plot_data <- base_data %>%
        filter(Year == as.numeric(input$trend_year)) %>%
        group_by(Date) %>%
        summarise(across(c(Running, Cycling, Yoga, Gym, Aerobics), mean, na.rm = TRUE)) %>%
        mutate(Display_Month = factor(month.abb[as.numeric(substr(Date, 6, 7))], levels = month.abb))
      
      x_var <- "Display_Month"
      chart_title <- paste(title_prefix, "-", input$trend_year)
    }
    
    trend_long <- plot_data %>%
      pivot_longer(
        cols = c(Running, Cycling, Yoga, Gym, Aerobics), 
        names_to = "Topic",
        values_to = "Search_Volume"
      )
    
    custom_colors <- c(
      "Gym" = "#3182bd",       
      "Running" = "#e6550d",   
      "Cycling" = "#FFC107",   
      "Yoga" = "#333333",      
      "Aerobics" = "#dd1c77"   
    )
    
    p <- ggplot(trend_long, aes(x = !!sym(x_var), y = Search_Volume, color = Topic, group = Topic,
                                text = paste0("<b>", Topic, "</b><br>Interest: ", round(Search_Volume, 1)))) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) + 
      scale_color_manual(values = custom_colors) + 
      theme_minimal() +
      labs(title = chart_title, x = NULL, y = "Search Interest") +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 10)
      )
    
    if (input$time_res == "Monthly") {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.2), 
        margin = list(t = 30, b = 0, l = 50, r = 10),
        hoverlabel = list(bgcolor = "white", font = list(color = "#333333"), bordercolor = "#cccccc")
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  output$health_metric_chart <- renderPlotly({
    req(input$health_target)
    clicked <- selected_country()
    
    health_col <- if(input$health_target == "Obesity") {
      "Obesity_Both.sexes" 
    } else {
      "Inactivity_Both.sexes"
    }
    
    if (!(health_col %in% colnames(fitness_data))) {
      return(plotly_empty() %>% layout(title = paste("Error: Column", health_col, "not found.")))
    }
    
    scatterplot_data <- active_global_data() %>%
      group_by(Country) %>%
      summarise(
        Gym = mean(Gym, na.rm = TRUE),
        Yoga = mean(Yoga, na.rm = TRUE),
        Health_Val = mean(.data[[health_col]], na.rm = TRUE),
        .groups = 'drop'
      )
    
    if(nrow(scatterplot_data) == 0) return(plotly_empty() %>% layout(title = "No data selected"))
    
    country_names <- sf::st_drop_geometry(map_data) %>%
      select(ISO_A2_EH, NAME) %>%
      distinct()
    
    plot_df <- scatterplot_data %>%
      left_join(country_names, by = c("Country" = "ISO_A2_EH")) %>%
      filter(!is.na(NAME)) %>%
      pivot_longer(cols = c(Gym, Yoga), names_to = "Topic", values_to = "Search_Volume")
    
    p <- ggplot(plot_df, aes(x = Search_Volume, y = Health_Val)) +
      geom_point(
        aes(text = paste0("<b>", NAME, "</b><br>", Topic, " Interest: ", round(Search_Volume, 1), "<br>", input$health_target, ": ", round(Health_Val, 1), "%")), 
        alpha = 0.5, color = "#a6a6a6", size = 1.8
      ) +
      geom_smooth(method = "lm", se = FALSE, color = "#08519c", linewidth = 0.8) +
      facet_wrap(~Topic, scales = "free_x") +
      theme_minimal() +
      labs(
        title = paste("Global Distribution:", input$health_target, "vs Lifestyle Volume"),
        x = "Average Regional Search Interest",
        y = paste(input$health_target, "(%)")
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 10),
        strip.text = element_text(face = "bold", size = 11), 
        panel.spacing = unit(1.5, "lines")                 
      )
    
    if (!is.null(clicked) && clicked != "" && clicked != "All Countries") {
      selected_df <- plot_df %>% filter(NAME == clicked)
      if (nrow(selected_df) > 0) {
        p <- p + geom_point(
          data = selected_df,
          color = "#e41a1c", size = 3.5, shape = 18,        
          aes(text = paste0("<b>", NAME, "</b> (Selected)<br>", Topic, " Interest: ", round(Search_Volume, 1), "<br>", input$health_target, ": ", round(Health_Val, 1), "%"))
        )
      }
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(t = 40, b = 40, l = 50, r = 10), 
        dragmode = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "#333333"), bordercolor = "#cccccc")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$gender_gap_chart <- renderPlotly({
    req(input$health_target)
    clicked <- selected_country()
    
    if (is.null(clicked) || clicked == "All Countries" || clicked == "") {
      base_data <- active_global_data() 
      target_name <- "Global Average"
    } else {
      target_code <- map_data$ISO_A2_EH[map_data$NAME == clicked][1]
      base_data <- fitness_data %>% filter(Country == target_code)
      target_name <- clicked
    }
    
    if(nrow(base_data) == 0) return(plotly_empty() %>% layout(title = "No data selected"))
    
    col_male <- paste0(input$health_target, "_Male")
    col_female <- paste0(input$health_target, "_Female")
    
    if (!(col_male %in% colnames(base_data))) col_male <- paste0(input$health_target, ".Male")
    if (!(col_female %in% colnames(base_data))) col_female <- paste0(input$health_target, ".Female")
    
    if (!(col_male %in% colnames(base_data))) {
      return(plotly_empty() %>% layout(title = "Gender data not available"))
    }
    
    val_male <- mean(base_data[[col_male]], na.rm = TRUE)
    val_female <- mean(base_data[[col_female]], na.rm = TRUE)
    
    gap_df <- data.frame(
      Gender = c("Male", "Female"),
      Value = c(val_male, val_female)
    )
    gap_df$Gender <- factor(gap_df$Gender, levels = c("Male", "Female"))
    
    p <- ggplot(gap_df, aes(x = Gender, y = Value, color = Gender, 
                            text = paste0("<b>", Gender, "</b><br>", 
                                          input$health_target, ": ", round(Value, 1), "%"))) +
      geom_segment(aes(x = Gender, xend = Gender, y = 0, yend = Value), linewidth = 2) +
      geom_point(size = 8) +
      scale_color_manual(values = c("Male" = "#3182bd", "Female" = "#e6550d")) + 
      theme_minimal() +
      labs(title = paste(target_name, "-", input$health_target, "by Gender"), x = NULL, y = "Percentage (%)") +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 10),
        panel.grid.major.x = element_blank(), 
        axis.text.x = element_text(face = "bold", size = 11)
      ) +
      scale_y_continuous(limits = c(0, max(gap_df$Value, na.rm = TRUE) * 1.2))
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(t = 30, b = 20, l = 50, r = 10), 
        dragmode = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "#333333"), bordercolor = "#cccccc")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)