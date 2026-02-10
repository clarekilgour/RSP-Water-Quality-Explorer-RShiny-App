library(shiny)
library(dygraphs)
library(xts)
library(dplyr)
library(purrr)
library(lubridate)
library(datastreamr)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)
library(sf)
library(RSQLite)
library(dbplyr)
library(tidyr)
library(RColorBrewer)

setAPIKey(Sys.getenv("DATASTREAM_API_KEY"))

# Get maximum year in stored data -------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "raw_data.sqlite")

db_end_year <- tbl(con, "Max_Year") %>%
  pull(max_year)

# Load list of CoSMo sites ---------------------------------------------------
site_ids_raw <- tbl(con, "Locations") %>%
  collect() %>% 
  mutate(selected = FALSE) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "WGS84")

pal <- leaflet::colorFactor(
  palette = c("#00A9FF", "orange"),
  domain = c(TRUE, FALSE)
)

DBI::dbDisconnect(con)

# UI -------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js"),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.addEventListener('click', function(e) {
          if (e.target && e.target.id === 'save_btn') {
            const plotDiv = document.querySelector('#dygraph_container');
            if (!plotDiv) return;

            html2canvas(plotDiv).then(function(canvas) {
              var link = document.createElement('a');
              link.download = 'dygraph.png';
              link.href = canvas.toDataURL();
              link.click();
            });
          }
        });
      });
    "))
  ),
  uiOutput("ui_page")
)

# Server ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  showMainUI <- reactiveVal(FALSE)
  
  site_ids <- reactiveValues(
    data = site_ids_raw
  )
  
   param_meta <- list(
    cond = list(label = "Specific conductance",
                characteristic = "Specific conductance",
                ylab = "Surface Water Specific Conductance (µS/cm)"),
    cl   = list(label = "Chloride concentration",
                characteristic = "Specific conductance",
                ylab = "Surface Water Chloride Concentration (mg/L Cl)"),
    temp = list(label = "Temperature",
                characteristic = "Temperature, water",
                ylab = "Water Temperature (°C)"),
    wl   = list(label = "Water level",
                characteristic = "Water level (probe)",
                ylab = "Water Level")
  )
  
  # Dynamic UI switching ------------------------------------------------------
  output$ui_page <- renderUI({
    if (!showMainUI()) {
      # ----- LANDING PAGE -----
      tagList(
        tags$div(
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center;
                   min-height: 90vh; text-align: center; padding: 20px;",
          
          tags$div(
            style = "max-width: 800px;",
            
            h1("Welcome to the Road Salt Project Water Quality Data Explorer!"),
            br(), br(),
            tags$div(
              style = "font-size: 18px;",
              p("Use this tool to explore high-frequency water quality data from across Vancouver's Lower Mainland.
                 All data available in this tool are sourced from the Department of Fisheries and Oceans Canada's CoSMo Database,
                 hosted on DataStream. For more information about this database, visit ",
                a("(DOI: https://doi.org/10.25976/0gvo-9d12)",
                  href = "https://doi.org/10.25976/0gvo-9d12",
                  target = "_blank"),
                "."),
              p("The Road Salt Project is a collaborative project between the University
                 of British Columbia, Simon Fraser University, the British Columbia Institute
                 of Technology, the Department of Fisheries and Oceans Canada, and dedicated
                 steward volunteers. This project is supported by an NSERC Alliance Grant."),
              p("We would like to thank those stream stewards who dedicated countless
                 volunteer hours to collect the data used in this application."),
              p("This app was created in collaboration with DataStream, and the code is available on ",
                a("GitHub",
                  href = "https://github.com/clarekilgour/RSP-Water-Quality-Explorer-RShiny-App",
                  target = "_blank"),
                "."),
              p("If you have feedback about this app, please reach out to Nikki.Kroetsch@dfo-mpo.gc.ca")
            ),
            
            actionButton("startButton", "Start", class = "btn btn-primary", style = "margin-top: 20px;")
          ),
          
          tags$div(
            tags$img(src = "logo.png", height = "150px"),
            style = "margin-top: 50px;"
          )
        )
      )
      
    } else {
      # ----- PLOTTING PAGE UI -----
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectizeInput(
            "sites", "Monitoring Locations:",
            choices = setNames(site_ids$data$ID, paste(site_ids$data$ID, "-", site_ids$data$Name)),
            selected = site_ids$data$ID[site_ids$data$selected],
            multiple = TRUE
          ),
          
          numericInput("start_year", "Start Year:", value = 2021,
                       min = 2021, max = year(Sys.Date())),
          numericInput("end_year", "End Year:", value = year(Sys.Date()) - 1,
                       min = 2021, max = year(Sys.Date())),
          helpText(paste0("Including data from ", year(Sys.Date()), " may slow the data retrieval")),
          
          selectInput(
            "param",
            "Parameter to plot:",
            choices = c(
              "Specific conductance (μS/cm)"      = "cond",
              "Chloride concentration (mg/L)"    = "cl",
              "Temperature (°C)"               = "temp",
              "Water level (m)"               = "wl"
            ),
            selected = "cond"
          ),
          
          actionButton("goButton", "Go", class = "btn-primary"),
          br(), br(),
          p("See the map of monitoring locations below. Click on the monitoring location to see the full site name."),
          br(),
          leafletOutput("map_out", height = "35vh", width = "100%")
        ),
        
        mainPanel(
          width = 7,
          conditionalPanel(
            condition = "input.goButton == 0",
            wellPanel(
              h3("Instructions"),
              p("1. Select one or more monitoring sites. Since the data sets are high-frequency, it may take a few minutes to load the data for each site."),
              p("2. Choose a start and end year."),
              p("3. Choose a parameter to plot (conductivity, chloride, temperature, or water level)."),
              p("4. Click the 'Go' button to graph your selected data!"),
              h4("Some Tips:"),
              p("1. You can zoom in to different parts of the graph by clicking and dragging your mouse over them."),
              p("2. Reset the zoom by double clicking on the graph!")
            )
          ),
          
          conditionalPanel(
            condition = "input.goButton > 0",
            br(), br(),
            div(
              id = "dygraph_container",
              withSpinner(dygraphOutput("simplePlot", height = "625px"))
            ),
            br(),
            div(
              style = "text-align: center; margin-top: 10px;",
              actionButton("save_btn", "Download Plot as PNG", class = "btn btn-success")
            )
          )
        )
      )
    }
  })
  
  # Switch pages --------------------------------------------------------------
  observeEvent(input$startButton, {
    showMainUI(TRUE)
  })
  
  # Pull data (SQLite for older years + API for newer years) ------------------
  param_xts <- eventReactive(input$goButton, {
    req(input$sites, input$start_year, input$end_year, input$param)

    meta <- param_meta[[input$param]]
    req(!is.null(meta))
    
    years <- input$start_year:input$end_year
    pulled_data <- list()
    
    # --- Pull from SQLite for years <= db_end_year (if present) ---
    if (any(years <= db_end_year)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), "raw_data.sqlite")
      onStop(function() suppressWarnings(DBI::dbDisconnect(con)))
      
      user_sel_PK <- tbl(con, "loc_char_year_tbl") %>%
        filter(MonitoringLocationID %in% input$sites) %>%
        filter(ActivityStartYear %in% years) %>%
        filter(CharacteristicName == meta$characteristic) %>%
        collect()
      
      
      db_out <- tbl(con, "obs_tbl") %>%
        filter(loc_char_year_PK %in% !!user_sel_PK$loc_char_year_PK) %>% 
        left_join(
          tbl(con, "loc_char_year_tbl"), by = "loc_char_year_PK"
        ) %>% 
        left_join(
          tbl(con, "date_tbl"), by = "date_PK"
        ) %>% 
        left_join(
          tbl(con, "time_tbl"), by = "time_PK"
        ) %>% 
        select(-loc_char_year_PK,-date_PK,-time_PK) %>% 
        collect()
      
      DBI::dbDisconnect(con)
      
      db_out <- split(db_out, paste(db_out$MonitoringLocationID, db_out$ActivityStartYear))
      pulled_data <- c(pulled_data, db_out)
      
      years <- years[years > db_end_year]
    }
    
    # --- Pull remaining years from DataStream API ---
    total_steps <- length(input$sites) * length(years)
    step <- 0
    
    withProgress(message = "Pulling data from CoSMo database...", value = 0, {
      for (site_code in input$sites) {
        site_guid <- as.data.frame(site_ids$data) %>%
          filter(ID == site_code) %>%
          pull(DS_Id)
        
        if (is.na(site_guid) || length(site_guid) == 0) {
          showNotification(paste("Missing GUID for site:", site_code), type = "error")
          next
        }
        
        for (yr in years) {
          step <- step + 1
          if (total_steps > 0) {
            incProgress(1 / total_steps, detail = paste("Site:", site_code, "Year:", yr))
          }
          
          filter_string <- paste0(
            "DOI eq '10.25976/0gvo-9d12' and ",
            "CharacteristicName eq '", meta$characteristic, "' and ",
            "LocationId eq '", site_guid, "' and ",
            "ActivityStartYear eq '", yr, "'"
          )
          
          obs_result <- tryCatch({
            records(list(
              `$select` = "MonitoringLocationID,ActivityStartDate,ActivityStartTime,CharacteristicName,ResultValue,ResultUnit",
              `$filter` = filter_string,
              `$top` = 2000
            ))
          }, error = function(e) {
            showNotification(paste("Error loading", site_code, "(", yr, "):", e$message), type = "error")
            return(NULL)
          })
          
          if (!is.null(obs_result) && nrow(obs_result) > 0) {
            pulled_data[[length(pulled_data) + 1]] <- obs_result
          }
        }
      }
    })
    
    # Normalize and combine ---------------------------------------------------
    pulled_data <- lapply(pulled_data, function(df) {
      need_cols <- c("ActivityStartDate", "ActivityStartTime", "ResultValue", "MonitoringLocationID")
      if (!all(need_cols %in% names(df))) return(NULL)
      
      df %>%
        mutate(
          ActivityStartDate = ymd(ActivityStartDate),
          Timestamp = ymd_hms(paste(ActivityStartDate, ActivityStartTime)),
          Value = ResultValue
        ) %>%
        select(MonitoringLocationID, Timestamp, Value)
    })
    
    pulled_data <- pulled_data[!sapply(pulled_data, is.null)]
    final_df <- bind_rows(pulled_data) %>%
      group_by(MonitoringLocationID, Timestamp) %>%
      summarise(Value = median(Value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(final_df) == 0) return(NULL)
    
    df_wide <- final_df %>%
      pivot_wider(names_from = MonitoringLocationID, values_from = Value) %>%
      arrange(Timestamp)
    
    xts::xts(df_wide %>% select(-Timestamp), order.by = df_wide$Timestamp)
  })
  
  # Convert to chloride only when selected -----------------------------------
  param_transformed <- reactive({
    raw_xts <- param_xts()
    req(!is.null(raw_xts), input$param)
    
    if (input$param == "cl") {
      raw_xts * 0.3117
    } else {
      raw_xts
    }
  })
  
  # Plot ---------------------------------------------------------------------
  output$simplePlot <- renderDygraph({
    xts_obj <- param_transformed()
    req(!is.null(xts_obj), input$param)
    
    ylab <- dplyr::case_when(
      input$param == "cond" ~ "Surface Water Specific Conductance (µS/cm)",
      input$param == "cl"   ~ "Surface Water Chloride Concentration (mg/L Cl)",
      input$param == "temp" ~ "Surface Water Temperature (°C)",
      input$param == "wl"   ~ "Water Level (m)",
      TRUE ~ ""
    )
    
    graph <- dygraph(xts_obj) %>%
      dyOptions(drawPoints = FALSE, colors = RColorBrewer::brewer.pal(8, "Set2")) %>%
      dyAxis("y", label = ylab) %>%
      dyRangeSelector()
    
    # Add chloride guideline lines only when plotting chloride
    if (input$param == "cl") {
      graph <- graph %>%
        dyLimit(limit = 150, label = "Long-term Chronic (150 mg/L Cl)", color = "orange") %>%
        dyLimit(limit = 600, label = "Short-term Acute (600 mg/L Cl)", color = "red")
    }
    
    graph
  })
  
  # Basemap ------------------------------------------------------------------
  output$map_out <- renderLeaflet({
    cent <- colMeans(sf::st_coordinates(site_ids$data$geometry))
    
    leaflet() %>%
      setView(lng = cent[[1]], lat = cent[[2]], zoom = 10) %>%
      addProviderTiles(providers$Esri.WorldImagery, group ="ESRI - Imagery") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group ="OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB") %>%
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "ESRI - Imagery"),
        position = "topleft",
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      addCircles(
        data = site_ids$data,
        label = ~ID,
        popup = ~Name,
        radius = 7,
        stroke = TRUE,
        opacity = 0.85,
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
        color = ~pal(selected)
      )
  })
  
  # Update selected sites in reactive sf object ------------------------------
  observeEvent(input$sites, ignoreNULL = FALSE, {
    site_ids$data$selected <- FALSE
    site_ids$data$selected[site_ids$data$ID %in% input$sites] <- TRUE
  })
}

# Launch ---------------------------------------------------------------------
shinyApp(ui, server)

