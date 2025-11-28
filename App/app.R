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


setAPIKey(Sys.getenv("DATASTREAM_API_KEY"))

# Load list of CoSMo sites
site_ids_raw <- locations(list(`$filter` = "DOI eq '10.25976/0gvo-9d12'")) %>%
  select(ID, Id, Latitude, Longitude,Name) %>% 
  subset(ID %in% 
           c("ALOU01", "ALOU04", "ANCI02", "BROT06", "BRUN01", "COUG02", "COUG03", "COUG05",
             "CYPR01", "EAGC01", "GUIC01", "HOYC03", "HYDE01", "LUCK01", "MOSS01",
             "MOSS03", "PEAC01", "ANCI02", "QUIB01", "QUIB02", "RODG02", "SERP01", "SERP02","SILV01",
             "SEYM01", "STIL04", "STIL05", "STON04", "STON08", "WAGG03", "WAGG01", "YORK05")) %>%
  subset(Id != "896348") %>%
  arrange(ID) %>% 
  mutate(selected = F) %>% 
  sf::st_as_sf(coords=c("Longitude","Latitude"),crs="WGS84")

pal <- leaflet::colorFactor(
  palette = c("#00A9FF", "orange"),
  domain = c(T,F)
)

# Get maximum year in stored data -----------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "raw_data.sqlite")

db_end_year <- tbl(con,"Max_Year") %>% 
  pull(max_year) 

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
  
  # This handles the dynamic UI switching
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
            # Wrap all <p> elements in a div that increases font size
            tags$div(style = "font-size: 18px;",  # You can bump to 20px if needed
                     p("Use this tool to explore high-frequency specific conductance data and calculated chloride 
          concentrations from across Vancouver's Lower Mainland. All data 
          available in this tool are sourced from the Department of Fisheries and 
          Oceans Canada's CoSMo Database, hosted on DataStream. For more information about this database, visit ",
                       a("(DOI: https://doi.org/10.25976/0gvo-9d12)",
                         href = "https://doi.org/10.25976/0gvo-9d12",
                         target = "_blank"), "."),
                     
                     p("The Road Salt Project is a collaborative project between the University 
          of British Columbia, Simon Fraser University, the British Columbia Institute 
          of Technology, the Department of Fisheries and Oceans Canada, and dedicated 
          steward volunteers. This project is supported by an NSERC Alliance Grant."),
                     
                     p("We would like to thank those stream stewards who dedicated countless 
          volunteer hours to collect the data used in this application."),
                     
                     p("This app was created in collaboration with DataStream, and the code is available on ",
                       a("GitHub",
                         href = "https://github.com/clarekilgour/RSP-Water-Quality-Explorer-RShiny-App",
                         target = "_blank"), "."),
                     
                     p("If you have feedback about this app, please reach out to Nikki.Kroetsch@dfo-mpo.gc.ca")
            ),
            
            actionButton("startButton", "Start", class = "btn btn-primary", style = "margin-top: 20px;")
          ),
          
          # Logo centered at bottom
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
          selectizeInput("sites", "Monitoring Locations:",
                         choices = setNames(site_ids$data$ID, paste(site_ids$data$ID, "-", site_ids$data$Name)),
                         selected = site_ids$data$ID[site_ids$data$selected],
                         multiple = TRUE),
          numericInput("start_year", "Start Year:", value = 2021,
                       min = 2021, max = year(Sys.Date())),
          numericInput("end_year", "End Year:", value = year(Sys.Date())-1,
                       min = 2021, max = year(Sys.Date())),
          helpText(paste0("Including data from ",year(Sys.Date())," may slow the data retrieval")),
          radioButtons("unit", "Display Units:",
                       choices = c("µS/cm" = "uscm", "mg/L Cl-" = "mgL"),
                       selected = "uscm", inline = TRUE),
          actionButton("goButton", "Go", class = "btn-primary"),
          br(), br(),
          p("See the map of monitoring locations below. Click on the monitoring location to see the full site name."),
          br(),
          leaflet::leafletOutput("map_out",height="35vh",width="100%")
        ),
        mainPanel(
          width = 7,
          conditionalPanel(
            condition = "input.goButton == 0",
            wellPanel(
              h3("Instructions"),
              p("1. Select one or more monitoring sites. Since the data sets are high-frequency, it may take a few minutes to load the data for each site."),
              p("2. Choose a start and end year."),
              p("3. Choose display units (specific conductance or estimated chloride). If displaying the data as mg/L Cl-, BC's provincial guidelines for chloride will automatically be added on the graph. 
                Once the graph is loaded, you can toggle between the units without reloading the data."),
              p("4. Click the 'Go' button to graph your selected data!"),
              p(""),
              h4("Some Tips:"),
              p("1. You can zoom in to different parts of the graph by clicking and dragging your mouse over them."),
              p("2. Reset the zoom by double clicking on the graph!")
            )
          ),
          
          # Plot AFTER Go is clicked
          conditionalPanel(
            condition = "input.goButton > 0",
            br(), br(),  
            div(id = "dygraph_container",
                withSpinner(
                  dygraphOutput("simplePlot", height = "625px")
                )
            ),
            br(),
            div(style = "text-align: center; margin-top: 10px;",
                actionButton("save_btn", "Download Plot as PNG", class = "btn btn-success"))
          )
        )
      )
    }
  })
  
  # When "Start" is clicked, switch to the plotting UI
  observeEvent(input$startButton, {
    showMainUI(TRUE)
  })
  
  conductivity_xts <- eventReactive(input$goButton, {
    req(input$sites, input$start_year, input$end_year)

    years <- input$start_year:input$end_year
    pulled_data <- list()
    
    if (any(years <= db_end_year)) {
      con <- DBI::dbConnect(RSQLite::SQLite(), "raw_data.sqlite")
      
      onStop({
        function() suppressWarnings(DBI::dbDisconnect(con)) #closes the connecting in case app disconnects
      })
      
      db_out  <- tbl(con,"Conductivity_data") %>% 
        filter(MonitoringLocationID %in% input$sites) %>% 
        filter(ActivityStartYear %in% years) %>% 
        collect()
      
      DBI::dbDisconnect(con)
      
      
      db_out <- split(
        db_out,
        paste(db_out$MonitoringLocationID, db_out$ActivityStartYear)
      )
      
      pulled_data <- c(
        pulled_data,
        db_out
      )
      
      years <- years[years>db_end_year]
    }
    
    total_steps <- length(input$sites) * length(years)
    step <- 0
    
    withProgress(message = "Pulling data from CoSMo database...", value = 0, {
      for (site_code in input$sites) {
        site_guid <- as.data.frame(site_ids$data) %>% filter(ID == site_code) %>% pull(Id)
        
        if (is.na(site_guid) || length(site_guid) == 0) {
          showNotification(paste("Missing GUID for site:", site_code), type = "error")
          next
        }
        
        for (yr in years) {
          step <- step + 1
          incProgress(1 / total_steps, detail = paste("Site:", site_code, "Year:", yr))
          
          filter_string <- paste0(
            "DOI eq '10.25976/0gvo-9d12' and ",
            "CharacteristicName eq 'Specific conductance' and ",
            "LocationId eq '", site_guid, "' and ",
            "ActivityStartYear eq '", yr, "'"
          )
          
          message("Requesting: ", filter_string)
          
          obs_result <- tryCatch({
            records(list(`$select` = "MonitoringLocationID,ActivityStartDate,ActivityStartTime,CharacteristicName,ResultValue,ResultUnit",
                              `$filter` = filter_string, `$top` = 2000))
          }, error = function(e) {
            message("ERROR for ", site_code, " (", yr, "): ", e$message)
            showNotification(paste("Error loading", site_code, "(", yr, "):", e$message), type = "error")
            return(NULL)
          })
          
          if (!is.null(obs_result) && nrow(obs_result) > 0) {
            pulled_data[[length(pulled_data) + 1]] <- obs_result
          }
        }
      }
    })
    
    pulled_data <- lapply(pulled_data, function(df) {
      if (!all(c("ActivityStartDate", "ActivityStartTime", "ResultValue", "MonitoringLocationID") %in% names(df))) {
        return(NULL)
      }
      
      df %>%
        mutate(
          ActivityStartDate = ymd(ActivityStartDate),
          Timestamp = ymd_hms(paste(ActivityStartDate, ActivityStartTime)),
          Specific.conductance = ResultValue 
        ) %>%
        select(MonitoringLocationID, Timestamp, Specific.conductance)
    })
    
    # Remove NULLs or malformed entries
    pulled_data <- pulled_data[!sapply(pulled_data, is.null)]
    
    final_df <- bind_rows(pulled_data) %>% 
      group_by( MonitoringLocationID, Timestamp) %>% 
      summarise(Specific.conductance = median(Specific.conductance,na.rm=T),
                .groups = "drop") # I noticed a few duplicated time stamps in the data, I didn't investigate 
    
    if (nrow(final_df) == 0) return(NULL)
    
    df_wide <- final_df %>%
      tidyr::pivot_wider(names_from = MonitoringLocationID, values_from = Specific.conductance) %>%
      arrange(Timestamp)
    
    xts::xts(df_wide %>% select(-Timestamp), 
             order.by = df_wide$Timestamp)
  })
  
  conductivity_transformed <- reactive({
    raw_xts <- conductivity_xts()
    req(!is.null(raw_xts))
    
    if (input$unit == "mgL") {
      raw_xts * 0.3117
    } else {
      raw_xts
    }
  })
  
  output$simplePlot <- renderDygraph({
    xts_obj <- conductivity_transformed()
    req(!is.null(xts_obj))
    
    graph <- dygraph(xts_obj) %>%
      dyOptions(drawPoints = FALSE, colors = RColorBrewer::brewer.pal(8, "Set2")) %>%
      dyAxis("y", label = if (input$unit == "uscm") "Surface Water Specific Conductance (µS/cm)" else "Surface Water Chloride Concentration (mg/L Cl)") %>%
      dyRangeSelector()
    
    if (input$unit == "mgL") {
      graph <- graph %>%
        dyLimit(limit = 150, label = "Long-term Chronic (150 mg/L Cl)", color = "orange") %>%
        dyLimit(limit = 600, label = "Short-term Acute (600 mg/L Cl)", color = "red")
    }
    graph
  })
  
  # Basemap -----------------------------------------------------------------
  output$map_out <- renderLeaflet({
    cent <- colMeans(sf::st_coordinates(site_ids$data$geometry))
    
    leaflet() %>% 
      setView(lng = cent[[1]], lat = cent[[2]], zoom = 10) %>% 
      addProviderTiles(providers$Esri.WorldImagery, group ="ESRI - Imagery") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group ="OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB") %>% 
      leaflet::addLayersControl(
        baseGroups = c("CartoDB",
                       "OpenStreetMap",
                       "ESRI - Imagery"),
        position = "topleft",
        options = leaflet::layersControlOptions(collapsed = F)
      ) %>% 
      leaflet::addCircles(
        data = site_ids$data,
        label = ~ID,
        popup = ~Name,
        radius = 7,
        stroke = T,
        opacity = 0.85,
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
        color = ~pal(selected)
      )
  })
  
  # Update map based on list ------------------------------------------------
  
  observeEvent(input$sites,ignoreNULL = F, {
    site_ids$data$selected <- F
    site_ids$data$selected[site_ids$data$ID %in% input$sites] <- T
    
  })
  
  # Save Plot --------------------------------------------------------------
  observeEvent(input$download_plot, {
    session$sendCustomMessage("save_plot", list())
  })
}


# Launch ---------------------------------------------------------------------
shinyApp(ui, server)
