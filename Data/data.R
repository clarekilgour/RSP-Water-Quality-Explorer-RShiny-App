if (T) { 
  # run annually or as needed to update historical data
  
  library(datastreamr)
  library(dplyr)
  library(RSQLite)
  library(lubridate)
  library(dbplyr)
  
  setAPIKey(Sys.getenv("DATASTREAM_API_KEY"))
  
  current_year <- year(Sys.Date())  
  years <- 2021:(year(Sys.Date()) - 1)
  
  rsp_ids <- locations(list(`$filter` = "DOI eq '10.25976/0gvo-9d12'")) %>%
    select(ID, Id, Latitude, Longitude,Name) %>% 
    subset(ID %in% 
             c("ALOU01", "ALOU04", "ANCI02", "BROT06", "BRUN01", "COUG02", "COUG03", "COUG05",
               "CYPR01", "EAGC01", "GUIC01", "HOYC03", "HYDE01", "LUCK01", "MOSS01",
               "MOSS03", "PEAC01", "ANCI02", "QUIB01", "QUIB02", "RODG02", "SERP01", "SERP02","SILV01",
               "SEYM01", "STIL04", "STIL05","STON04", "STON08", "WAGG03", "WAGG01", "YORK05")) %>%
    subset(Id != "896348") %>% #Removing duplicate COUG05 
    arrange(ID) %>% 
    mutate(selected = F) %>% 
    sf::st_as_sf(coords=c("Longitude","Latitude"),crs="WGS84")
  
  all_obs <- list()  
  
  for (i in seq_len(nrow(rsp_ids))) {
    loc_id <- rsp_ids$Id[i]     
    loc_name <- rsp_ids$ID[i]   
    
    for (yr in years) {
    
    filter_string <- paste0(
      "DOI eq '10.25976/0gvo-9d12' and ",
      "CharacteristicName eq 'Specific conductance' and ",
      "LocationId eq '", loc_id, "' and ",
      "ActivityStartYear eq '", yr,"'"
    )
    
    obs_result <- records(
      list(`$select` = "MonitoringLocationID,ActivityStartDate,ActivityStartTime,CharacteristicName,ResultValue,ResultUnit", 
           `$filter` = filter_string, 
           `$top` = 5000))
    
    if (!is.null(obs_result) && nrow(obs_result) > 0) {
      obs_result$MonitoringLocationID <- loc_name
      all_obs[[length(all_obs) + 1]] <- obs_result
      message("Data pulled for ", loc_name, "(", yr, "): ", nrow(obs_result), " rows")
    } else {
      message("No data for ", loc_name, " (", yr, ")")
    }
    }
  }
  
  obs <- bind_rows(all_obs)
  
  obs <- obs %>% 
    mutate(ActivityStartYear = as.numeric(substr(ActivityStartDate,1,4)))
  
  # Tidying Data before saving as SQL DB
  obsDups <- obs %>%
    group_by(MonitoringLocationID, ActivityStartDate,
             ActivityStartTime, CharacteristicName) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1L) #29 duplicate entries
  
  obsDups$MonitoringLocationID <- as.factor(obsDups$MonitoringLocationID)
  obsDups$CharacteristicName <- as.factor(obsDups$CharacteristicName) #There are 160 instances of duplicates
  
  obs2 <- obs %>% distinct(MonitoringLocationID, ActivityStartDate,
                           ActivityStartTime, CharacteristicName, .keep_all = TRUE)
  #2,783,155 - 29 = 2,783,126 (correct)
  
  con <- DBI::dbConnect(RSQLite::SQLite(), "raw_data.sqlite")
  
  t1<-dplyr::copy_to(df=obs2,
                     con,
                     "Conductivity_data",
                     overwrite =T,
                     temporary =F,
                     analyze=T,
                     in_transaction=T)
  
  t2<-dplyr::copy_to(df=tibble(max_year = max(obs$ActivityStartYear)),
                     con,
                     "Max_Year",
                     overwrite =T,
                     temporary =F,
                     analyze=T,
                     in_transaction=T)
  
  # This will speed up queries and optimize the database
  s1<-RSQLite::dbExecute(con, "CREATE INDEX Conductivity_data_idx ON Conductivity_data (MonitoringLocationID,ActivityStartYear);")
  s6<-RSQLite::dbExecute(con, "pragma vacuum;")
  s7<-RSQLite::dbExecute(con, "pragma optimize;")
  
  DBI::dbDisconnect(con)
  
}
