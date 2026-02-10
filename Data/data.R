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
    arrange(ID) 
  
  all_obs <- list()  
  
  for (i in seq_len(nrow(rsp_ids))) {
    loc_id <- rsp_ids$Id[i]     
    loc_name <- rsp_ids$ID[i]   
    
    for (yr in years) {
      
      filter_string <- paste0(
        "DOI eq '10.25976/0gvo-9d12' and ",
        "CharacteristicName in ('Temperature, water', 'Specific conductance','Water level (probe)') and ",
        "LocationId eq '", loc_id, "' and ",
        "ActivityStartYear eq '", yr,"'"
      )
      
      obs_result <- observations(
        list(`$select` = "ActivityStartDate,ActivityStartTime,CharacteristicName,ResultValue,ResultUnit", 
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
  
  # Set up primary keys
  obs3 <- obs2 %>% 
    select(-Id) %>% 
    mutate(
      loc_char_year_PK = as.integer(as.factor(paste(MonitoringLocationID,CharacteristicName,ResultUnit,ActivityStartYear))),
      date_PK = as.integer(as.factor(ActivityStartDate)),
      time_PK = as.integer(as.factor(ActivityStartTime)),
    )
  
  # Denormalize Data
  loc_char_year_tbl <- obs3 %>% 
    select(
      loc_char_year_PK,
      MonitoringLocationID,
      CharacteristicName,
      ResultUnit,
      ActivityStartYear
    ) %>% 
    distinct()
  
  date_tbl <- obs3 %>% 
    select(
      date_PK,
      ActivityStartDate
    ) %>% 
    distinct()
  
  time_tbl <- obs3 %>% 
    select(
      time_PK,
      ActivityStartTime
    ) %>% 
    distinct()
  
  obs_tbl <- obs3 %>% 
    select(
      loc_char_year_PK,
      date_PK,
      time_PK,
      ResultValue
    )
  
  
  con <- DBI::dbConnect(RSQLite::SQLite(), "raw_data.sqlite")
  
  for (i in c("loc_char_year_tbl","date_tbl","time_tbl","obs_tbl")){
    t1<-dplyr::copy_to(df=get(i),
                       con,
                       i,
                       overwrite =T,
                       temporary =F,
                       analyze=T,
                       in_transaction=T)
    
  }
  
  t2<-dplyr::copy_to(df=tibble(max_year = max(obs3$ActivityStartYear)),
                     con,
                     "Max_Year",
                     overwrite =T,
                     temporary =F,
                     analyze=T,
                     in_transaction=T)
  
  t2<-dplyr::copy_to(df=rsp_ids %>% rename(DS_Id=Id),
                     con,
                     "Locations",
                     overwrite =T,
                     temporary =F,
                     analyze=T,
                     in_transaction=T)
  
  # This will speed up queries and optimize the database
  s1<-RSQLite::dbExecute(con, "CREATE INDEX obs_idx ON obs_tbl (loc_char_year_PK);")
  s1<-RSQLite::dbExecute(con, "CREATE INDEX loc_char_year_idex ON loc_char_year_tbl (MonitoringLocationID,ActivityStartYear,CharacteristicName);")
  s1<-RSQLite::dbExecute(con, "pragma vacuum;")
  s1<-RSQLite::dbExecute(con, "pragma optimize;")
  
  DBI::dbDisconnect(con)
  
  file.copy("raw_data.sqlite",file.path("App","raw_data.sqlite"))

}
