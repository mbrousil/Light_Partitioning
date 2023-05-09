
find_simultaneous_no_sdd <- function(chla_path, doc_path, tss_path,
                                     wqp_metadata, middle_only = FALSE){
  
  # Load data ---------------------------------------------------------------
  
  # Read in the exported harmonized datasets
  chla <- read_feather(chla_path)
  doc <- read_feather(doc_path)
  tss <- read_feather(tss_path)
  
  
  # Aggregate ---------------------------------------------------------------
  
  # If user indicates that only middle percentile values are desired then...
  if(middle_only){
    
    chla_agg <- chla %>%
      # Remove values 99.9% or above or 0.1% or below
      filter(quantile(harmonized_value, 0.999) >= harmonized_value,
             quantile(harmonized_value, 0.001) <= harmonized_value) %>%
      group_by(SiteID, date, lon, lat, datum) %>%
      summarize(mean_chla = mean(harmonized_value))
    
    doc_agg <- doc %>%
      filter(quantile(harmonized_value, 0.999) >= harmonized_value,
             quantile(harmonized_value, 0.001) <= harmonized_value) %>%
      group_by(SiteID, date, lon, lat, datum) %>%
      summarize(mean_doc = mean(harmonized_value))
    
    tss_agg <- tss %>%
      filter(quantile(harmonized_value, 0.999) >= harmonized_value,
             quantile(harmonized_value, 0.001) <= harmonized_value) %>%
      group_by(SiteID, date, lon, lat, datum) %>%
      summarize(mean_tss = mean(harmonized_value))
    
    # Otherwise...
  } else if(!middle_only){
    
    chla_agg <- chla %>%
      group_by(SiteID, date, lon, lat, datum) %>%
      summarize(mean_chla = mean(harmonized_value))
    
    doc_agg <- doc %>%
      group_by(SiteID, date, lon, lat, datum) %>%
      summarize(mean_doc = mean(harmonized_value))
    
    tss_agg <- tss %>%
      group_by(SiteID, date, lon, lat, datum) %>%
      summarize(mean_tss = mean(harmonized_value))
    
  }
  
  
  # Determine simultaneous points -------------------------------------------
  
  simultaneous <- reduce(.x = list(chla_agg, doc_agg, tss_agg),
                         .f = inner_join,
                         by = c('SiteID', 'date', 'lon', 'lat', 'datum'))
  
  # Inform the user of the dataset size
  message(sprintf(paste0("The final dataset contains %s simultaneous records."), 
                  nrow(simultaneous)))
  
  
  # Generate export-ready dataset -------------------------------------------
  
  simul_clean <- simultaneous %>%
    left_join(x = .,
              y = wqp_metadata %>%
                select(MonitoringLocationIdentifier, lat, lon,
                       type = ResolvedMonitoringLocationTypeName),
              by = c("SiteID" = "MonitoringLocationIdentifier",
                     "lat", "lon")) %>%
    select(SiteID, type, date, lat, lon, chla = mean_chla, doc = mean_doc,
           tss = mean_tss) %>%
    distinct()
  
  
  return(simul_clean)
  
}
















