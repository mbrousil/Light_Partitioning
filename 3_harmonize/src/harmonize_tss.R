harmonize_tss <- function(raw_tss, p_codes){
  
  # Minor data prep ---------------------------------------------------------
  
  raw_tss <- raw_tss %>% 
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    filter(
      # Water only
      media %in% c("Water","water"))  %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(.,"index")
  
  
  # Remove fails ------------------------------------------------------------
  
  tss_fails_removed <- raw_tss %>%
    # No failure-related field comments, slightly different list of words than
    # lab and result list (not including things that could be used to describe
    # field conditions like "warm", "ice", etc.):
    filter(!grepl(pattern = paste0(c(
      "fail", "suspect", "error", "beyond accept", "interference",
      "questionable", "outside of accept", "problem", "contaminat",
      "improper", "violation", "invalid", "unable", "no test", "cancelled",
      "instrument down", "no result", "time exceed", "not accept",
      "QC EXCEEDED"),
      collapse = "|"),
      x = field_comments,
      ignore.case = T) |
        is.na(field_comments),
      # No failure-related lab comments:
      !grepl(paste0(
        c("fail", "suspect", "error", "beyond accept", "interference",
          "questionable", "outside of accept", "problem", "contaminat",
          "improper", "warm", "violation", "invalid", "unable", "no test",
          "cancelled", "instrument down", "no result", "time exceed",
          "not accept", "QC EXCEEDED", "not ice", "ice melt",
          "PAST HOLDING TIME", "beyond", "exceeded", "failed", "exceededs"),
        collapse = "|"),
        lab_comments,
        ignore.case = T) |
        is.na(lab_comments),
      # no failure-related result comments:
      !grepl(paste0(
        c("fail", "suspect", "error", "beyond accept", "interference",
          "questionable", "outside of accept", "problem", "contaminat",
          "improper", "warm", "violation", "invalid", "unable", "no test",
          "cancelled", "instrument down", "no result", "time exceed",
          "not accept", "QC EXCEEDED", "not ice", "ice melt",
          "PAST HOLDING TIME", "null", "unavailable", "exceeded", "rejected"),
        collapse = "|"),
        result_comments,
        ignore.case = T) |
        is.na(result_comments),
      # no failure-related value comments:
      !grepl(paste0(
        c("fail", "suspect", "error", "beyond accept", "interference",
          "questionable", "outside of accept", "problem", "contaminat",
          "improper", "warm", "violation", "invalid", "unable", "no test",
          "cancelled", "instrument down", "no result", "time exceed",
          "not accept", "QC EXCEEDED", "not ice", "ice melt",
          "PAST HOLDING TIME", "not done", "no reading", 
          "not reported", "no data"),
        collapse = "|"),
        value,
        ignore.case = T) |
        is.na(value),
      # no failure-related detection comments:
      !grepl(paste0(
        c("fail", "suspect", "error", "beyond accept", "interference",
          "questionable", "outside of accept", "problem", "contaminat",
          "improper", "warm", "violation", "invalid", "unable", "no test",
          "cancelled", "instrument down", "no result", "time exceed",
          "not accept", "QC EXCEEDED", "not ice", "ice melt",
          "PAST HOLDING TIME"),
        collapse = "|"),
        ResultDetectionConditionText,
        ignore.case = T) |
        is.na(ResultDetectionConditionText))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(raw_tss) - nrow(tss_fails_removed)
    )
  )
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- tss_fails_removed %>%
    # only want NAs and character value data:
    filter(is.na(value_numeric)) %>%
    # if the value is na BUT there is non detect language in the comments...  
    mutate(
      mdl_vals = ifelse(test = (is.na(value) & 
                                  (grepl("non-detect|not detect|non detect|undetect|below", lab_comments, ignore.case = TRUE) | 
                                     grepl("non-detect|not detect|non detect|undetect|below", result_comments, ignore.case = TRUE) |
                                     grepl("non-detect|not detect|non detect|undetect|below", ResultDetectionConditionText, ignore.case = TRUE))) |
                          #.... OR, there is non-detect language in the value column itself....
                          grepl("non-detect|not detect|non detect|undetect|below", value, ignore.case = TRUE),
                        #... use the DetectionQuantitationLimitMeasure.MeasureValue value.
                        yes = DetectionQuantitationLimitMeasure.MeasureValue,
                        # if there is a `<` and a number in the values column...
                        no = ifelse(test = grepl("[0-9]", value) & grepl("<", value),
                                    # ... use that number as the MDL
                                    yes = str_replace_all(value, c("\\<"="", "\\*" = "", "\\=" = "" )),
                                    no = NA)),
      # preserve the units if they are provided:
      mdl_units = ifelse(!is.na(mdl_vals), DetectionQuantitationLimitMeasure.MeasureUnitCode, units),
      # zero = 0,
      half = as.numeric(mdl_vals) / 2)
  
  # Using the EPA standard for non-detects, select a random number between zero and HALF the MDL:
  mdl_updates$std_value <- with(mdl_updates, runif(nrow(mdl_updates), 0, half))
  mdl_updates$std_value[is.nan(mdl_updates$std_value)] <- NA
  
  # Keep important data
  mdl_updates <- mdl_updates %>%
    select(index, std_value, mdl_vals, mdl_units) %>%
    filter(!is.na(std_value))
  
  
  print(
    paste(
      round((nrow(mdl_updates)) / nrow(tss_fails_removed) * 100, 1),
      '% of samples had values listed as being below a detection limit'
    )
  )
  
  # Replace "harmonized_value" field with these new values
  tss_mdls_added <- tss_fails_removed %>%
    left_join(x = ., y = mdl_updates, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% mdl_updates$index, std_value, value_numeric),
           harmonized_units = ifelse(index %in% mdl_updates$index, mdl_units, units),
           harmonized_comments = ifelse(index %in% mdl_updates$index,
                                        "Approximated using the EPA's MDL method.", NA))
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  tss_approx <- tss_mdls_added %>%
    # First, remove the samples that we've already approximated using the EPA method:
    filter(!index %in% mdl_updates$index,
           # Then select fields where the numeric value column is NA....
           is.na(value_numeric) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", value) &
             # ...AND any of the comment fields have approximation language...
             (grepl("result approx|RESULT IS APPROX|value approx", lab_comments, ignore.case = T)|
                grepl("result approx|RESULT IS APPROX|value approx", result_comments, ignore.case = T )|
                grepl("result approx|RESULT IS APPROX|value approx", ResultDetectionConditionText, ignore.case = T)))
  
  tss_approx$approx_value <- as.numeric(str_replace_all(tss_approx$value, c("\\*" = "")))
  tss_approx$approx_value[is.nan(tss_approx$approx_value)] <- NA
  
  # Keep important data
  tss_approx <- tss_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(tss_approx)) / nrow(raw_tss) * 100, 3),
      '% of samples had values listed as approximated'
    )
  )
  
  # Replace harmonized_value field with these new values
  tss_approx_added <- tss_mdls_added %>%
    left_join(x = ., y = tss_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% tss_approx$index,
                                     approx_value,
                                     harmonized_value),
           harmonized_comments = ifelse(index %in% tss_approx$index,
                                        'Value identified as "approximated" by organization.',
                                        harmonized_comments))
  
  
  # Clean up "greater than" values ------------------------------------------
  
  greater_vals <- tss_approx_added %>%
    filter((!index %in% mdl_updates$index) & (!index %in% tss_approx$index)) %>%
    # Then select fields where the NUMERIC value column is NA....
    filter(is.na(value_numeric) & 
             # ... AND the original value column has numeric characters...
             grepl("[0-9]", value) &
             #... AND a `>` symbol
             grepl(">", value))
  
  greater_vals$greater_value <- as.numeric(str_replace_all(greater_vals$value,
                                                           c("\\>" = "", "\\*" = "", "\\=" = "" )))
  greater_vals$greater_value[is.nan(greater_vals$greater_value)] <- NA
  
  # Keep important data
  greater_vals <- greater_vals %>%
    select(greater_value, index)
  
  print(
    paste(
      round((nrow(greater_vals)) / nrow(raw_tss) * 100, 9),
      '% of samples had values listed as being above a detection limit//greater than'
    )
  )
  
  # Replace harmonized_value field with these new values
  tss_harmonized_values <- tss_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           harmonized_comments = ifelse(index %in% greater_vals$index,
                                        'Value identified as being greater than listed value.',
                                        harmonized_comments))
  
  # Free up memory
  rm(raw_tss)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Set up a lookup table so that final units are all in ug/L... 
  unit_conversion_table <- tibble(
    units = c('mg/L', 'mg/l', 'ppm', 'ug/l', 'ug/L', 'mg/m3',
              'ppb', 'mg/cm3', 'ug/ml', 'mg/ml', 'ppt', 'umol/L',
              'g/l'),
    conversion = c(1000, 1000, 1000, 1, 1, 1, 1, 1000000,
                   1000, 1000000, 0.000001, 60.080000, 1000000)
  )
  
  tss_harmonized_units <- tss_harmonized_values %>%
    # Drop nonsensical units using an inner join
    inner_join(unit_conversion_table, by = 'units') %>%
    # To avoid editing the tss_lookup, I'm converting ug/l to mg/l here:
    mutate(harmonized_value = (harmonized_value * conversion) / 1000,
           harmonized_unit = 'mg/L') %>%
    # MR limit
    filter(harmonized_value < 1000) 
  
  # How many records removed due to values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(tss_harmonized_values) - nrow(tss_harmonized_units)
    )
  )
  
  
  # Investigate depth -------------------------------------------------------
  
  # Define a depth lookup table to convert all depth data to meters. 
  depth_conversion_table <- tibble(sample_depth_unit = c('cm', 'feet', 'ft', 'in',
                                                         'm', 'meters'),
                                   depth_conversion = c(1 / 100, 0.3048, 0.3048,
                                                        0.0254, 1, 1)) 
  # Join depth lookup table to tss data
  tss_harmonized_depth <- inner_join(x = tss_harmonized_units,
                                     y = depth_conversion_table,
                                     by = c('sample_depth_unit')) %>%
    # Some depth measurements have negative values (assume that is just preference)
    # I also added .01 meters because many samples have depth of zero assuming they were
    # taken directly at the surface
    mutate(harmonized_depth = abs(as.numeric(sample_depth) * depth_conversion) + .01)
  
  # We lose lots of data by keeping only data with depth measurements
  print(
    paste(
      'If we only kept samples that had depth information we would lose',
      round((nrow(tss_harmonized_units) - nrow(tss_harmonized_depth)) / nrow(tss_harmonized_units) * 100, 1),
      '% of samples'))
  
  rm(tss_harmonized_depth)
  gc()
  
  
  # Aggregate analytical methods --------------------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of tss analytical methods present: ",
      length(unique(tss_harmonized_units$analytical_method))
    )
  )
  
  # Group together TSS methods into useful categories
  tss_aggregated_methods <- tss_harmonized_units %>%
    mutate(method_status = case_when(
      # "Gold standard" method mostly via analytical_method field:
      grepl("2540D|2540 D|105|103|160.2",
            analytical_method,
            ignore.case = T) |
        # ... also use USGS p-codes to identify the data performed with "gold standard" method:
        grepl(paste0(c("Suspended solids, water, unfiltered, milligrams per liter",
                       "Suspended solids, dried at 105 degrees Celsius, water",
                       "Suspended solids dried at 105 degrees Celsius, water, unfiltered"),
                     collapse = "|"), 
              parameter_name_description,
              ignore.case = T) ~ "SM 2540 B/EPA 160.2",
      # This one seems appropriate but is heated at 110 deg Celsius:
      grepl("Suspended solids dried at 110 degrees Celsius, water, unfiltered", 
            parameter_name_description,
            ignore.case = T) ~ "TSS USGS 110",
      # This one may be appropriate to still consider:
      grepl("2540 F|Settlable Solids|Settleable",
            analytical_method,
            ignore.case = T) ~ "Settleable Solids ",
      grepl("2540C|2540 C|Total Dissolved|160.1|TDS|TOTAL FILTRATABLE RESIDUE",
            analytical_method,
            ignore.case = T) ~ "Nonsensical",
      grepl("160.4|2540 E|Ashing|Volatile Residue",
            analytical_method,
            ignore.case = T) ~ "Nonsensical", 
      grepl("Percent Solids|Total Solids|2540B|Total, Fixed and Volatile",
            analytical_method,
            ignore.case = T) ~ "Nonsensical",
      # Clearly TSS, but not exactly sure how it was performed
      grepl(paste0(c("Nonfilterable Solids", "Non-filterable Residue by Filtration and Drying",
                     "Total Nonfilterable Residue", "RESIDUE, TOTAL NONFILTRABLE",
                     "Non-Filterable Residue - TSS", "Total Suspended Solids in Water",
                     "Total Suspended Solids", "TOTAL NONFILTRATABLE RESIDUE",
                     "Suspended-Sediment in Water", "Residue Non- filterable (TSS)", "TSS",
                     "Residue by Evaporation and Gravimetric"),
                   collapse = "|"),
            analytical_method,
            ignore.case = T) ~ "Ambiguous TSS",
      grepl(paste0(c("Oxygen", "Nitrogen", "Ammonia", "Metals", "E. coli", "Coliform",
                     "Carbon", "Anion", "Cation", "Phosphorus", "Silica", "PH", "HARDNESS",
                     "Nutrient", "Turbidity", "Temperature", "Nitrate", "Conductance",
                     "Conductivity", "Alkalinity", "Chlorophyll", "SM ", "EPA ", "2540 G"),
                   collapse = "|"),
            analytical_method,
            ignore.case = T) ~ "Nonsensical",
      grepl(paste0(c("UNKOWN", "SSC by filtration (D3977;WI WSC)",
                     "Sediment conc by evaporation", "Historic", "Filterable Residue - TDS",
                     "Cheyenne River Sioux Tribe Quality Assurance Procedures"),
                   collapse = "|"),
            analytical_method,
            ignore.case = T) ~ "Nonsensical",
      # This fills the rest as ambiguous. Should include things like local
      # SOPs, not known, etc.
      TRUE ~ "Ambiguous")) 
  
  tss_filter_aggregates <- tss_aggregated_methods %>%
    filter(!grepl(pattern = "nonsensical",
                  x = method_status,
                  ignore.case = TRUE))
  
  # How many records removed due to methods?
  print(
    paste0(
      "Rows removed due to analytical method type: ",
      nrow(tss_aggregated_methods) - nrow(tss_filter_aggregates)
    )
  )
  
  
  # Filter fractions --------------------------------------------------------
  
  # Filter out bad fractions
  
  # These fractions don't make sense for TSS, so should be removed. KW feels that
  # many of the remaining fractions are open to interpretation, and doesn't want to 
  # filter them out
  tss_remove_fractions <- tss_filter_aggregates %>%
    filter(!fraction %in% c('Fixed', 'Volatile', 'Dissolved', 'Acid Soluble'))
  
  # How many records removed due to fraction?
  print(
    paste0(
      "Rows removed due to fraction type: ",
      nrow(tss_filter_aggregates) - nrow(tss_remove_fractions)
    )
  )
  
  # Export ------------------------------------------------------------------
  
  # Export in memory-friendly way
  out_path <- "3_harmonize/out/harmonized_tss.feather"
  
  write_feather(tss_remove_fractions,
                out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(tss_remove_fractions)
    )
  )
  
  return(out_path)
  
}