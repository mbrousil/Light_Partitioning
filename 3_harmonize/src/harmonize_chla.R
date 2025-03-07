
harmonize_chla <- function(raw_chla, p_codes, chla_analytical_method_matchup){
  
  # Minor data prep ---------------------------------------------------------
  
  # First step is to read in the data and do basic formatting and filtering
  raw_chla <- raw_chla %>%
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    filter(
      media %in% c("Water", "water")) %>%
    # Add an index to control for cases where there's not enough identifying info
    # to track a unique record
    rowid_to_column(., "index")
  
  
  # Parameter name selection ------------------------------------------------
  
  chla_param_filter <- raw_chla %>%
    filter(
      orig_parameter %in% c('Chlorophyll a', 'Chlorophyll a (probe relative fluorescence)',
                            'Chlorophyll a, corrected for pheophytin', 'Chlorophyll a (probe)',
                            'Chlorophyll a, free of pheophytin', 'Chlorophyll a - Phytoplankton (suspended)'))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to non-target parameter names: ",
      nrow(raw_chla) - nrow(chla_param_filter)
    )
  )
  
  
  # Remove fails ------------------------------------------------------------
  
  chla_fails_removed <- chla_param_filter %>%
    filter(
      # REMOVE failure-related field comments, slightly different list of words
      # than lab and result list (not including things that could be used
      # to describe field conditions like "warm", "ice", etc.)
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "violation", "invalid", "unable", "no test", "cancelled",
            "instrument down", "no result", "time exceed", "not accept",
            "QC EXCEEDED"),
          collapse = "|"),
        x = field_comments,
        ignore.case = T
      ) |
        is.na(field_comments),
      # Remove failure-related lab (What about controls comments?):
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "beyond", "exceeded", "failed", "exceededs"),
          collapse = "|"),
        x = lab_comments,
        ignore.case = T
      ) |
        is.na(lab_comments),
      # Remove failure-related result comments
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "null", "unavailable", "exceeded", "rejected"),
          collapse = "|"),
        x = result_comments,
        ignore.case = T
      ) | is.na(result_comments),
      # No failure-related values
      !grepl(
        pattern = paste0(
          c("fail", "suspect", "error", "beyond accept", "interference",
            "questionable", "outside of accept", "problem", "contaminat",
            "improper", "warm", "violation", "invalid", "unable", "no test",
            "cancelled", "instrument down", "no result", "time exceed",
            "not accept", "QC EXCEEDED", "not ice", "ice melt",
            "PAST HOLDING TIME", "not done", "no reading", "not reported",
            "no data"),
          collapse = "|"),
        x = value,
        ignore.case = T
      ) | is.na(value))
  
  # How many records removed due to fails, missing data, etc.?
  print(
    paste0(
      "Rows removed due to fails, missing data, etc.: ",
      nrow(chla_param_filter) - nrow(chla_fails_removed)
    )
  )
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- chla_fails_removed %>%
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
      round((nrow(mdl_updates)) / nrow(chla_fails_removed) * 100, 1),
      '% of samples had values listed as being below a detection limit'
    )
  )
  
  # Replace "harmonized_value" field with these new values
  chla_mdls_added <- chla_fails_removed %>%
    left_join(x = ., y = mdl_updates, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% mdl_updates$index, std_value, value_numeric),
           harmonized_units = ifelse(index %in% mdl_updates$index, mdl_units, units),
           harmonized_comments = ifelse(index %in% mdl_updates$index,
                                        "Approximated using the EPA's MDL method.", NA))
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  chla_approx <- chla_mdls_added %>%
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
  
  chla_approx$approx_value <- as.numeric(str_replace_all(chla_approx$value, c("\\*" = "")))
  chla_approx$approx_value[is.nan(chla_approx$approx_value)] <- NA
  
  # Keep important data
  chla_approx <- chla_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(chla_approx)) / nrow(chla_mdls_added) * 100, 3),
      '% of samples had values listed as approximated'
    )
  )
  
  # Replace harmonized_value field with these new values
  chla_approx_added <- chla_mdls_added %>%
    left_join(x = ., y = chla_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% chla_approx$index,
                                     approx_value,
                                     harmonized_value),
           harmonized_comments = ifelse(index %in% chla_approx$index,
                                        'Value identified as "approximated" by organization.',
                                        harmonized_comments))
  
  # Clean up "greater than" values ------------------------------------------
  
  greater_vals <- chla_approx_added %>%
    filter((!index %in% mdl_updates$index) & (!index %in% chla_approx$index)) %>%
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
      round((nrow(greater_vals)) / nrow(chla_approx_added) * 100, 9),
      '% of samples had values listed as being above a detection limit//greater than'
    )
  )
  
  # Replace harmonized_value field with these new values
  chla_harmonized_values <- chla_approx_added %>%
    left_join(x = ., y = greater_vals, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% greater_vals$index,
                                     greater_value, harmonized_value),
           harmonized_comments = ifelse(index %in% greater_vals$index,
                                        'Value identified as being greater than listed value.',
                                        harmonized_comments))
  
  # Free up memory
  rm(raw_chla)
  gc()
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Now count the units column: 
  unit_counts <- chla_harmonized_values %>%
    count(units) %>%
    arrange(desc(n))
  
  unit_conversion_table <- tibble(
    units = c("mg/l", "mg/L", "ppm", "ug/l", "ug/L", "mg/m3", "ppb", "mg/cm3", "ug/ml", "mg/ml", "ppt"),
    conversion = c(1000, 1000, 1000, 1, 1, 1, 1, 1000000, 1000, 1000000, 1000000)
  )
  
  converted_units_chla <- chla_harmonized_values %>%
    inner_join(x = .,
               y = unit_conversion_table,
               by = "units") %>%
    mutate(harmonized_value = value_numeric * conversion,
           harmonized_unit = "ug/L") %>%
    # MR limit 
    filter(harmonized_value < 1000)
  
  # How many records removed due to limits on values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(chla_harmonized_values) - nrow(converted_units_chla)
    )
  )
  
  
  # Clean up depths ---------------------------------------------------------
  
  # As with value col, check for entries with potential salvageable data. But don't
  # create a flag column for this one
  salvage_depths <- converted_units_chla %>%
    filter(grepl(x = sample_depth, pattern = "-|>|<|=")) %>%
    count(sample_depth)
  
  salvage_depth_units <- converted_units_chla %>%
    count(sample_depth_unit)
  
  depth_unit_conversion_table <- tibble(
    depth_units = c("in", "ft", "feet", "cm", "m", "meters"),
    depth_conversion = c(0.0254, 0.3048, 0.3048, 0.01, 1, 1)
  )
  
  converted_depth_units_chla <- converted_units_chla %>%
    left_join(x = .,
               y = depth_unit_conversion_table,
               by = c("sample_depth_unit" = "depth_units")) %>%
    mutate(harmonized_depth_value = as.numeric(sample_depth) * depth_conversion,
           harmonized_depth_unit = "m") %>%
    # Surface limits - for the time being using two columns for this. Make sure
    # numeric value is within +/-2m OR the raw character version indicates something
    # similar:
    filter(abs(harmonized_depth_value) <= 2 |
             sample_depth %in% c("0-2", "0-0.5")|
             # Don't want to lose all the NA depth records
             is.na(sample_depth))
  
  # How many records removed due to limits on depth?
  print(
    paste0(
      "Rows removed due to non-target depths: ",
      nrow(converted_units_chla) - nrow(converted_depth_units_chla)
    )
  )
  
  
  # Aggregate analytical methods --------------------------------------------
  
  # Get an idea of how many analytical methods exist:
  print(
    paste0(
      "Number of chla analytical methods present: ",
      length(unique(converted_depth_units_chla$analytical_method))
    )
  )
  
  analytical_counts <- converted_depth_units_chla %>%
    count(analytical_method) %>%
    arrange(desc(n))
  
  # Add a new column aggregating the analytical methods groups, then filter out
  # unlikely ones
  grouped_analytical_methods_chla <- converted_depth_units_chla %>%
    left_join(x = .,
              y = chla_analytical_method_matchup,
              by = c("analytical_method")) %>%
    filter(!is.na(analytical_method_grouping),
           analytical_method_grouping != "unlikely")
  
  # How many records removed due to limits on analytical method?
  print(
    paste0(
      "Rows removed due to unlikely analytical methods: ",
      nrow(converted_depth_units_chla) - nrow(grouped_analytical_methods_chla)
    )
  )
  
  
  # Filter fractions --------------------------------------------------------
  
  # Now count the fraction column
  fraction_counts <- grouped_analytical_methods_chla %>%
    count(fraction) %>%
    arrange(desc(n))
  
  # Create a column to lump things that do/don't make sense for the fraction column
  grouped_fractions_chla <- grouped_analytical_methods_chla %>%
    mutate(aquasat_fraction = if_else(
      condition = fraction %in% c("Non-Filterable (Particle)", "Suspended",
                                  "Non-filterable", "<Blank>", "Acid Soluble"),
      true = "Nonsensical",
      false = "Makes sense")) %>%
    filter(aquasat_fraction == "Makes sense")
  
  # How many records removed due to unlikely fraction types?
  print(
    paste0(
      "Rows removed due to unlikely fraction type: ",
      nrow(grouped_analytical_methods_chla) - nrow(grouped_fractions_chla)
    )
  )
  
  
  # Aggregate sample methods ------------------------------------------------
  
  # Get an idea of how many sample methods exist:
  print(
    paste0(
      "Number of chla sample methods present in raw dataset: ",
      length(unique(grouped_fractions_chla$sample_method)),
      ". Skipping due to length."
    )
  )
  
  
  # Aggregate collection equipment ------------------------------------------
  
  # Get an idea of how many equipment types exist:
  print(
    paste0(
      "Number of chla equipment types present in raw dataset: ",
      length(unique(grouped_fractions_chla$collection_equipment)),
      ". Skipping due to length."
    )
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Export in memory-friendly way
  data_out_path <- "3_harmonize/out/harmonized_chla.feather"
  
  write_feather(grouped_fractions_chla,
                data_out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(grouped_fractions_chla)
    )
  )
  
  return(data_out_path)
  
}
