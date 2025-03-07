
harmonize_doc <- function(raw_doc, p_codes){
  
  # Minor data prep ---------------------------------------------------------
  
  raw_doc <- raw_doc %>% 
    # Link up USGS p-codes. and their common names can be useful for method lumping:
    left_join(x = ., y = p_codes, by = "parm_cd") %>%
    filter(
      # Water only
      media %in% c("Water","water"))  %>%
    # Index for being able to track each sample
    rowid_to_column(.,"index")
  
  
  # Remove fails ------------------------------------------------------------
  
  doc_fails_removed <- raw_doc %>%
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
      nrow(raw_doc) - nrow(doc_fails_removed)
    )
  )
  
  
  # Clean up MDLs -----------------------------------------------------------
  
  # Find MDLs and make them usable as numeric data
  mdl_updates <- doc_fails_removed %>%
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
      round((nrow(mdl_updates)) / nrow(doc_fails_removed) * 100, 1),
      '% of samples had values listed as being below a detection limit'
    )
  )
  
  # Replace "harmonized_value" field with these new values
  doc_mdls_added <- doc_fails_removed %>%
    left_join(x = ., y = mdl_updates, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% mdl_updates$index, std_value, value_numeric),
           harmonized_units = ifelse(index %in% mdl_updates$index, mdl_units, units),
           harmonized_comments = ifelse(index %in% mdl_updates$index,
                                        "Approximated using the EPA's MDL method.", NA))
  
  
  # Clean up approximated values --------------------------------------------
  
  # Next step, incorporating and flagging "approximated" values. Using a similar
  # approach to our MDL detection, we can identify value fields that are labelled
  # as being approximated.
  
  doc_approx <- doc_mdls_added %>%
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
  
  doc_approx$approx_value <- as.numeric(str_replace_all(doc_approx$value, c("\\*" = "")))
  doc_approx$approx_value[is.nan(doc_approx$approx_value)] <- NA
  
  # Keep important data
  doc_approx <- doc_approx %>%
    select(approx_value, index)
  
  print(
    paste(
      round((nrow(doc_approx)) / nrow(raw_doc) * 100, 3),
      '% of samples had values listed as approximated'
    )
  )
  
  # Replace harmonized_value field with these new values
  doc_approx_added <- doc_mdls_added %>%
    left_join(x = ., y = doc_approx, by = "index") %>%
    mutate(harmonized_value = ifelse(index %in% doc_approx$index,
                                     approx_value,
                                     harmonized_value),
           harmonized_comments = ifelse(index %in% doc_approx$index,
                                        'Value identified as "approximated" by organization.',
                                        harmonized_comments))
  
  
  # Clean up "greater than" values ------------------------------------------
  
  # None present currently. Skip for now
  
  
  # Harmonize value units ---------------------------------------------------
  
  # Set up a lookup table so that final units are all in ug/L. 
  unit_conversion_table <- tibble(units = c('mg/L', 'mg/l', 'ppm', 'ug/l', 'ug/L', 'mg/m3',
                                            'ppb', 'mg/cm3', 'ug/ml', 'mg/ml', 'ppt', 'umol/L'),
                                  conversion = c(1000, 1000, 1000, 1, 1, 1, 1, 1000000, 1000,
                                                 1000000, 0.000001, 60.080000))
  
  doc_harmonized_units <- doc_approx_added %>%
    inner_join(unit_conversion_table, by = 'units') %>%
    mutate(harmonized_value = (value_numeric * conversion) / 1000,
           harmonized_unit = 'mg/L') %>%
    # MR limit
    filter(harmonized_value < 50)
  
  # How many records removed due to values?
  print(
    paste0(
      "Rows removed while harmonizing units: ",
      nrow(doc_approx_added) - nrow(doc_harmonized_units)
    )
  )
  
  
  # Investigate depth -------------------------------------------------------
  
  # Define a depth lookup table to convert all depth data to meters. 
  depth_conversion_table <- tibble(sample_depth_unit = c('cm', 'feet', 'ft', 'in',
                                                         'm', 'meters'),
                                   depth_conversion = c(1 / 100, 0.3048, 0.3048,
                                                        0.0254, 1, 1)) 
  # Join depth lookup table to doc data
  doc_harmonized_depth <- inner_join(x = doc_harmonized_units,
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
      round((nrow(doc_harmonized_units) - nrow(doc_harmonized_depth)) / nrow(doc_harmonized_units) * 100, 1),
      '% of samples'))
  
  rm(doc_harmonized_depth)
  gc()
  
  
  # Aggregate and tier analytical methods -----------------------------------
  
  doc_aggregated_methods <- doc_harmonized_units %>%
    # KW: includes more phrases that are nonsensical
    filter(!grepl(
      paste0(
        c("Oxygen", "Nitrogen", "Ammonia", "Metals", "E. coli", "Anion", "Cation",
          "Phosphorus", "Silica", "PH", "HARDNESS", "Nutrient", "Turbidity",
          "Nitrate", "Conductance", "Alkalinity", "Chlorophyll", "Solids",
          "Temperature", "Color in Water", "Coliform", "PARTICULATE CARBON (inorg+org)",
          "5210", "bed sed", "bs, calc", "5220", "Suspended-Sediment in Water"),
        collapse = "|"),
      x = analytical_method,
      ignore.case = TRUE)) %>%
    mutate(method_status = case_when(
      
      # DOC-specific:
      grepl("5310 B ~ Total Organic Carbon by Combustion-Infrared Method|Total Organic Carbon by Combustion|
          5310 B ~ Total Organic Carbon by High-Temperature Combustion Method|SM5310B|
          Organic-C, combustion-IR method|EPA 415.1|SM 5310 B|EPA 415.1M|TOC, combustion (SM5310B,COWSC)|
          DOC, combustion, NDIR (SM5310B)|TOC, combustion & CO2 detection|415.1|TOC, wu, combustion (5310B;DDEC)|
          SM185310B|DOC, wf, combustion (5310B;DDEC)|EPA Method 415.1 for Total Organic Carbon in aqueous matrices|
          SM 5310 B v20|DOC, 0.45u silver, persulfate IR",
            analytical_method,ignore.case = T) ~ "5310B + EPA 415.1 - Combustion + IR",
      
      grepl("5310 C ~ Total Organic Carbon in Water- Ultraviolet Oxidation Method|UV OR HEATED PERSULFATE OXIDATION|
          DOC, UV/persulfate (NYWSC; ALSC)|415.2|DOC, persulfate oxidation & IR|
          SM5310C|SM 5310 C|TOC, persulfate oxid (5310C; PA)|TOC, wu, persulfate (SM5310C;CO)|DOC, persulfate oxid, IR (COWSC)|
          Dissolved Organic Carbon in Water by Persulfate Oxidation and Infrared Spectrometry|TOC, persulfate-UV oxid (NYSDEC)|
          TOTAL ORGANIC CARBON (TOC) PERSULFATE-ULTRAVIOLET|TOC - Persulfate-Ultraviolet or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;ND)|TOC, wu, persulfate (SM5310C;ND)|TOC, UV/persulfate/IR (USGS-NYL)|
          DOC, persulfate oxid (5310C; PA)|EPA 415.2|DOC, UV/persulfate (NYWSC; KECK)|
          5310 C ~ Total Organic Carbon by High-Temperature Combustion Method|415.2 M ~ Total Organic Carbon in Water|
          SM 5310 C, EPA 415.3|5310 C ~ Total organic carbon by Persulfate-UV or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;CO)", 
            analytical_method,ignore.case = T) ~ "5310C + USGS O-1122-92 + EPA 415.2 - Persulfate-UV/Heated Persulfate Oxidation + IR",
      
      grepl("TOC, wet oxidation|WET OXIDATION METHOD|DOC,0.45um cap,acid,persulfateIR|
    5310 D ~ Total Organic Carbon in Water- Wet-Oxidation Method|DOC, wf, 0.45 um cap, combust IR|415.3|
    Determination of Total Organic Carbon and Specific UV Absorbance at 254 nm in Source Water and Drinking Water|
    EPA 415.3|SM 5310 D|O-3100 ~ Total Organic Carbon in Water", 
            analytical_method,ignore.case = T) ~ "5310D + EPA 415.3 + USGS O-3100 - Wet Oxidation + Persulfate + IR",
      
      grepl("440 W  ~ Determination of Carbon and Nitrogen", 
            analytical_method,ignore.case = T) ~ "EPA 440.0",
      
      grepl("9060 A ~ Total Organic Carbon in water and wastes by Carbonaceous Analyzer|9060 AM ~ Total Volatile Organic Carbon|
          EPA 9060|EPA 9060A",
            analytical_method,ignore.case = T) ~ "EPA 9060A - Carbonaceous Analyzer",
      (!grepl("5310 B ~ Total Organic Carbon by Combustion-Infrared Method|Total Organic Carbon by Combustion|
          5310 B ~ Total Organic Carbon by High-Temperature Combustion Method|SM5310B|
          Organic-C, combustion-IR method|EPA 415.1|SM 5310 B|EPA 415.1M|TOC, combustion (SM5310B,COWSC)|
          DOC, combustion, NDIR (SM5310B)|TOC, combustion & CO2 detection|415.1|TOC, wu, combustion (5310B;DDEC)|
          SM185310B|DOC, wf, combustion (5310B;DDEC)|EPA Method 415.1 for Total Organic Carbon in aqueous matrices|
          SM 5310 B v20|DOC, 0.45u silver, persulfate IR|5310 C ~ Total Organic Carbon in Water- Ultraviolet Oxidation Method|
          UV OR HEATED PERSULFATE OXIDATION|
          DOC, UV/persulfate (NYWSC; ALSC)|415.2|DOC, persulfate oxidation & IR|
          SM5310C|SM 5310 C|TOC, persulfate oxid (5310C; PA)|TOC, wu, persulfate (SM5310C;CO)|DOC, persulfate oxid, IR (COWSC)|
          Dissolved Organic Carbon in Water by Persulfate Oxidation and Infrared Spectrometry|TOC, persulfate-UV oxid (NYSDEC)|
          TOTAL ORGANIC CARBON (TOC) PERSULFATE-ULTRAVIOLET|TOC - Persulfate-Ultraviolet or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;ND)|TOC, wu, persulfate (SM5310C;ND)|TOC, UV/persulfate/IR (USGS-NYL)|
          DOC, persulfate oxid (5310C; PA)|EPA 415.2|DOC, UV/persulfate (NYWSC; KECK)|
          5310 C ~ Total Organic Carbon by High-Temperature Combustion Method|415.2 M ~ Total Organic Carbon in Water|
          SM 5310 C, EPA 415.3|5310 C ~ Total organic carbon by Persulfate-UV or Heated-Persulfate Oxidation Method|
          DOC, wu, persulfate (SM5310C;CO)|TOC, wet oxidation|WET OXIDATION METHOD|DOC,0.45um cap,acid,persulfateIR|
    5310 D ~ Total Organic Carbon in Water- Wet-Oxidation Method|DOC, wf, 0.45 um cap, combust IR|415.3|
    Determination of Total Organic Carbon and Specific UV Absorbance at 254 nm in Source Water and Drinking Water|
    EPA 415.3|SM 5310 D|O-3100 ~ Total Organic Carbon in Water|9060 A ~ Total Organic Carbon in water and wastes by Carbonaceous Analyzer|9060 AM ~ Total Volatile Organic Carbon|
          EPA 9060|EPA 9060A",
              # KW: include NA values in the "Ambiguous" method lump
              analytical_method,ignore.case = T) & !is.na(analytical_method)) | is.na(analytical_method) ~ "Ambiguous"))
  
  doc_grouped_more <- doc_aggregated_methods %>% 
    mutate(grouped = case_when(grepl(pattern = "5310B", 
                                     x = method_status) ~ "Combustion+IR",
                               grepl(pattern = "5310C", 
                                     x = method_status) ~ "Persulfate-UV/Heated Persulfate Oxidation+IR",
                               grepl(pattern = "5310D", 
                                     x = method_status) ~ "Wet Oxidation+Persulfate+IR",
                               grepl(pattern = "EPA 440.0", 
                                     x = method_status) ~ "Elemental Analyzer",
                               grepl(pattern = "EPA 9060A", 
                                     x = method_status) ~ "Carbonaceous Analyzer",
                               method_status == "Ambiguous" ~ "Ambiguous"))
  
  rm(doc_aggregated_methods)
  gc()
  
  doc_tiered <- doc_grouped_more %>%
    mutate(tiers = case_when(grouped %in% c("Wet Oxidation+Persulfate+IR",
                                            "Persulfate-UV/Heated Persulfate Oxidation+IR") ~ "Restrictive",
                             grouped == "Combustion+IR" ~ "Narrowed",
                             grouped %in% c("Ambiguous", "Carbonaceous Analyzer") ~ "Inclusive")) 
  # KW: no longer filter out any of the tiers
  doc_filter_tiers <- doc_tiered # %>%
  #filter(tiers %in% c("Narrowed", "Restrictive",))
  
  # How many records removed due to methods?
  print(
    paste0(
      "Rows removed due to analytical method type: ",
      nrow(doc_harmonized_units) - nrow(doc_filter_tiers)
    )
  )
  
  
  # Filter fractions --------------------------------------------------------
  
  # Final step in harmonization is to group and subset fractions of interest
  doc_harmonized <- doc_filter_tiers %>%
    mutate(
      harmonized_fraction = case_when(
        fraction %in% c('Dissolved', 'Filtered, lab', 'Filterable', 
                        'Filtered, field') ~ "Dissolved",
        fraction %in% c('Total', 'Total Recovrble', 'Total Recoverable',
                        'Recoverable', 'Unfiltered', "Acid Soluble", "Suspended",
                        "Non-Filterable (Particle)") ~ "Total",
        fraction %in% c('Fixed') ~ "Fixed",
        fraction %in% c('Non-Filterable (Particle)') ~ 'Particle',
        is.na(fraction) | fraction %in% c(" ", "Field", "Bed Sediment",
                                          "Inorganic", "Organic") ~ "Ambiguous")
    ) %>%
    # Filter to dissolved fraction
    filter(harmonized_fraction == "Dissolved")
  
  # How many records removed due to methods? Similar number as AquaSat(1)
  print(
    paste0(
      "Rows removed due to fraction type: ",
      nrow(doc_filter_tiers) - nrow(doc_harmonized)
    )
  )
  
  
  # Export ------------------------------------------------------------------
  
  # Export in memory-friendly way
  out_path <- "3_harmonize/out/harmonized_doc.feather"
  
  write_feather(doc_harmonized,
                out_path)
  
  # Final dataset length:
  print(
    paste0(
      "Final number of records: ",
      nrow(doc_harmonized)
    )
  )
  
  return(out_path)
  
}