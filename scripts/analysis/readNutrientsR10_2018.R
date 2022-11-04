# This script is for reading nutrient data from Region 10 (R10) sampling
# in 2018.  Data are at: …data/chemistry/nutrients/2018_ESF-EFWS_NutrientData_Updated03012019_SS_CTNUpdate04012019.xlsx

# Data unique identifiers (e.g., site_id field in Excel file) follows the QAPP
# for the 2018 sampling ("...\projectDocuments\QAPP, SOPs, manuals, HASP\QAPP for an assessment of methane emissions from Region 10 reservoirs.docx")

# All samples were unfiltered, hence all analytes are reported with a "T" in the 
# name (e.g. TNH4, rather than NH4).  Just ignore this and adopt the analyte names
# defined in github issue #10.

# Samples were collected at an open-water and tributary site.  We only want the
# open_water site data.

# function reads in data, filters data, and creates all new columns
get_awberc_data <- function(path, data, sheet) { 
  
  laketable <- data.frame( # create df of lake & site ids to convert values
    lake_id = c("LVR", "SFR", "LGR", "MMR", "PLR", "WPL", "SFP", "BKL"),
    surgename = c("253", "331", "302", "323", "239", "308", "263", "999"),
    site_id = c("SU05", "U22", "U10", "U04", "SU03", "U01", "U06", "U10"), 
    stringsAsFactors = FALSE)
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, skip = 1) %>%
    janitor::clean_names() %>% # clean up names for rename and select, below
    janitor::remove_empty(which = c("rows", "cols")) %>% # remove empty rows
    rename(rdate = collection_date_cdate, # rename fields
           ddate = analyte_detection_date_ddate,
           finalConc = peak_concentration_corrected_for_dilution_factor,
           analyte = analyte_name_analy,
           tp = tp_adjusted_concentration_full_series_ug_p_l, 
           lake_id = site_id_id,
           crossid = c_ross_id_pos, 
           site_id = long_id_subid,
           sample_type = type, 
           rep = rep_number) %>%
    inner_join(y = laketable, by = c("lake_id","site_id")) %>% # keep open water SuRGE sites
    select(-lake_id) %>% # remove the old 2018 lake_id
    rename(lake_id = surgename) %>% # replace lake_id with the SuRGE lake code column
    mutate(finalConc = ifelse( # correct TP  are in tp
      analyte == "TP",
      tp,
      finalConc)) %>%
    filter(sample_type != "SPK") %>% # exclude matrix spike 
    mutate(crossid = case_when( # convert sample depths (all 2018 is shallow)
      grepl("BLK", sample_type, ignore.case = TRUE) ~ "blank", # report "blank" depth for blanks
      crossid == 0.1 ~ "shallow",
      TRUE ~ crossid)) %>%
    rename(sample_depth = crossid) %>% # rename sample depth column
    mutate(nutrients_qual = if_else( # determine if holding time exceeded
      (as.Date(ddate) - as.Date(rdate)) > 28, "H", "")) %>% # TRUE = hold time violation
    select(lake_id, site_id, sample_depth, sample_type, analyte,
           finalConc, nutrients_qual, rep) %>%
    mutate(sample_type = case_when( # recode sample type identifiers
      grepl("DUP", sample_type, ignore.case = TRUE) ~ "duplicate",
      grepl("UKN", sample_type, ignore.case = TRUE) ~ "unknown",
      grepl("BLK", sample_type, ignore.case = TRUE) ~ "blank",
      TRUE ~ sample_type)) %>%
    mutate(analyte = str_to_lower(analyte)) %>% # make analyte names lowercase
    mutate(analyte = case_when( # change analyte names where necessary
      analyte == "trp" ~ "op",
      analyte == "tnh4" ~ "nh4",
      analyte == "tno2" ~ "no2",
      analyte == "tno2-3" ~ "no2_3",
      TRUE   ~ analyte)) %>%
    mutate(units = case_when( # add units column
      analyte == "nh4"  ~ "ug_n_l",
      analyte == "no2"  ~ "ug_n_l",
      analyte == "no2_3"  ~ "ug_n_l",
      analyte == "op"  ~ "ug_p_l",
      analyte == "tn"  ~ "ug_n_l",
      analyte == "tp"  ~ "ug_p_l",
      TRUE ~ ""))  %>%
    mutate(analyte_flag = case_when( # create the analyte_flag column
      analyte == "nh4" & finalConc < 6 ~ "ND",
      analyte == "no2" & finalConc < 6 ~ "ND",
      analyte == "no2_3" & finalConc < 6 ~ "ND",
      analyte == "op" & finalConc < 3 ~ "ND",
      analyte == "tn" & finalConc < 25 ~ "ND",
      analyte == "tp" & finalConc < 5 ~ "ND",
      TRUE ~ "")) %>%
    mutate(analyte_bql = case_when( # create the analyte_bql column
      analyte == "nh4" & finalConc < 20 ~ "L",
      analyte == "no2" & finalConc < 20 ~ "L",
      analyte == "no2_3" & finalConc < 20 ~ "L",
      analyte == "op" & finalConc < 5 ~ "L",
      analyte == "tn" & finalConc < 30 ~ "L",
      analyte == "tp" & finalConc < 5 ~ "L",
      TRUE ~ "")) %>%
    # if analyte_flag is ND, remove the analyte_bql L flag
    mutate(analyte_bql = if_else(analyte_flag == "ND", "", analyte_bql)) %>%
    mutate(finalConc = case_when( #
      analyte == "nh4" & finalConc < 6 ~ 6,
      analyte == "no2" & finalConc < 6 ~ 6,
      analyte == "no2_3" & finalConc < 6 ~ 6,
      analyte == "op" & finalConc < 3 ~ 3,
      analyte == "tn" & finalConc < 25 ~ 25,
      analyte == "tp" & finalConc < 5 ~ 5,
      TRUE ~ finalConc)) %>%
    mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) # remove non-numeric chars
  
  # mutate(sample_type = case_when(
  #   sample_type == "duplicate" ~ "unknown",
  #   TRUE ~ sample_type)) %>%
  
  return(d)
  
  
}


# function aggregates dups, renames/sorts columns, and casts to wide
dup_agg <- function(data) { 
  
  # aggregate dups and convert analyte_flags to numeric (for summarize operations)
  e <- data %>%
    mutate(analyte_flag = if_else(
      str_detect(analyte_flag, "ND"), 1, 0)) %>% # convert to numeric
    mutate(analyte_bql = if_else(
      str_detect(analyte_bql, "L"), 1, 0)) %>% # convert to numeric   
    mutate(sample_type = case_when(
      sample_type == "duplicate" ~ "unknown",
      TRUE ~ sample_type)) %>%
    group_by(lake_id, site_id, analyte, sample_depth, 
             sample_type, rep, nutrients_qual, units) %>%
    summarize(value = mean(finalConc, na.rm = TRUE), # group and calculate means
              analyte_flag = mean(analyte_flag, na.rm = TRUE), 
              analyte_bql = mean(analyte_bql, na.rm = TRUE)) %>%
    mutate(sample_type = case_when(
      rep == "2" ~ "duplicate", TRUE ~ sample_type)) 
  
  # cast to wide, convert analyte flags to text, and convert any NaN to NA
  f <- e %>%
    pivot_wider(names_from = analyte, 
                values_from = c(value, analyte_flag, analyte_bql,  
                                units, nutrients_qual)) %>% # cast to wide
    mutate(across(
      contains("flag"), # convert all _flag values back to text (< or blank)
      ~ if_else(. < 1, "", "ND"))) %>%
    mutate(across(
      contains("bql"), # convert all _flag values back to text (< or blank)
      ~ if_else(. < 1, "", "L"))) %>%
    mutate(across(
      starts_with(c("value", "analyte", "units", "nutrients")), 
      # convert NaN values to NA
      ~ ifelse(is.nan(.), NA, .))) 
  
  # rename functions to rename flag, units, and value columns
  flagnamer <- function(data) {str_c(str_remove(data, "analyte_flag_"), "_flag")}
  unitnamer <- function(data) {str_c(str_remove(data, "units_"), "_units")}
  qualnamer <- function(data) {str_c(str_remove(data, "nutrients_qual_"), "_qual")}
  bqlnamer <- function(data) {str_c(str_remove(data, "analyte_bql_"), "_bql")}
  valunamer <- function(data) {str_remove(data, "value_")}
  
  # apply rename functions to column names, then reorder columns
  g <- f %>%
    rename_with(flagnamer, .cols = contains("flag")) %>%
    rename_with(unitnamer, .cols = contains("units")) %>%
    rename_with(qualnamer, .cols = contains("qual")) %>% 
    rename_with(bqlnamer, .cols = contains("bql")) %>% 
    rename_with(valunamer, .cols = contains("value")) %>%
    # mutate(duplicate = case_when(
    #   duplicate == 2 ~ "duplicate",
    #   duplicate == 1 ~ "not a duplicate",
    #   TRUE ~ "neither")) %>%
    select(order(colnames(.))) %>% # alphabetize column names
    select(lake_id, site_id, sample_depth, sample_type, everything()) %>%
    ungroup() %>%
    select(-rep)
  
  return(g)
  
  
}


cin.awberc.path <- paste0(userPath, 
                          "data/chemistry/nutrients/")

chem18 <- get_awberc_data(cin.awberc.path, 
                          "2018_ESF-EFWS_NutrientData_Updated03012019_SS_CTNUpdate04012019_JB.xlsx", 
                          "2018DATA") 

chem18 %>% distinct(lake_id)


# final object, cast to wide with dups aggregated
chem18 <- dup_agg(chem18) %>%
  # combine all of the flag columns
  unite("nh4_flags", nh4_flag, nh4_bql, nh4_qual, sep = " ") %>%
  unite("no2_flags", no2_flag, no2_bql, no2_qual, sep = " ") %>%
  unite("no2_3_flags", no2_3_flag, no2_3_bql, no2_3_qual, sep = " ") %>%
  unite("op_flags", op_flag, op_bql, op_qual, sep = " ") %>%
  unite("tp_flags", tp_flag, tp_bql, tp_qual, sep = " ") %>%
  unite("tn_flags", tn_flag, tn_bql, tn_qual, sep = " ") %>%
  # if a sample and dup have both L and ND flags, retain only the L flag
  mutate(across(ends_with("flags"),
                ~ if_else(
                  str_detect(., "ND L"), "L", .))) %>%
  # replace any blank _flags with NA
  mutate(across(ends_with("flags"),
                ~ if_else(str_detect(., "\\w"), ., NA_character_) %>%
                  str_squish(.))) # remove any extra white spaces
  
# SAMPLE INVENTORY----------------------
# Are all collected samples included?

# Samples collected
chem.inventory.expected <- chem.samples.foo %>% # see chemSampleList.R, 
  filter(lab == "R10", # See readNutrientsAda.R for R10 2020 nutrient data
         sample_year == 2018,
         analyte_group == "nutrients") %>% 
  select(-sample_year, -lab, -analyte_group, -visit)


# Samples analyzed  
chem.inventory.analyzed <- chem18 %>% select(-site_id, -matches(c("flag|qual|units"))) %>%
  pivot_longer(!c(lake_id, sample_depth, sample_type), names_to = "analyte") %>%
  select(-value)

# all analyzed samples in collected list?
# yes
setdiff(chem.inventory.analyzed, chem.inventory.expected) %>% print(n=Inf)


# all collected samples in analyzed list?
# yes
setdiff(chem.inventory.expected, chem.inventory.analyzed) %>% 
  arrange(analyte, lake_id) %>% print(n=Inf)


