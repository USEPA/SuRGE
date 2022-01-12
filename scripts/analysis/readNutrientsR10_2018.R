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
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, skip = 1) %>%
    # d <- read_excel(paste0(cin.awberc.path,
    #                        "2021_ESF-EFWS_NutrientData_Updated11212021_AKB.xlsx"), #
    #                 sheet = "2021 Data", skip = 1) %>%
    janitor::clean_names() %>% # clean up names for rename and select, below
    janitor::remove_empty(which = c("rows", "cols")) %>% # remove empty rows
    rename(rdate = collection_date_cdate, #rename fields
           ddate = analyte_detection_date_ddate,
           finalConc = peak_concentration_corrected_for_dilution_factor,
           analyte = analyte_name_analy,
           tp_tn = tp_adjusted_concentration_full_series_ug_p_l, # this is only TP?
           lake_id = site_id_id,
           crossid = c_ross_id_pos, 
           site_id = long_id_subid,
           sample_type = type, 
           rep = rep_number) %>%
    mutate(nutrients_qual = if_else( # determine if holding time exceeded
      (as.Date(ddate) - as.Date(rdate)) > 28, TRUE, FALSE)) %>% # TRUE = hold time violation
    select(lake_id, site_id, crossid, sample_type, analyte, finalConc, nutrients_qual, rep)

    # mutate(finalConc = ifelse( # correct TP and TN are in tp_tn
    #   analyte %in% c("TP", "TN"),
    #   tp_tn,
    #   finalConc)) %>%  
    # filter(sample_type != "SPK") %>% # exclude matrix spike
    # select(lake_id, site_id, crossid, sample_type, analyte, finalConc, rep) %>% # keep only needed fields
    # mutate(analyte = str_to_lower(analyte)) %>% # make analyte names lowercase
    # mutate(analyte = case_when( # change analyte names where necessary
    #   analyte == "trp" ~ "op",
    #   analyte == "tnh4" ~ "nh4",
    #   analyte == "tno2" ~ "no2",
    #   analyte == "tno2-3" ~ "no2_3",
    #   TRUE   ~ analyte)) %>%
    # # strip character values from site_id, convert to numeric
    # mutate(site_id =  as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
    # mutate(sample_type = case_when( # recode sample type identifiers
    #   grepl("dup", sample_type, ignore.case = TRUE) ~ "duplicate",
    #   grepl("ukn", sample_type, ignore.case = TRUE) ~ "unknown",
    #   grepl("blk", sample_type, ignore.case = TRUE) ~ "blank",
    #   TRUE ~ sample_type)) %>%
    # # sample filtered or unfiltered
    # mutate(filter = str_sub(crossid, 1, 1) %>% tolower(.),
    #        filter = case_when(
    #          filter == "d" ~ "filtered",
    #          filter == "t" ~ "unfiltered",
    #          TRUE ~ filter)) %>%
    # # define sample depth
    # mutate(sample_depth = str_sub(crossid, 3, nchar(crossid)),
    #        sample_depth = case_when(
    #          grepl("d", sample_depth, ignore.case = TRUE) ~ "deep",
    #          grepl("s", sample_depth, ignore.case = TRUE) ~ "shallow",
    #          TRUE ~ sample_depth)) %>%
    # # filtered sample has really high NH4 (59.3), but unfiltered from same depth
    # # has much lower (3.94).  Replacing suspicious value with lower value
    # mutate(finalConc = replace(finalConc,
    #                            lake_id == "070 River" & sample_depth == "shallow" & 
    #                              analyte == "nh4" & filter == "filtered", 
    #                            3.94)) %>%
    # mutate() %>%
    # # strip out unneeded analyses
    # # exclude filtered samples run for totals
    # filter(!(analyte %in% c("tp", "tn") & filter == "filtered")) %>%
    # # exclude unfiltered samples run for inorganics
    # filter(!(analyte %in% c("nh4", "op", "no2_3", "no2") & filter == "unfiltered")) %>%
    # filter(!(analyte == "turea")) %>% # exclude urea
    # #filter(grepl(pattern = c("CH4|069|070"), lake_id)) %>% # Filter SuRGE data
    # filter(lake_id %in% coc.vector) %>% # Filter SuRGE data, see above
    # mutate(lake_id = str_remove(lake_id, "CH4-")) %>% # standardize lake IDs
    # mutate(lake_id = case_when( # # standardize lake ID sub-component names
    #   str_detect(lake_id, "LAC") ~ str_replace(lake_id, " LAC", "_lacustrine"),
    #   str_detect(lake_id, "River") ~ str_replace(lake_id, " River", "_riverine"),
    #   str_detect(lake_id, "Trans") ~ str_replace(lake_id, " Trans", "_transitional"),
    #   lake_id == "70" ~ "70_transitional",
    #   lake_id == "69" ~ "69_riverine",
    #   TRUE ~ lake_id)) %>%
    # mutate(finalConc = as.numeric(finalConc)) %>% # make analyte values numeric
    # mutate(analyte_flag = case_when( # create the analyte_flag column
    #   analyte == "nh4" & finalConc < 7 ~ "<", 
    #   analyte == "no2" & finalConc < 8 ~ "<",
    #   analyte == "no2_3" & finalConc < 8 ~ "<",
    #   analyte == "op" & finalConc < 0.7 ~ "<",
    #   analyte == "tn" & finalConc < 8 ~ "<",
    #   analyte == "tp" & finalConc < 8 ~ "<",
    #   TRUE ~ "")) %>%
    # mutate(finalConc = case_when( # create the analyte_flag column
    #   analyte == "nh4" & finalConc < 7 ~ 7, 
    #   analyte == "no2" & finalConc < 8 ~ 8,
    #   analyte == "no2_3" & finalConc < 8 ~ 8,
    #   analyte == "op" & finalConc < 0.7 ~ 0.7,
    #   analyte == "tn" & finalConc < 8 ~ 8,
    #   analyte == "tp" & finalConc < 8 ~ 8,
    #   TRUE ~ finalConc)) %>%
    # mutate(units = case_when(
    #   analyte == "nh4"  ~ "ug_n_l",
    #   analyte == "no2"  ~ "ug_n_l",
    #   analyte == "no2_3"  ~ "ug_n_l",
    #   analyte == "op"  ~ "ug_p_l",
    #   analyte == "tn"  ~ "ug_n_l",
    #   analyte == "tp"  ~ "ug_p_l",
    #   TRUE ~ "")) %>%
    # mutate(sample_type = case_when(
    #   sample_type == "duplicate" ~ "unknown",
    #   TRUE ~ sample_type)) %>%
    # select(-crossid) # no longer need crossid
    # 
  return(d)
  
  
}


# function aggregates dups, renames/sorts columns, and casts to wide
dup_agg <- function(data) { 
  
  # aggregate dups and convert analyte_flags to numeric (for summarize operations)
  e <- data %>%
    mutate(analyte_flag = if_else(str_detect(analyte_flag, "<"), 1, 0)) %>% # convert to numeric
    group_by(lake_id, site_id, analyte, sample_depth, 
             sample_type, rep, nutrients_qual, units) %>%
    summarize(value = mean(finalConc, na.rm = TRUE), # group and calculate means
              analyte_flag = mean(analyte_flag, na.rm = TRUE)) %>%
    mutate(sample_type = case_when(
      rep %in% c("B", 2) ~ "duplicate", TRUE ~ sample_type)) %>%
    select(-rep)
  
  # cast to wide, convert analyte flags to text, and convert any NaN to NA
  f <- e %>%
    pivot_wider(names_from = analyte, values_from = c(value, analyte_flag, units, nutrients_qual)) %>% # cast to wide
    mutate(across(contains("flag"), # convert all _flag values back to text (< or blank)
                  ~ if_else(.<1, "", "<"))) %>%
    mutate(across(starts_with(c("value", "analyte", "units", "nutrients")), # convert NaN values to NA
                  ~ ifelse(is.nan(.), NA, .))) 
  
  # rename functions to rename flag, units, and value columns
  flagnamer <- function(data) {str_c(str_remove(data, "analyte_flag_"), "_flag")}
  unitnamer <- function(data) {str_c(str_remove(data, "units_"), "_units")}
  qualnamer <- function(data) {str_c(str_remove(data, "nutrients_qual_"), "_qual")}
  valunamer <- function(data) {str_remove(data, "value_")}
  
  # apply rename functions to column names, then reorder columns
  g <- f %>%
    rename_with(flagnamer, .cols = contains("flag")) %>%
    rename_with(unitnamer, .cols = contains("units")) %>%
    rename_with(qualnamer, .cols = contains("qual")) %>% 
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
                          "2018_ESF-EFWS_NutrientData_Updated03012019_SS_CTNUpdate04012019.xlsx", 
                          "2018DATA") 

chem18 %>% distinct(lake_id)


chem18 <- dup_agg(chem18) # final object, cast to wide with dups aggregated

