# Script for reading anions analyzed at AWBERC by Kit Daniels for CIN, RTP, and
# USGS samples collected in 2021.

# see ...data/chemistry/anions_ada_daniels/Daniels_Anions_2021.xlsx for data
# see issue 10 for analyte naming conventions

# 1. Function to read daniels anion data---------------
path <- paste0(userPath,
               "data/chemistry/anions_ada_daniels/",
               "Daniels_Anions_2021_2_import.xls")

sheet <- "2021_SURGE_anions"

get_daniels <- function(path, sheet){
  top_table <- read_excel(path = path, sheet = sheet, na = "n.a.") %>%
    janitor::clean_names() %>%
    select(-matches(("no2|no3|po4|full|dilution"))) %>% # these analytes are taken from Lachat
    mutate(sample_depth = case_when(grepl("blank", sample_id, ignore.case = TRUE) ~ "blank",
                                    grepl("shallo", sample_id, ignore.case = TRUE) ~ "shallow",
                                    grepl("deep", sample_id, ignore.case = TRUE) ~ "deep",
                                    TRUE ~ "oops"),
           sample_type = case_when(grepl("dup", sample_id, ignore.case = TRUE) ~ "duplicate",
                                   grepl("blank", sample_id, ignore.case = TRUE) ~ "blank",
                                   TRUE ~ "unknown"),
           #https://stackoverflow.com/questions/35403491/r-regex-extracting-a-string-between-a-dash-and-a-period
           lake_id = gsub("^[^-]*-([^-]+).*", "\\1", sample_id) %>% 
             as.numeric() %>% as.character(),
           d_anion_analysis_date = as.Date(
             gsub( " .*$", "", injection_date_time), format = "%m/%d/%Y")) %>%
    mutate(lake_id = case_when(str_detect(sample_id, "LAC") ~ paste0(lake_id, "_lacustrine"),
                               str_detect(sample_id, "TRAN") ~ paste0(lake_id, "_transitional"),
                               str_detect(sample_id, "RIV") ~ paste0(lake_id, "_riverine"),
                               lake_id == "69" ~ "69_riverine",
                               TRUE ~ lake_id)) %>%
    # create the analyte_flag column
    mutate(f_flag = case_when(is.na(f_mg_l) ~ "ND", TRUE ~ NA_character_)) %>% 
    mutate(cl_flag = case_when(is.na(cl_mg_l) ~ "ND", TRUE ~ NA_character_)) %>% 
    mutate(br_flag = case_when(is.na(br_mg_l) ~ "ND", TRUE ~ NA_character_)) %>% 
    mutate(so4_flag = case_when(is.na(so4_mg_l) ~ "ND", TRUE ~ NA_character_)) %>% 
    # sub mdl for NA
    mutate(f_mg_l = case_when(is.na(f_mg_l) ~ 0.005, TRUE ~ f_mg_l),
           cl_mg_l = case_when(is.na(cl_mg_l) ~ 0.03, TRUE ~ cl_mg_l),
           br_mg_l = case_when(is.na(br_mg_l) ~ 0.02, TRUE ~ br_mg_l),
           so4_mg_l = case_when(is.na(so4_mg_l) ~ 0.025, TRUE ~ so4_mg_l)) %>%
    mutate(f_bql = case_when(f_mg_l > 0.005 & f_mg_l < 0.439 ~ "L", NA_character_) %>%
             cl_bql = case_when(cl_mg_l > 0.03 & cl_mg_l < 0.4597 ~ "L", NA_character_) %>%
             no2_bql = case_when(br_mg_l > 0.02 & br_mg_l < 0.4638 ~ "L", NA_character_) %>%
             br_bql = case_when(so4_mg_l > 0.025 & so4_mg_l < 0.423 ~ "L", NA_character_))

# 2. Extract units and analyte names
analytes.units <- top_table %>% 
  select(contains("_l")) %>% 
  colnames(.) %>%
  tibble(analyte = str_extract(., pattern = "[^_]+"),
         #https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
         unit = sub(".*?_", "", .)) %>% #? ensures first occurrence of _ is indexed
  select(-.) %>%
  #pivot_longer(analyte) %>%
  pivot_wider(names_from = analyte, values_from = unit) %>%
  rename_with(.cols = everything(), ~paste0(., "_units"))

# 3. merge data with units
top_table <- top_table %>%
  # rename columns to analyte name, remove units
  rename_with(., 
              .cols = contains("_l"), # rename columns with this pattern
              ~sub("_.*", "", .x) # extract before first occurrence of _
              ) %>% 
  cbind(., analytes.units) %>% # add units to data.
  mutate(site_id = as.numeric(site_id)) %>% # make site id numeric
  select(-sample_id, -injection_date_time) %>%
  as_tibble()

top_table
}

# 2. Use function to read data-------------
d.anions <- get_daniels(path, sheet)

# 3. Merge with sample collection date and calculate qual_flag-------------------

# Get sample collection date from fld_sheet.  This date not entered anywhere, but
# it was either day 1 or 2 at lake.  Conservatively assume day 1.
sample_date <- fld_sheet %>%
  select(lake_id, trap_deply_date) %>% # day 1
  filter(!is.na(trap_deply_date)) %>%
  group_by(lake_id) %>%
  summarize(sample_col_date = min(trap_deply_date))

# join date with d.anions
dim(d.anions) # 152 rows
d.anions <- left_join(d.anions, sample_date) 
dim(d.anions) # 152 rows


# calculate holding time violation (-qual)
d.anions <- d.anions %>%
  add_column(f_qual = NA,
         cl_qual = NA,
         br_qual = NA,
         so4_qual = NA) %>%
  mutate(across(contains("qual"), 
                ~ if_else((d_anion_analysis_date - sample_col_date) > 28, "H", ""))) %>%  # TRUE = hold time violation)
  select(-contains("date"))
    

# 4. Merge lake_id, site_id, sample_depth, sample_type  
# see issue #32 for details

dup_agg <- function(data) {
  
  d <- data %>% # pivot longer and split all dupes into separate groups
    pivot_longer(cols = c(f, cl, br, so4), names_to = "analyte") %>%
    group_split(lake_id, site_id, sample_depth, sample_type, analyte) 
    
  
  # sub-function to implement decision tree & aggregate dupes
  selector <- function(data) {

    anion = as.character(data[1,"analyte"]) # get the analyte name

    v <- data %>% select(starts_with(anion), analyte, site_id, sample_depth,
           sample_type, lake_id, value) %>% # only keep fields for that analyte
      as.tibble() %>%
      rename(qual = ends_with("qual"), # strip the analyte name from fields
             units = ends_with("units"),
             bql = ends_with("bql"),
             flag = ends_with("flag")) %>%
      arrange(qual, bql, flag, value) # ordering the values for the conditional statements below

      # FILTERING:
      # If all rows are completely identical, use slice_head to keep one of them
      # if the first row of x is qual = FALSE, we only want qual = FALSE rows
      # after filtering, we either have all qual = TRUE or all qual = FALSE
      # if the last row of x is flag = NA, we only want flag = NA rows
      # after filtering, we either have all flag < or all flag NA
      # now we can average whatever is left (NAs are irrelevant now)
    
    w <- v %>% # if all rows are identical in all values, just keep the first row
      {if (n_distinct(v) == 1) slice_head(v) else v} 

    x <- w %>% # if the first row qual = FALSE, then only keep FALSE rows
      {if (slice_head(v)$qual == FALSE) filter(w, qual == FALSE) else w} %>% 
      arrange(qual, flag, value)
      
  # 9/16/22 NEED ANOTHER STEP HERE TO PROCESS THE L FLAGS, BUT I CAN'T REMEMBER 
    # EXACTLY HOW THIS FILTERING PROCESS WORKS. NEED TO EXAMINE MORE CLOSELY.
    # I ALSO SHOULD CONSIDER REPLACING THIS W/ A PURRR MAP SINCE IT'S VERY SLOW.
    y <- x %>% # check  slice_tail(x)$flag. if is.na(flag) == TRUE, keep only flag = NA rows
      {if (is.na(slice_tail(x)$flag) == TRUE) filter(x, is.na(flag) == TRUE) else x}
    
    z <- y %>% # get the mean of any remaining groups with multiple rows
      dplyr::group_by(across(!value)) %>% summarize(value = mean(value, na.rm = TRUE))
      

    return(z)
      
  }
  
  e <- map(d, ~ selector(.)) # map the function across all of the grouped dupes 
    
  f <- bind_rows(e)
  
  g <- f %>%
    pivot_wider(names_from = analyte, # pivot to wide
                values_from = c(flag, value, units, qual),
                names_glue = "{analyte}_{.value}") %>%
    rename_with(~str_remove(., "_value"), .cols = contains("value")) 
    
    
  return(g)

}

options(dplyr.summarise.inform = FALSE) # summarize() causes lots of console spam

d.anions.aggregated <- dup_agg(d.anions) 

d.anions.aggregated %>%
  

# Sample Inventory Audit.-#-#-##-#-##-#-##-#-##-#-##-#-#

# 5. Compare samples analyzed to those in comprehensive sample list.
# [3/14/2022] all samples in data are also in comprehensive list. Good

# print rows in d.anions not in chem.samples.
setdiff(d.anions.aggregated[c("lake_id", "sample_depth", "sample_type")],
        chem.samples.foo %>% 
          filter(analyte_group == "anions",
                 sample_year >= 2020,
                 lab != "ADA") %>%
          select(lake_id, sample_depth, sample_type)) 

# 6. # Have all Daniels anion samples in comprehensive sample list been analyzed?
# Print rows from comprehensive sample list not in Daniels anion data.
# Missing blank and dup from 68 and 75.  Pegasus has been unable to locate
# these last four sample.
setdiff(chem.samples.foo %>% filter(analyte_group == "anions",
                                    sample_year == 2021, # only 2021 samples to daniels
                                    lab != "ADA") %>% # ADA ran their own anions
          select(lake_id, sample_depth, sample_type),
        d.anions[,c("lake_id", "sample_depth", "sample_type")]) %>%
  arrange(lake_id) 

