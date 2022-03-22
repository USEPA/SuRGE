# script for reading chlorophyll, phycocyanin, and microcystin measured
# at Narragansett laboratory.

# Jake needs to locate data and provide additional background information.

# As on 1/20/2022, do data have been uploaded; however, NAR compiled a file
# listing all the samples they have received.  Lets read those in and compare
# to the samples expected.

nar.samples <- read_excel(paste0(userPath,
                                 "data/sampleTrackingSheets//NAR algal indicator//",
                                 "narSampleReceiptList.xlsx"))

# print rows in nar.samples not in chem.samples
# all good (1/21/2022)
setdiff(nar.samples[c("lake_id", "analyte", "sample_type")],
        chem.samples.foo[c("lake_id", "analyte", "sample_type")]) %>% print(n=Inf)


# Have all NAR algae samples in comprehensive sample list been delivered to NAR?
# 1/20/2022. Missing lake 204 samples.  Confirmed missing by NAR.  Checking
# with Raghu
setdiff(chem.samples.foo %>% filter(analyte_group == "algae.nar", 
                                    sample_year != 2018) %>% # 2018 R10 not sent to NAR
          select(lake_id, analyte, sample_type),
        nar.samples[c("lake_id", "analyte", "sample_type")]) %>%
  arrange(lake_id) %>% print(n=Inf)


get_chla_data <- function(path, data, sheet) { 
  
  d <- read_csv(paste0(path, data)) %>% #
    janitor::clean_names() %>% # 
    # mutate(site_id = str_extract(sample_id, # create site_id   
    #                              "U02|U22|U35|U10|U31|U05|U08|U33|U04|U10|U09|SU05|SU03|SU31|SU34"), # R10 2018 site_id values
    #        site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>% # convert to numeric
    # mutate(sample_type = str_sub(sample_id, -3)) %>% # get sample_type
    # mutate(sample_type = case_when( # convert sample_type to SuRGE format
    #   sample_type == "BLK" ~ "blank",
    #   sample_type == "DUP" ~ "duplicate",
    #   sample_type == "UKN" ~ "unknown",
    #   sample_type == "UNK" ~ "unknown",
    #   TRUE   ~ sample_type)) %>%
    # rename(chla = chl_a_jh) %>% # see Wiki for naming conventions
    # mutate(sample_depth = "shallow") %>% # all samples were collected near a-w interface
    # mutate(extract_conc = (5 * chla) / extract_volume_l) %>% # extracted concentration
    # mutate(filter_hold_time = collection_date - extraction_date) %>% # get hold time
    # mutate(extract_hold_time = analysis_date - extraction_date) %>% # get hold time
    # mutate(chla_qual = case_when( # qual flag if either hold time exceeded
    #   filter_hold_time > 60 ~ "1",
    #   extract_hold_time > 300 ~ "1",
    #   TRUE   ~ "")) %>%
    # mutate(chla_flag = case_when( # flag if conc is below detection limit 
    #   extract_conc < 9 ~ "<",
    #   TRUE   ~ "")) %>%
    # select(lake_id, site_id, sample_type, sample_depth, chla, extract_conc, 
    #        chla_flag, chla_qual)
  
  return(d)
  
}

# chlorophyll, phycocyanin, and microcystin results
cin.pig.path <- paste0(userPath,
                       "data/algalIndicators/pigments/")

chla_20_21 <- get_chla_data(cin.pig.path,
                               "surge_chla_all_2020_2021.csv")
