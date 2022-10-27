# READ ANALYTICAL DATA FROM TTEB LABORATORY

# In 2018, TTEB ran TOC and TN, but not metals, on R10 samples.  These data are in 
# BEAULIEU_01_20_2022_update.xlsx.  We will use TN from AWBERC nutrient chemistry,
# not TTEB analysis.

# In March 2021, TTEB ran metals (TOC ran by MASI contract lab) on R10 and CIN 
# SuRGE samples collected in 2020.  Data are in `SURGE_2021_01_20_2022_update.xlsx`.

# During the summer of 2021, TTEB ran metals, TOC, and DOC on SuRGE samples
# multiple locations.  Data are in `SURGE_2021_03_10_2022_update.xlsx`.

# As of 3/10/2022, `SURGE_2021_03_10_2022_update.xlsx` and `SURGE_03_10_2022_update.xlsx`
# files contains a subset of the data.  We are waiting for an update from TTEB.

# As of 3/17/2022, the tteb sharedrive (L:\Public\CESER-PUB\IPCB) contains 
# SURGE.dbf.  The records in this file are duplicates of those in SURGE 2021.dbf

# 1. READ CHEMISTRY DATA--------------
# Files contain samples from SuRGE + other studies.  Filter below.

tteb.BEAULIEU <- read_excel(paste0(userPath, 
                                   "data/chemistry/tteb/BEAULIEU_06_30_2022_update.xlsx")) 
# all records in SURGE are in SURGE2021
# tteb.SURGE <- read_excel(paste0(userPath, 
#                                 "data/chemistry/tteb/SURGE_03_09_2022_update.xlsx"))

tteb.SURGE2021 <- read_excel(paste0(userPath, 
                                "data/chemistry/tteb/SURGE_2021_06_30_2022_update.xlsx"))

# Vectors of analyte names, grouped by reporting limit

analytes_0.05 <- c("f", "cl", "no2", "br", "no3", "po4")
analytes_0.5 <- c("al", "as", "ba", "be", "ca", "cd", "cr", "cu", "fe", "k",  "li", 
               "mg", "mn", "na", "ni", "pb", "sb", "sr", "v", "zn", "so4") 
analytes_1 <-c("toc", "doc")
analytes_2 <- "sn"
analytes_4 <- "si"
analytes_20 <- c("s", "p")
analyte_names <- c(analytes_0.05, analytes_0.5, 
                   analytes_1, analytes_2, analytes_4, analytes_20)

tteb <- bind_rows(tteb.BEAULIEU, tteb.SURGE2021) %>% 
  janitor::clean_names() %>%
  rename_with(.cols = contains("_aes"), ~gsub("_aes", "", .)) %>% # remove aes from variable name
  select(-studyid, -tn, -flag) %>% # remove unneeded columns
  rename(lab_id = labid,
         toc = toc_comb) %>%
  
  # a value of 9999999999999990.000 indicates no data for that sample/analyte.
  # This often occurs if a summary file contains samples with different
  # requested analytes.  For example, the Beaulieu file contains samples that did
  # not request metals (e.g. Falls Lake (FL), 2018 SuRGE samples (LVR, PLR))
  # and samples that did (e.g. 2020 SuRGE samples).  Samples that did not request
  # metals have values of 9999999999999990.000 for all metals analytes.
  # However, a value of 9999999999999990.000 may also indicate that the analyte
  # was outside of the standard curve and was rerun, but the summary file wasn't
  # updated with re-run value.  This is the case for labid 203173.
  mutate(across(everything(), # replace lab's placeholder numbers with 'NA'
                ~ na_if(., 9999999999999990.000))) %>%
  # create 'flag' columns for every analyte to flag observations < det. limit
  mutate(across(ends_with(analyte_names), # nice code Joe!
                ~ if_else(. < 0 , "ND", ""), # bd reported as -detection limit
                .names = "{col}_flag")) %>%
  # create 'bql' columns to flag observations < reporting limit. 
  mutate(across(contains(analyte_names) & !ends_with(c("flag", "sampid")), 
                ~ case_when(
                  cur_column() %in% analytes_0.05 & . < 0.05 & . > 0 ~ "L",
                  cur_column() %in% analytes_0.5 & . < 0.5 & . > 0 ~ "L", 
                  cur_column() %in% analytes_1 & . < 1 & . > 0 ~ "L",
                  cur_column() %in% analytes_2 & . < 2 & . > 0 ~ "L", 
                  cur_column() %in% analytes_4 & . < 4 & . > 0 ~ "L", 
                  cur_column() %in% analytes_20 & . < 20 & . > 0  ~ "L", 
                  TRUE ~ ""),
                .names = "{col}_bql")) %>%
  # create 'units' columns. Most units in mg/L 
  mutate(across(ends_with(analyte_names), # 
                ~ "mg_l", 
                .names = "{col}_units")) %>%
  mutate(toc_units = "mg_c_l") %>% # TOC is the exception
  mutate(across(al:zn, # make all values positive. absolute value of - detection limit
                ~ abs(.))) %>%
  select(order(colnames(.))) %>% # alphabetize column names
  select(lab_id, sampid, everything()) # put these columns first


# 2. READ CHAIN OF CUSTODY----------------
# Read in chain on custody data for SuRGE samples submitted to TTEB
# Need to add R10 2018 samples to ttebSampleIds.xlsx
ttebCoc <- read_excel(paste0(userPath, 
                             "data/chemistry/tteb/ttebSampleIds.xlsx")) %>%
  clean_names(.) %>%
  mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)))

janitor::get_dupes(ttebCoc, lab_id) # [9/30/22 lab_id == NA for a few sites.  Leah tracking down true value.


# 3. REVIEW CHAIN OF CUSTODY-------------

# Compare list of submitted samples to comprehensive sample list
# print rows in ttebSampleIds not in chem.samples.
# [September 30, 2022] extra samples because master sample list hasn't been updated for 2022
setdiff(ttebCoc[c("lake_id", "sample_depth", "sample_type", "analyte")],
        chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals"), #tteb does organics and metals
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(sample_year == 2020 & analyte_group == "organics"))  %>% # 2020 doc/toc sent to MASI
          mutate(analyte = replace(analyte, analyte_group == "metals", "metals")) %>%
          distinct() %>%
          select(lake_id, sample_depth, sample_type, analyte)) %>%
  arrange(lake_id)

# Have all tteb samples in comprehensive sample list been submitted?
# Print rows from comprehensive sample list not in tteb coc.
# All samples accounted for
setdiff(chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals"), #tteb does organics and metals
                 #sample_year >= 2020, # no 2018 samples sent to TTEB
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(sample_year == 2020 & analyte_group == "organics"))  %>% # 2020 doc/toc sent to MASI
          mutate(analyte = replace(analyte, # convert long list of metals into "metals"
                                   analyte_group == "metals", 
                                   "metals")) %>%
          distinct() %>% # remove duplicate "metals"
          select(lake_id, sample_depth, sample_type, analyte),
        ttebCoc[c("lake_id", "sample_depth", "sample_type", "analyte")]) %>%
  arrange(lake_id)



# 4. JOIN TTEB DATA WITH CoC--------------
# inner_join will keep all matched samples.  Since
# we are matching with SuRGE CoC, only SuRGE samples will be retained.
# tteb contains data from other studies too (i.e. Falls Lake dat)
tteb.all <- inner_join(ttebCoc, tteb)
nrow(tteb.all) # 347 records [7/20/2022] 


# 5. DOC AND TOC ARE SUBMITTED TO TTEB AS TOC.   FIX HERE.
tteb.all <- tteb.all %>%
  # Add the visit field
  mutate(visit = if_else(lake_id %in% c("281", "250") & 
                           between(colldate, 
                                   as.Date("2022-08-15"), 
                                   as.Date("2022-09-15")),
                         2, 1, missing = 1)) %>% 
  select(-colldate) %>% # Colldate no longer needed
  mutate(doc = case_when(analyte == "doc" ~ toc,
                         TRUE ~ NA_real_),
         doc_units = "mg_c_l",
         doc_flag = case_when(analyte == "doc" ~ toc_flag,
                              TRUE ~ ""),
         doc_bql = case_when(analyte == "doc" ~ toc_bql,
                              TRUE ~ "")) %>%
  mutate(toc = case_when(analyte == "doc" ~ NA_real_,
                         TRUE ~ toc))

# 6. SAMPLE INVENTORY REVIEW
# Are all submitted samples in chemistry data?
# 9/30/2022 no 2022 data available yet, so all 2022 samples show up in this list
# missing 5 samples, but four of them were due to instrument failure.
ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id)) %>% arrange(lab_id)
# per Maily, 4/21/2022: During these weeks of running, the instrument was having 
# many instrument failures including mechanical arm failures and injection errors. 
# After many attempts to rerun the samples over these several days, the sample was 
# depleted and we were unable to reanalyze the samples. "203642, 203643, and 203644 
# will not have results as there were instrument failures with that run and after 
# several attempts it looks like they ran out of sample."

# After filtering those lost to instrument failure, only missing one sample 203165
# Waiting for update from Maily. [7/20/2022]
ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id)) %>%
  filter(!(lab_id %in% c(203606, 203642:203645))) %>% # instrument failure
  write.table(paste0(userPath, "data/chemistry/tteb/missingTteb07202022.txt"), row.names = FALSE)


# 7. UNIQUE IDs ARE DUPLICATED FOR EACH ANALYTE
# any combination of lake_id, site_id, sample_depth, and sample_type could
# be repeated for metals, doc, and toc.  To eliminate replicates of rows
# that share unique IDs, split by analyte, select columns that contain data
# for the analyte, then merge by unique ID.
tteb.all <- tteb.all %>%
  group_split(analyte) %>% # split by analyte
  map(., function(x) 
    if (unique(x$analyte == "doc")) { # if contains doc
      x %>% select(lake_id, site_id, sample_depth, sample_type, visit, contains("doc")) # select doc stuff
    } else if (unique(x$analyte == "toc")) { # if contains toc
      x %>% select(lake_id, site_id, sample_depth, sample_type, visit, contains("toc")) # select toc stuff
    } else if (unique(x$analyte == "metals")) { # if contains metals
      x %>% select(lake_id, site_id, sample_depth, sample_type, visit,
                   ni, ni_flag, 
                   ni_bql, ni_units, # if ni in matches, also grabs units (i.e. doc_units)
                   s, s_flag, 
                   s_bql, s_units, # if s in matches, grabs too many variable
                   matches("(al|as|ba|be|ca|cd|cr|cu|fe|k|li|mg|mn|na|p|pb|sb|si|sn|sr|v|zn)")) # select metals stuff
    }) %>%
  reduce(., full_join) # merge on lake_id, site_id, sample_depth, sample_type

dim(tteb.all) #175 rows.  Good, reduced from 347 to 175.




# 7. CLEAN UP FINAL OBJECT, STEP 1

tteb.all <- tteb.all %>% 
  select(-analyte, -sampid) %>%
  mutate(site_id = as.numeric(
    gsub(".*?([0-9]+).*", "\\1", site_id))) %>% # remove non numeric chars
  # rename the toc and doc fields to enable a clean join with other objects containing
  # TOC data (i.e. oc.ada, masi.toc). DOC data (ada.oc). See mergeChemistry.R
  # Unite all of the _flag, _qual, and _bql columns
  # I haven't figured out a way to simplify the following code...
  unite("tteb.toc_flags", toc_flag, toc_bql, sep = " ")  %>%
  unite("tteb.doc_flags", doc_flag, doc_bql, sep = " ")  %>%
  unite("al_flags", al_flag, al_bql, sep = " ")  %>%
  unite("as_flags", as_flag, as_bql, sep = " ")  %>%
  unite("ba_flags", ba_flag, ba_bql, sep = " ")  %>%
  unite("be_flags", be_flag, be_bql, sep = " ")  %>%
  unite("ca_flags", ca_flag, ca_bql, sep = " ")  %>%
  unite("cd_flags", cd_flag, cd_bql, sep = " ")  %>%
  unite("cr_flags", cr_flag, cr_bql, sep = " ")  %>%
  unite("cu_flags", cu_flag, cu_bql, sep = " ")  %>%
  unite("fe_flags", fe_flag, fe_bql, sep = " ")  %>%
  unite("k_flags", k_flag, k_bql, sep = " ")  %>%
  unite("li_flags", li_flag, li_bql, sep = " ")  %>%
  unite("mg_flags", mg_flag, mg_bql, sep = " ")  %>%
  unite("mn_flags", mn_flag, mn_bql, sep = " ")  %>%
  unite("na_flags", na_flag, na_bql, sep = " ")  %>%
  unite("ni_flags", ni_flag, ni_bql, sep = " ")  %>%
  unite("p_flags", p_flag, p_bql, sep = " ")  %>%
  unite("pb_flags", pb_flag, pb_bql, sep = " ")  %>%
  unite("s_flags", s_flag, s_bql, sep = " ")  %>%
  unite("sb_flags", sb_flag, sb_bql, sep = " ")  %>%
  unite("si_flags", si_flag, si_bql, sep = " ")  %>%
  unite("sn_flags", sn_flag, sn_bql, sep = " ")  %>%
  unite("sr_flags", sr_flag, sr_bql, sep = " ")  %>%
  unite("v_flags", v_flag, v_bql, sep = " ")  %>%
  unite("zn_flags", zn_flag, zn_bql, sep = " ")


# Tried to create a better way to unite columns, but it created more problems 
# than it solved. 
# uniter <- function(data, analyte) {
#   
#   a <- data %>% 
#     as_tibble() %>%
#     select(starts_with(analyte)) %>%
#     select(contains(c("flag", "bql"))) %>%
#     # replace_na(list("BANANAS")) %>%
#     # rowwise() %>%
#    unite(newcolumn, everything(), sep = " ") %>%
#     select("{analyte}_flags" := newcolumn) %>%
#     replace_na(list(""))
#     # 
# }
# 
# # map the uniter function to each analyte name and return a tibble
# tteb_flags <- analyte_names %>%
#   map(~ uniter(tteb.all, .x)) %>% bind_cols()

# bind these tibbles to tteb.all
# tteb.all <- bind_cols(tteb.all, tteb_flags) %>%
#   select(-ends_with(c("flag", "bql"))) %>%

temp <- tteb.all
# 8 CLEAN UP FINAL OBJECT, STEP 2
tteb.all <- tteb.all %>%
  mutate(across(ends_with("flags"),
                ~ if_else(str_detect(., "NA"), " ", .))) %>%
  mutate(across(ends_with("flags"),
                ~ if_else(str_detect(., "ND L"), "L", .))) %>%
  mutate(across(ends_with("flags"),   # replace any blank _flags with NA
                ~ if_else(str_detect(., "\\w"), ., NA_character_) %>%
                str_squish(.))) # remove any extra white spaces


janitor::get_dupes(tteb.all %>% select(lake_id, site_id, sample_type, sample_depth))


