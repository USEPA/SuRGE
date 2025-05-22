# READ ANALYTICAL DATA FROM TTEB LABORATORY

# In 2018, TTEB ran TOC and TN, but not metals, on R10 samples.  These data are in 
# BEAULIEU_01_20_2022_update.xlsx.  We will use TN from AWBERC nutrient chemistry,
# not TTEB analysis.

# In March 2021, TTEB ran metals (TOC ran by MASI contract lab) on R10 and CIN 
# SuRGE samples collected in 2020.  Data are in `SURGE_2021_01_20_2022_update.xlsx`.

# During the summer of 2021, TTEB ran metals, TOC, and DOC on SuRGE samples
# multiple locations.  Data are in `SURGE_2021_03_10_2022_update.xlsx`.

# anions also analyzed in 2022 and 2023

# As of 3/17/2022, the tteb sharedrive (L:\Public\CESER-PUB\IPCB) contains 
# SURGE.dbf.  The records in this file are duplicates of those in SURGE 2021.dbf

# 1. READ CHEMISTRY DATA--------------
# Files contain samples from SuRGE + other studies.  Filter below.

tteb.BEAULIEU <- read_excel(paste0(
  userPath, 
  "data/chemistry/tteb/BEAULIEU_06_30_2022_update.xlsx")) 


# Aug 23-24 2022 samples excluded from earlier TTEB reports due to qa.qc issues

# Get vector of lab ids of Aug 23 & 24 2022 from sample submission forms
# (to filter sample data from excel)
tteb.23.24Aug2022.ids <- read_excel(paste0(
  userPath, 
  "data/chemistry/tteb/ttebTOC.DOC23August2022.xlsx"), 
  sheet = 1, range = "F7:F23") %>%
  bind_rows(read_excel(paste0(
    userPath, 
    "data/chemistry/tteb/ttebTOC.DOC24August2022.xlsx"), 
    sheet = 1, range = "F7:F23")) %>%
  janitor::clean_names() %>%
  janitor::remove_empty(which = c("rows", "cols")) %>%
  rename(lab_id = x1) %>%
  mutate(lab_id = as.character(lab_id)) 

# Grab data from spreadsheet of samples affected by qa.qc issues.
tteb.23.24Aug2022 <- read_excel(paste0(
  userPath, 
  "data/chemistry/tteb/TOC_L6_220913 - QC Failures - DNR.xlsx"), 
  sheet = 1, range = "A11:N72") %>%
  janitor::clean_names() %>%
  select(lab_id = sample_id_1,  
         colldate = date_time_14, toc = x11) %>% # measured as TOC, see line 271 for fix
  filter(!str_detect(lab_id, "Dup|Spk"),  # remove lab dupes/spikes for now
         str_starts(lab_id, "^[0-9]"),  
  str_sub(lab_id, 1, 6) %in% tteb.23.24Aug2022.ids$lab_id) %>%
  mutate(colldate = lubridate::date(colldate),
         lab_id = as.numeric(lab_id)) 



# SuRGE only data
tteb.SURGE2021 <- read_excel(paste0(
  userPath, 
  "data/chemistry/tteb/SURGE_2021_06_30_2022_update.xlsx"))

tteb.SURGE2022 <- read_excel(paste0(
  userPath, 
  "data/chemistry/tteb/SURGE_2022_04_10_2024_update.xlsx"))

tteb.SURGE2023 <- read_excel(paste0(
  userPath, 
  "data/chemistry/tteb/SURGE_2023_10_24_2024_update.xlsx")) %>%
  select(-NO2N) # new variable added 5/16/24. Duplicated with NO2IC, omit



# anions ic preliminary data (13 Nov 2023) [not in formal data report 3/20/25]
tteb.prelim.anions.ic <- read_excel(
  paste0(userPath, 
         "data/chemistry/tteb/tteb_prelim_anions_ic.xlsx")) %>%
  filter(str_detect(lab_id, "^[0-9]+$")) %>% # keep numeric only (no dups)
  mutate(lab_id = as.numeric(lab_id)) %>% # make lab_id numeric
  mutate(across(f:so4, # replace anything below det limit with zero for now
                ~ ifelse(str_detect(., "<"), 0, .))) %>%
  mutate(across(f:so4, 
                ~ str_extract(., "\\d+\\.?\\d*") %>%
                  as.numeric()), # extract numbers and make numeric
         # The function below replaces all zeroes with NA. Note that any
         # values below the det. limit are zero, so they become NA as well. 
         # We can change this if desired.
         across(everything(), 
                ~ if_else(. == 0, NA_real_, .))) 

# toc preliminary data (18 Oct 2023) [not in formal data report 5/15/24]
tteb.prelim.toc <- read_excel(
  paste0(userPath, 
         "data/chemistry/tteb/tteb_prelim_toc.xlsx")) %>%
  filter(str_detect(lab_id, "^[0-9]"),  # keep if lab_id starts w/ a number
           !grepl(c("spk|dup"), lab_id, ignore.case = TRUE)) %>% # omit lab dups and spikes
  mutate(lab_id = str_extract(lab_id, "[0-9]+") %>% # remove non-numeric
           as.numeric()) # make numeric

# Vectors of analyte names, grouped by reporting limit
analytes_0.005 <- "no2" 
analytes_0.006 <- "no3"
analytes_0.007 <- "f"  # this is mdl
analytes_0.008 <- "br" # this is mdl
analytes_0.011 <- "po4" 
analytes_0.03 <- "cl"  # this is mdl
analytes_0.05 <- "so4" # this is mdl
# lowest standard is 0.5 for these (e-mail from Eugenia Riddick on 9/30/2022)
analytes_0.5 <- c("al", "as", "ba", "be", "ca", "cd", "cr", "cu", "fe", "k",  "li", 
                  "mg", "mn", "na", "ni", "pb", "sb", "sr", "v", "zn") 
analytes_1 <-c("toc", "doc") # lowest standard, this is RL
analytes_2 <- "sn" # lowest standard, this is RL
analytes_4 <- "si" # lowest standard, this is RL
analytes_20 <- c("s", "p") # lowest standard, this is RL
analyte_names <- c(analytes_0.005, analytes_0.006, analytes_0.007, 
                   analytes_0.008, analytes_0.011, analytes_0.03,
                   analytes_0.05, analytes_0.5, 
                   analytes_1, analytes_2, analytes_4, analytes_20)

tteb <- bind_rows(tteb.BEAULIEU, tteb.SURGE2021, tteb.SURGE2022, 
                  tteb.SURGE2023) %>% 
  janitor::clean_names() %>%
  rename_with(~ if_else( # rename any column names containing an underscore _
    str_detect(., "_"), str_extract(., "^[^_]*"), .)) %>% # keep chars before _
  rename_with(~ if_else( # rename any column names containing a number
    str_detect(., "[0-9]"), str_sub(., 1, 3), .)) %>% # keep first 3 chars
  select(-studyid, -tn, lab_id = labid) %>% # remove unneeded columns
  rename() %>%
  # Add Aug 23 & 24 2022 data, which already has matching column names
  bind_rows(tteb.23.24Aug2022) %>%
  
  # a value of 9999999999999990.000 indicates no data for that sample/analyte.
  # This often occurs if a summary file contains samples with different
  # requested analytes.  For example, the Beaulieu file contains samples that did
  # not request metals (e.g. Falls Lake (FL), 2018 SuRGE samples (LVR, PLR))
  # and samples that did (e.g. 2020 SuRGE samples).  Samples that did not request
  # metals have values of 9999999999999990.000 for all metals analytes.
  # However, a value of 9999999999999990.000 may also indicate that the analyte
  # was outside of the standard curve and was rerun, but the summary file wasn't
  # updated with re-run value.  This is the case for labid 203173.
  mutate(across(where(is.numeric), # replace lab's placeholder numbers with 'NA'
                ~ na_if(., 9999999999999990.000))) %>%
  # create 'flag' columns for every analyte to flag observations < det. limit
  mutate(across(ends_with(analyte_names), # nice code Joe!
                ~ if_else(. < 0 , "ND", ""), # bd reported as -detection limit
                .names = "{col}_flag")) %>%
  # create 'qual' for hold time violations
  mutate(across(ends_with(analyte_names), 
                ~ if_else(str_detect(flag, "H"), "H", ""),
                .names = "{col}_qual")) %>%
  # create 'bql' columns to flag observations < reporting limit. 
  mutate(across(contains(analyte_names) & !ends_with(c("flag", "qual", "sampid")), 
                ~ case_when(
                  cur_column() %in% analytes_0.005 & . < 0.005 & . > 0  ~ "L", 
                  cur_column() %in% analytes_0.006 & . < 0.006 & . > 0  ~ "L",
                  cur_column() %in% analytes_0.007 & . < 0.007 & . > 0  ~ "L",
                  cur_column() %in% analytes_0.008 & . < 0.008 & . > 0  ~ "L",
                  cur_column() %in% analytes_0.011 & . < 0.011 & . > 0  ~ "L",
                  cur_column() %in% analytes_0.03 & . < 0.03 & . > 0  ~ "L",
                  cur_column() %in% analytes_0.05 & . < 0.05 & . > 0  ~ "L",
                  cur_column() %in% analytes_0.5 & . < 0.5 & . > 0 ~ "L", 
                  cur_column() %in% analytes_1 & . < 1 & . > 0 ~ "L",
                  cur_column() %in% analytes_2 & . < 2 & . > 0 ~ "L", 
                  cur_column() %in% analytes_4 & . < 4 & . > 0 ~ "L", 
                  cur_column() %in% analytes_20 & . < 20 & . > 0  ~ "L", 
                  TRUE ~ ""),
                .names = "{col}_bql")) %>%
  # create 'units' columns. Most units in mg/L 
  mutate(across(ends_with(analyte_names) & !contains("qual"), # 
                ~ "mg_l", 
                .names = "{col}_units")) %>%
  mutate(toc_units = "mg_c_l") %>% # TOC is the exception
  mutate(across(al:so4, # make all values positive. absolute value of - detection limit
                ~ abs(.))) %>%
  select(order(colnames(.))) %>% # alphabetize column names
  select(lab_id, sampid, colldate, flag, 
         comment, everything()) %>% # put these columns first
  # extra shallow collected.  See note in ttebSampleIds.xlsx
  # and chemistry065NARtoCIN06September2022.pdf.  Easier
  # to delete than integrate into analysis
  filter(!(lab_id == 214171),
         !(lab_id == 0)) # lab qaqc sample 

# Add the tteb prelim data, which already has matching column names
tteb <- bind_rows(tteb, tteb.prelim.toc, tteb.prelim.anions.ic)



# 2. READ CHAIN OF CUSTODY----------------
# Read in chain on custody data for SuRGE samples submitted to TTEB
ttebCoc <- read_excel(paste0(userPath, 
                             "data/chemistry/tteb/ttebSampleIds.xlsx")) %>%
  clean_names(.) %>%
  mutate(site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
         analyte = tolower(analyte)) %>%
  # this sample lost in lab.  See See 2/21/2023 email from Maily Pham
  filter(!(lake_id == "240" & sample_type == "unknown" & analyte == "doc" & sample_depth == "deep"),
         !(lab_id == 214171)) # extra shallow collected.  See note in ttebSampleIds.xlsx
# and chemistry065NARtoCIN06September2022.pdf.  Easier
# to delete than integrate into analysis

unique(ttebCoc$lake_id) # looks good

# 3. REVIEW CHAIN OF CUSTODY-------------

# Compare list of submitted samples to comprehensive sample list
# print rows of submitted samples (ttebSampleIds) that are not in theoretical
# list of all samples to be generated.
# All samples that were submitted for analysis were expected. Good. [5/15/2024]
setdiff(ttebCoc[c("lake_id", "sample_depth", "sample_type", "analyte")],
        chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals", "anions"), #tteb does organics, metals and anions in >=2022
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(lab == "ADA" & analyte_group == "anions"), # ADA does own anions
                 !(sample_year == 2020 & analyte_group == "organics"), # 2020 doc/toc sent to MASI
                 !(sample_year <= 2021 & analyte_group == "anions")) %>% # 2020 anions run by Daniels 
          mutate(analyte = case_when(analyte_group == "metals" ~ "metals",
                                     analyte_group == "anions" ~ "anions",
                                     TRUE ~ analyte)) %>%
          distinct() %>%
          select(lake_id, sample_depth, sample_type, analyte)) %>%
  arrange(lake_id)

# Have all tteb samples in comprehensive sample list been submitted?
# Print rows from comprehensive sample list not in tteb coc.
# [5/15/2024] all good!
setdiff(chem.samples.foo %>% 
          filter(analyte_group %in% c("organics", "metals", "anions"), #tteb does organics, metals and anions in >=2022
                 !(lab == "ADA" & analyte_group == "organics"), # ADA does own organics
                 !(lab == "ADA" & analyte_group == "anions"), # ADA does own anions
                 !(sample_year == 2020 & analyte_group == "organics"), # 2020 doc/toc sent to MASI
                 !(sample_year <= 2021 & analyte_group == "anions")) %>% # 2020 anions run by Daniels 
          mutate(analyte = case_when(analyte_group == "metals" ~ "metals",
                                     analyte_group == "anions" ~ "anions",
                                     TRUE ~ analyte)) %>%
          distinct() %>%
          select(lake_id, sample_depth, sample_type, analyte),
        ttebCoc[c("lake_id", "sample_depth", "sample_type", "analyte")]) %>%
  arrange(lake_id)



# 4. JOIN TTEB DATA WITH CoC--------------
# inner_join will keep all matched samples.  Since
# we are matching with SuRGE CoC, only SuRGE samples will be retained.
# tteb contains data from other studies too (i.e. Falls Lake data)
dim(ttebCoc) #854
dim(tteb) #1203 [4/16/2025]
tteb.all <- inner_join(ttebCoc, tteb)
nrow(tteb.all) # 849 records [3/20/2025] 


# 5. DOC AND TOC ARE SUBMITTED TO TTEB AS TOC.   FIX HERE.
tteb.all <- tteb.all %>%
  mutate(doc = case_when(analyte == "doc" ~ toc,
                         TRUE ~ NA_real_),
         doc_units = "mg_c_l",
         doc_flag = case_when(analyte == "doc" ~ toc_flag,
                              TRUE ~ ""),
         doc_qual = case_when(analyte == "doc" ~ toc_qual,
                              TRUE ~ ""),
         doc_bql = case_when(analyte == "doc" ~ toc_bql,
                             TRUE ~ "")) %>%
  mutate(toc = case_when(analyte == "doc" ~ NA_real_,
                         TRUE ~ toc)) %>%
  # Remove shipping_notes; they're added in mergeChemistry.R
  select(-shipping_notes) 

# 6. SAMPLE INVENTORY REVIEW
# Are all submitted samples in chemistry data?
# missing samples, but four of them were due to instrument failure.
ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id)) %>% arrange(lab_id)

# per Maily, 4/21/2022: During these weeks of running, the instrument was having 
# many instrument failures including mechanical arm failures and injection errors. 
# After many attempts to rerun the samples over these several days, the sample was 
# depleted and we were unable to reanalyze the samples. [203606]

# per Maily, 6/29/2022: "203642, 203643, and 203644 will not have results as 
# there were instrument failures with that run and after several attempts it 
# looks like they ran out of sample."

# per Maily, 2/21/2023: [203165] "Both analysts who had been running TOC at the time both 
# stated they could not find that sample in their records. As Sonia said, they 
# were able to find samples before and after that number, but that single sample 
# still appears to be missing and no record of it was on the instrument logs as 
# being run. We apologize for the inconvenience, but it is a MIA sample it appears."  

# per Sonia, 9/28/2023: The missing DOC/TOC samples were analyzed on 9/13/2022. 
# Except for 214916, Oct 5th submission, the missing IC samples were analyzed on 
# 8/31/2022. 214916 was run with other samples on 10/11/22. The files were ready 
# for the database but we must have missed them during the last data upload. Thank 
# you for bringing this oversite to our attention.

# per Sonia, 10/10/2023: As for 212510, we found your submission sheets and checked 
# the AES run records. It seems like 212510 was not analyzed. Maily, could you see 
# if you have any records indicating if the vial was spilled or broken? 
# per Sonia, 4/2/2024: Unfortunately, 212510 was not run for AES and we do not have the sample. 

ttebCoc %>% filter(!(lab_id %in% tteb.all$lab_id)) %>%
  filter(!(lab_id %in% c(212510, 203606, 203165, 203642:203644))) %>% # instrument failure
  #filter(!grepl("2023", coc)) %>% # exclude 2023 samples where we are waiting for update
  select(-contains("notes")) #%>%
  #write.table(paste0(userPath, "data/chemistry/tteb/missingTteb05152024.txt"), row.names = FALSE)


# 7. UNIQUE IDs ARE DUPLICATED FOR EACH ANALYTE
# any combination of lake_id, site_id, sample_depth, and sample_type could
# be repeated for anions, metals, doc, and toc.  To eliminate replicates of rows
# that share unique IDs, split by analyte, select columns that contain data
# for the analyte, then merge by unique ID.
tteb.all <- tteb.all %>%
  group_split(analyte) %>% # split by analyte
  map(., function(x) 
    if (unique(x$analyte == "doc")) { # if contains doc
      x %>% select(lake_id, site_id, sample_depth, sample_type, visit, contains("doc")) # select doc stuff
    } else if (unique(x$analyte == "toc")) { # if contains toc
      x %>% select(lake_id, site_id, sample_depth, sample_type, visit, contains("toc")) # select toc stuff
    } else if (unique(x$analyte == "anions")) { # if contains anions
      x %>% select(lake_id, site_id, sample_depth, sample_type, visit,
                   f, f_bql, f_flag, f_qual, f_units,
                   cl, cl_bql, cl_flag, cl_qual, cl_units,
                   br, br_bql, br_flag, br_qual, br_units,
                   so4, so4_bql, so4_flag, so4_qual, so4_units)
    } else if (unique(x$analyte == "metals")) { # if contains metals
      x %>% select(lake_id, site_id, sample_depth, sample_type, visit,
                   s, s_flag, s_qual, 
                   s_bql, s_units, # if s in matches, grabs too many variables
                   matches("^(al|as|ba|be|ca|cd|cr|cu|fe|k|li|mg|mn|na|ni|p|pb|sb|si|sn|sr|v|zn)")) # select metals stuff
    }) %>%
  reduce(., full_join) # merge on lake_id, site_id, sample_depth, sample_type, visit

dim(tteb.all) #333 rows.  Good, reduced from 847 to 335.  [5/18/2024]




# 7. CLEAN UP FINAL OBJECT, STEP 1

tteb.all <- tteb.all %>% 
  mutate(site_id = as.numeric(
    gsub(".*?([0-9]+).*", "\\1", site_id))) %>% # remove non numeric chars
  # rename the toc, doc, and anion fields to enable a clean join with other objects 
  rename(tteb.toc = toc, 
         tteb.doc = doc, 
         tteb.toc_units = toc_units, 
         tteb.doc_units = doc_units,
         tteb.f = f,
         tteb.f_units = f_units,
         tteb.cl = cl,
         tteb.cl_units = cl_units,
         tteb.br = br,
         tteb.br_units = br_units,
         tteb.so4 = so4,
         tteb.so4_units = so4_units) %>%
  unite("tteb.f_flags", f_flag, f_bql, f_qual, sep = " ") %>%
  unite("tteb.cl_flags", cl_flag, cl_bql, cl_qual, sep = " ") %>%  
  unite("tteb.br_flags", br_flag, br_bql, br_qual, sep = " ") %>%
  unite("tteb.so4_flags", so4_flag, so4_bql, so4_qual, sep = " ") %>%
  unite("tteb.toc_flags", toc_flag, toc_bql, toc_qual,sep = " ")  %>%
  unite("tteb.doc_flags", doc_flag, doc_bql, doc_qual, sep = " ")  %>%
  unite("al_flags", al_flag, al_bql, al_qual, sep = " ")  %>%
  unite("as_flags", as_flag, as_bql, as_qual,sep = " ")  %>%
  unite("ba_flags", ba_flag, ba_bql, ba_qual,sep = " ")  %>%
  unite("be_flags", be_flag, be_bql, be_qual,sep = " ")  %>%
  unite("ca_flags", ca_flag, ca_bql, ca_qual, sep = " ")  %>%
  unite("cd_flags", cd_flag, cd_bql, cd_qual, sep = " ")  %>%
  unite("cr_flags", cr_flag, cr_bql, cr_qual, sep = " ")  %>%
  unite("cu_flags", cu_flag, cu_bql, cu_qual,sep = " ")  %>%
  unite("fe_flags", fe_flag, fe_bql, fe_qual,sep = " ")  %>%
  unite("k_flags", k_flag, k_bql, k_qual, sep = " ")  %>%
  unite("li_flags", li_flag, li_bql,li_qual, sep = " ")  %>%
  unite("mg_flags", mg_flag, mg_bql, mg_qual,sep = " ")  %>%
  unite("mn_flags", mn_flag, mn_bql, mn_qual, sep = " ")  %>%
  unite("na_flags", na_flag, na_bql,  na_qual,sep = " ")  %>%
  unite("ni_flags", ni_flag, ni_bql, ni_qual,sep = " ")  %>%
  unite("p_flags", p_flag, p_bql, p_qual, sep = " ")  %>%
  unite("pb_flags", pb_flag, pb_bql, pb_qual, sep = " ")  %>%
  unite("po4_flags", po4_flag, po4_bql, po4_qual, sep = " ")  %>%
  unite("s_flags", s_flag, s_bql, s_qual, sep = " ")  %>%
  unite("sb_flags", sb_flag, sb_bql, sb_qual, sep = " ")  %>%
  unite("si_flags", si_flag, si_bql, si_qual, sep = " ")  %>%
  unite("sn_flags", sn_flag, sn_bql, sn_qual, sep = " ")  %>%
  unite("sr_flags", sr_flag, sr_bql, sr_qual, sep = " ")  %>%
  unite("v_flags", v_flag, v_bql, v_qual, sep = " ")  %>%
  unite("zn_flags", zn_flag, zn_bql, zn_qual, sep = " ")


# 8 CLEAN UP FINAL OBJECT, STEP 2
tteb.all <- tteb.all %>%
  mutate(across(ends_with("flags"),
                ~ str_remove_all(., "NA"))) %>%
  mutate(across(ends_with("flags"),
                ~ if_else(str_detect(., "ND L"), "L", .))) %>%
  mutate(across(ends_with("flags"),   # replace any blank _flags with NA
                ~ if_else(str_detect(., "\\w"), ., NA_character_) %>%
                  str_squish()))  # remove any extra white spaces 


# Final check for dupes
# if dups, check for duplicate records between final and preliminary data.
janitor::get_dupes(tteb.all, lake_id, site_id, visit, sample_depth, sample_type)


