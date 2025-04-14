# Assumes raw GC data have been processed in RStudio project gas_lab 
# (https://github.com/USEPA/gas_lab) (https://github.com/jbeaulie/gas_lab)
# and consolidated text file posted to github

# READ GC DATA----------------
# Read individual files
paths <-  paste0(userPath, "data/gases")
get_gc <- function(paths){
  #d <-  #assign to object for code development
  fs::dir_ls(path = paths, # see above
             regexp = 'gcMasterFile', # file names containing this pattern
             recurse = TRUE, # look in all subdirectories
             type = "file") %>% # only retain file names, not directory names
    .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
    #.[5] %>% # subset one list element for testing
    # imap will read each file in fs_path list generated above
    # the "i" in imap allows the file name (.y) to be used in function.
    purrr::imap(~read_csv(.x) %>% 
                  # add file name, but omit everything before final "/"
                  # https://stackoverflow.com/questions/65312331/extract-all-text-after-last-occurrence-of-a-special-character
                  mutate(file = sub(".*\\/", "", .y),
                         sample = gsub("R10_", "R10", sample)) %>% # remove _
                  filter(!(grepl(c("ACT|FL"), sample)))) %>% # omit acton and falls lake
    map_dfr(., identity) # rbinds into one df
}

# apply function
gc <- get_gc(paths = paths) # warnings are ok
dim(gc) # 2570 records [4/2/2025]

# omit analyzed samples not needed here for various reasons
gc <- gc %>%
  filter(!(sample %in% 
             # samples to strip:
             c("SG210634", "SG210633", "SG210632", # extra air samples collected from 275. strip from GC data
               "SG211409", # lake 16, 2 injections of 20 mL, sites 11 and 21 combined.
               "SG221172", "SG221216",  # turned in by CIN on 8/26/2022 as a TRAP sample but not recorded on any data sheets and looks like air on GC
               # PR samples delivered on 10/11/2023
               paste0("SG23", c("0672", "0673", "0674", "0670", "0671", "0684",
                                "0683", "0675", "0676", "0677", "0678", "0679",
                                "0680", "0681", "0685", "0689", "0690", "0691",
                                "0692", "0693", "0686", "0687", "0688")),
               # paper records indicate SG210652 was collected as part of triplicate from 
               # lake_id == 16 and site_id == 15 but sample rack and gc data contain
               # SG2100526. I'm guessing 0652 was incorrectly recorded in field and again
               # on sample tracking sheet, but not sure. This is one of a triplicate
               # and will just omit from dataset.
               "SG210526",
               # SG230238 was analyzed and looks like dissolved gas sample. Would have been in
               # lake 195, but not recorded. probably just delete
               "SG230238"))
             )

dim(gc) # 2570 - 2539 = 31, good

# Check for duplicates.  Should be none.
gc %>% janitor::get_dupes(sample) %>% 
  select(sample, dupe_count, ch4_ppm, n2_percent) %>% 
  print(n=Inf)

# Any negative values
# only 13. These have all been checked and area counts of unknown are 
# below that of Helium blank. Set to 0.
gc %>%
  pivot_longer(-c(sample, file)) %>%
  filter(value < 0) 

# lets replace any negative values with 0
gc <- gc %>%
  mutate(across(contains("ppm"),  ~ case_when(.x < 0 ~ 0,
                                             TRUE ~ .x)),
         across(contains("percent"),  ~ case_when(.x < 0 ~ 0,
                                                     TRUE ~ .x)))



# INSPECT EXETAINER CODES FROM FIELD SHEETS----------------------
# see readFieldSheets.R
dim(all_exet) # 2713

# List of exetainer codes !=8 characters long.
# region 8 stuff plus SG2201, a short tube sample from NAR, so good
all_exet %>%
  filter(!is.na(sample), nchar(sample) != 8) %>%
  print(n=Inf)

# Any duplicates [130]
all_exet %>% janitor::get_dupes(sample) %>% print(n=Inf) 
# "SG220153", "SG230153" are duplicated codes from ADA (18, 136, 166, 186)
# Pegasus inadvertently sent ADA two ...0153 exetainers in 2022 and 2023.
# These are air and DG samples. Discard these, they have dups. Discarded from
# GC data in gas_lab RStudio project (https://github.com/jbeaulie/gas_lab)
# (https://github.com/USEPA/gas_lab)
all_exet <- all_exet %>% filter(!(sample %in% c("SG220153", "SG230153")))

# It appears that the Region 10 and Cincinnati field crews used exetainers 
# with identical sample codes in 2020. Investigate these:
# lakes with duplicated exetainers (needed for list below)
duplicated_exetainer_lakes <- all_exet %>%
  janitor::get_dupes(sample) %>% 
  distinct(lake_id) %>%
  pull() 

# All exetainers collected from lakes with duplicated exetainers
# compare list against Excel files. Attempt to determine which
# samples were run
full_join(
  # missing exetainers
  all_exet %>% 
    filter(!(sample %in% c("SG220153", "SG230153"))) %>% # exclude duplicates sent to ADA
    janitor::get_dupes(sample) %>%
    distinct(sample) %>% # 52 duplicated codes
    mutate(duplicate = TRUE),
  # all exetainer codes from lakes with missing samples
  all_exet %>% 
    filter(lake_id %in% duplicated_exetainer_lakes)) %>%
  left_join(., 
            lake.list.all %>% 
              filter(lake_id %in% duplicated_exetainer_lakes) %>%
              select(lake_id, lab)
  ) %>% 
  mutate(across(!duplicate, as.character)) %>% # to facilitate pivot longer
  pivot_longer(!c(sample, duplicate, lab)) %>%
  pivot_wider(names_from = c(lab, name), values_from = value) %>% 
  print(n=Inf)

# In many cases, but not all, I was able to resolve duplicates by carefully
# reviewing GC data, sample IDs, assumed sample types, and patterns of reps
# strip out samples that weren't run or duplicates couldn't be resolved.
# see "SuRGE\SuRGE_Sharepoint\data\gases\duplicated_exetainers.xlsx"
dim(all_exet) # 2709
all_exet <- all_exet %>%
  filter(
    # duplicated sample IDs in Air_2021_01_19_FID_ECD_STD_UNK.xlsx
    !(sample %in% c("SG200185", "SG200186", "SG200188") & lake_id == "238"), # CIN-235, not R10-238, 3 samples
    !(sample == "SG200023" & lake_id == "249"), # CIN-234 not R10-249, 1 sample
    !(sample %in% c("SG200213", "SG200211", "SG200212") & lake_id == "265"), # CIN-235 not R10-265, 3 samples
    !(sample %in% c("SG200202", "SG200206", "SG200198") & lake_id == "287"), # CIN-235 not R10 287, 3 samples
    
    # duplicated sample IDs in DG_2021_01_11_FID_ECD_STD_UNK.xlsx
    !(sample %in% c("SG200181", "SG200182", "SG200183")), # could not determine which lab these are from, 6 samples
    !(sample %in% c("SG200187", "SG200189", "SG200192") & lake_id == "235"), # R10 238 not CIN-235, 3 samples
    !(sample %in% c("SG200013", "SG200014", "SG200015", "SG200024", "SG200025")), # could not determine which lab these are from, 10 samples
    !(sample %in% c("SG200203", "SG200208", "SG200210") & lake_id == "265"), # CIN-235 not R10-265, 3 samples
    !(sample %in% c("SG200214", "SG200215", "SG200216")), # could not determine which lab these are from, 6 samples
    !(sample %in% c("SG200191", "SG200193", "SG200196", "SG200197", "SG200200", "SG200201") & lake_id == "287"), # CIN-235 not R10 287, 6 samples
    
    # duplicated sample IDs in T_2021_02_03_FID_ECD_STD_UNK.xlsx
    !(sample %in% c("SG200019", "SG200020")), # could not determine which lab these are from, 4 samples
    !(sample %in% c("SG200016", "SG200017", "SG200018", "SG200027", "SG200028", "SG200029",
                    "SG200010", "SG200011", "SG200012") & lake_id == "234"), #R10 249 not CIN-234, 9 samples
    !(sample %in% c("SG200001", "SG200002", "SG200003", "SG200004", "SG200005",
                    "SG200006", "SG200007", "SG200008", "SG200009") & lake_id == "205"), #R10 265 not CIN-205, 9 samples
    !(sample %in% c("SG200031", "SG200032", "SG200194", "SG200195", "SG200199",
                    "SG200204", "SG200205", "SG200207", "SG200209") & lake_id == "235"), #R10 249/287 not CIN-235, 9 samples
    
    # duplicated sample IDs in T_2021_04_22_FID_ECD_TCD_STDshorttubes_.xlsx
    !(sample == "SG200257")) # can't tell which lab, 2 samples  


dim(all_exet) # 2709 - 2632 = 77, perfect

# omit exetainers that were delivered to my office but never analyzed.
# labels fell off or technicians couldn't determine source of the samples
all_exet <- all_exet %>%
  filter(!(sample %in% 
           c(
             # Josh Fisher 9/21
             # Exetainers 0414 and 0030 could not be identified on any datasheets.
             # 0403 appers to on data from ch4-205 but has the exetainer code
             # scratched out without comment.
             "SG200030", "SG200403", "SG200414",
             # Josh Fisher 9/18/2020
             # These 6 exetainers had their labels fall off and cannot
             # be individually identified.
             "SG200727", "SG200736", "SG200739",
             "SG200741", "SG200742", "SG200750"))
  )

dim(all_exet) # 2625

# MERGE EXETAINER CODES AND GC DATA-----------------
# any unmatched records
dim(gc) # 2539 [4/7/25]
dim(all_exet) # 2625

# any analyzed samples missing corresponding code from field sheet?
# yes, but these correspond to 2020 duplicated codes that were stripped
# from all_exet above. use an inner-join to exclude these from final data 
gc %>% filter(!(sample %in% all_exet$sample)) %>% 
  filter(!(sample %in% 
             # list of excluded exetainers based on duplicated exetainer
             # codes, see above
             c("SG200185", "SG200186", "SG200188", "SG200023",
               "SG200213", "SG200211", "SG200212", "SG200202", 
               "SG200206", "SG200198", "SG200181", "SG200182", 
               "SG200183", "SG200187", "SG200189", "SG200192",
               "SG200013", "SG200014", "SG200015", "SG200024", 
               "SG200025", "SG200203", "SG200208", "SG200210",
               "SG200214", "SG200215", "SG200216", "SG200191", 
               "SG200193", "SG200196", "SG200197", "SG200200", 
               "SG200201", "SG200019", "SG200020", "SG200016", 
               "SG200017", "SG200018", "SG200027", "SG200028", 
               "SG200029", "SG200010", "SG200011", "SG200012",
               "SG200001", "SG200002", "SG200003", "SG200004", 
               "SG200005", "SG200006", "SG200007", "SG200008", 
               "SG200009", "SG200031", "SG200032", "SG200194",
               "SG200195", "SG200199", "SG200204", "SG200205", 
               "SG200207", "SG200209", "SG200257")))




# any exetainer codes harvested from field sheets that are not included
# in list of samples analyzed on GC?
# 100  These have all been inspected and cannot be recovered. See
# "SuRGE\SuRGE_Sharepoint\data\gases\review_of_missing_gas_samples.xlsx"
all_exet %>% 
  filter(!(sample %in% gc$sample)) %>%
  mutate(sample_year = paste0("20", substr(sample, 3,4))) 


gc_lakeid <- inner_join(gc, all_exet)
dim(gc_lakeid) # 2525

# omit rows that don't have gas data. [0 cases of this]
# Note that some trap samples were run on Shimadzu and therefore don't have n2,
# ar, or O2 percent.  Must filter on ppm and percent variables.
# Had some trouble with this operation.  Ended up using tidyverse 1.3.1 approach
# https://stackoverflow.com/questions/41609912/remove-rows-where-all-variables-are-na-using-dplyr
gc_lakeid <- gc_lakeid %>% 
  rowwise() %>%
  filter(!all(is.na(c_across(matches(c("ppm|percent")))))) # if all ppm columns are NA, then omit row

dim(gc_lakeid) # 2525, no empty rows




# QA/QC GC REPS--------------
##########################################################
############################# FIX FOR DIFFERENT SAMPLE TYPES!!!!!!!!!!!!!!!
#pdf("output/figures/scatterplot3dTrap.pdf",
 #   paper = "a4r", width = 11, height = 8)  # initiate landscape pdf file)
par(mfrow = c(1,2))

uniqueCases <- filter(gc_lakeid, 
                       !is.na(ch4_ppm), # has GC data
                       !is.na(lake_id)) %>% # is connected with Lake and station
  distinct(lake_id, site_id) # unique combinations of lake and site

for(i in 1:nrow(uniqueCases)) {
  site.i <- uniqueCases$site_id[i]
  lake.i <- uniqueCases$lake_id[i]
  data.i <- filter(gc_lakeid,
                   site_id == site.i, lake_id == lake.i,
                   !is.na(ch4_ppm))
  
  # CO2, CH4, N2 scatterplot
  try(
    with(data.i, {
      
      s3d <- scatterplot3d(co2_ppm/10000, ch4_ppm/10000, n2_percent, 
                           xlab = "CO2 (%)", ylab = "CH4 (%)", zlab = "N2 (%)",
                           pch=21, bg = "red", main = paste(lake.i, site.i, sep = "\n"))
      
      s3d.coords <- s3d$xyz.convert(co2_ppm/10000, ch4_ppm/10000, n2_percent)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)
  
  # n2o, o2, ar scatterplot
  try(
    with(data.i, {

      s3d <- scatterplot3d(n2o_ppm, o2_percent, ar_percent,
                           xlab = "N2O (ppm)", ylab = "O2 (%)", zlab = "ar (%)",
                           pch=21, bg = "red", main = uniqueCases[i, ])

      s3d.coords <- s3d$xyz.convert(n2o_ppm, o2_percent, ar_percent)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)
  
}
dev.off()

# Basic plots
ggplot(gc_lakeid, aes(paste(lake_id, site_id), (co2_ppm/10000))) + geom_point()
gc_lakeid %>%
  select(-contains("trap")) %>%
  pivot_longer(!c(sample, lake_id, site_id, visit, file, type)) %>%
  filter(value < 0) %>%
  arrange(sample) %>%
  print(n=Inf)


ggplot(gc_lakeid, aes(paste(lake_id, site_id), (ch4_ppm/10000))) + geom_point()
ggplot(gc_lakeid, aes(paste(lake_id, site_id), (n2o_ppm/10000))) + geom_point()
ggplot(gc_lakeid, aes(paste(lake_id, site_id), n2_percent)) + geom_point()
ggplot(gc_lakeid, aes(paste(lake_id, site_id), o2_percent)) + geom_point()
# A few Ar + O2 peaks were not separated.  I asked Kit to fix
ggplot(gc_lakeid, aes(paste(lake_id, site_id), ar_percent)) + geom_point()


# Aggregate by lake_id, site_id, and visit----------------
gc_lakeid_agg <- gc_lakeid %>%
  group_by(lake_id, site_id, visit, sample_depth_m, type) %>%
  summarise(n2o_sd=sd(n2o_ppm, na.rm=TRUE),
            m_n2o_ppm=mean(n2o_ppm, na.rm=TRUE),
            n2o_cv= (n2o_sd/m_n2o_ppm) * 100,
            
            co2_sd=sd(co2_ppm, na.rm=TRUE),
            m_co2_ppm=mean(co2_ppm, na.rm=TRUE),
            co2_cv=(co2_sd/m_co2_ppm) * 100,
            
            ch4_sd=sd(ch4_ppm, na.rm=TRUE),
            m_ch4_ppm=mean(ch4_ppm, na.rm=TRUE),
            ch4_cv=(ch4_sd/m_ch4_ppm) * 100,                     
            
            o2_sd=sd(o2_percent, na.rm=TRUE),
            m_o2=mean(o2_percent, na.rm=TRUE),
            o2_cv=(o2_sd/m_o2) * 100,
            
            ar_sd=sd(ar_percent, na.rm=TRUE),
            m_ar=mean(ar_percent, na.rm=TRUE),
            ar_cv=(ar_sd/m_ar) * 100,
            
            n2_sd=sd(n2_percent, na.rm=TRUE),
            m_n2=mean(n2_percent, na.rm=TRUE),
            n2_cv=(n2_sd/m_n2) * 100) %>%
  rename(n2o_ppm = m_n2o_ppm, co2_ppm = m_co2_ppm, ch4_ppm = m_ch4_ppm,
         o2_percent = m_o2, ar_percent = m_ar, n2_percent = m_n2) %>%
  mutate(total = (ch4_ppm/10000) + (co2_ppm/10000) + (n2o_ppm/10000) + n2_percent + o2_percent + ar_percent) %>%
  ungroup() # This removes grouping, which complicates things down the line.



ggplot(gc_lakeid_agg, aes(site_id, ch4_ppm)) + # Everything appears to have agg correctly
  geom_point() +
  facet_wrap(~type, scales = "free")


# # MERGE RAW GC DATA WITH eqAreaData---------------
# # Merge all gas samples.  Will calculate dissolved concentrations downstream.
# # 1) Need to melt, which requires a data.frame, not a dplyr tbl_df.
# # 2) melt creates a 'variable' column, already have 'variable' column
# # in xtrCodes.gas.agg. Must rename first.
# xtrCodes.gas.agg <- rename(xtrCodes.gas.agg, type = variable) # rename 'variable'
# 
# xtrCodes.gas.agg.m <- melt(as.data.frame(xtrCodes.gas.agg), # convert tbl_df to df
# id.vars = c("Lake_Name", "siteID", "type")) # specify id variable
# 
# xtrCodes.gas.agg.m <- mutate(xtrCodes.gas.agg.m, type =  # adopt more intuitive names
#                              ifelse(type == "tp.xtr", "trap",
#                                     ifelse(type == "ar.xtr", "air", 
#                                            ifelse(type == "dg.xtr", "dissolved",
#                                                   type))))
#   
# xtrCodes.gas.agg.c <- dcast(xtrCodes.gas.agg.m,  # cast
#                             Lake_Name + siteID ~ type + variable) %>%
#   select(-air_o2.sd, -air_o2, -air_o2.cv, -air_ar.sd, -air_ar, -air_ar.cv, -air_n2.sd,
#          -air_n2, -air_n2.cv, -air_total,
#          -dissolved_o2.sd, -dissolved_o2, -dissolved_o2.cv, -dissolved_ar.sd, -dissolved_ar, 
#          -dissolved_ar.cv, -dissolved_n2.sd, -dissolved_n2, -dissolved_n2.cv, -dissolved_total)
# 
# # Merge
# eqAreaData <- merge(xtrCodes.gas.agg.c, eqAreaData, all = TRUE)


