

# READ GC DATA----------------
# Read individual files
paths <-  paste0(userPath, "data/gases")
get_gc <- function(paths){
  #d <-  #assign to object for code development
  fs::dir_ls(path = paths, # see above
             #regexp = 'surgeData', # file names containing this pattern
             recurse = TRUE, # look in all subdirectories
             type = "file") %>% # only retain file names, not directory names
    .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
    #.[5] %>% # subset one list element for testing
    # imap will read each file in fs_path list generated above
    # the "i" in imap allows the file name (.y) to be used in function.
    purrr::imap(~read_excel(.x) %>% 
                  # add file name, but omit everything before final "/"
                  # https://stackoverflow.com/questions/65312331/extract-all-text-after-last-occurrence-of-a-special-character
                  mutate(file = sub(".*\\/", "", .y))) %>% 
    # remove empty dataframes.  Pegasus put empty Excel files in each lake
    # folder at beginning of season.  These files will be populated eventually,
    # but are causing issues with code below
    purrr::discard(~ nrow(.x) == 0) %>% 
    # format data
    #x %>% # for testing
    map(., function(x) { 
      # assign to temporary object foo.  Needed for `if` statement at end
      # of function.
      # only keep rows from Sample.code to end.  This omits standard
      # curves, graphs, etc
      x[which(x == "Sample.code"):nrow(x),] %>% 
        janitor::row_to_names(., row_number = 1) %>% # elevate first row to names
        janitor::clean_names(.) %>%
        rename_with(.cols = last_col(), ~"file") %>% # GC file name is in last column
        select(sample,
               file,
               contains("ppm"),
               contains("percent"),
               contains("flag")) %>%
        filter(!(grepl("STD", sample, ignore.case = TRUE)), # remove standards
               !(grepl("stop", sample, ignore.case = TRUE)), # remove 'stop' samples
               !(grepl("chk", sample, ignore.case = TRUE)), # remove "check" standards
               !(grepl("air", sample, ignore.case = TRUE)), # probably air standard checks
               !(grepl("fl", sample, ignore.case = TRUE)), # Falls Lake
               !(grepl("act", sample, ignore.case = TRUE)), # Acton Lake
               sample != "") %>%  # exclude blank rows
        mutate(across(-c(sample, file), as.numeric), # convert to numeric
               # total = case_when(grepl("n2_percent", colnames(.)) ~ (ch4_ppm/10000) + (co2_ppm/10000) + (n2o_ppm/10000) + n2_percent + o2_percent + ar_percent,
               #                   TRUE ~ NA_real_),
               sample = str_remove(sample, '\\.'), # remove "." from exetainer codes
               sample = str_remove(sample, '\\_'), # remove "_" from exetainer codes
               sample = gsub("sg", "SG", sample, ignore.case = TRUE), # sg -> SG
               sample = sub("\\R10+$", "", sample)) %>% # replace "R10" from the END of sample code.
        # "flag" and "analyte" are in different orders across the spreadsheets.
        # Need to make uniform
        # case_when works even if the specified column isn't present
        # https://stackoverflow.com/questions/68965823/rename-only-if-field-exists-otherwise-ignore
        rename_with(
          ~case_when(
            . == "ch4_flag" ~ "flag_ch4",
            . ==  "co2_flag" ~ "flag_co2",
            . == "n2o_flag" ~ "flag_n2o",
            . == "n2_flag" ~ "flag_n2",
            . == "ar_flag" ~ "flag_ar",
            . == "o2_flag" ~ "flag_o2",
            TRUE ~ .))
      
    }) %>%
    map_dfr(., identity) # rbinds into one df
}

# apply function
gc <- get_gc(paths = paths) # warnings are ok
dim(gc) # 1289 records [2/26/2024]

# Check for duplicates.  Should be none.
gc %>% janitor::get_dupes(sample) %>% 
  select(sample, dupe_count, ch4_ppm, n2_percent) %>% 
  print(n=Inf)

# Any negative values
# yup, 148, write to clipboard for Kit to inspect
# gc %>%
#   select(-contains("flag")) %>%
#   pivot_longer(-c(sample, file)) %>%
#   filter(value < 0) %>%
#   write.table(file = "clipboard", row.names = F)

# lets replace any negative CH4 and CO2 with NA for
# now
gc <- gc %>%
  mutate(ch4_ppm = case_when(ch4_ppm < 0 ~ NA_real_,
                             TRUE ~ ch4_ppm),
         co2_ppm = case_when(co2_ppm < 0 ~ NA_real_,
                             TRUE ~ co2_ppm))


# MERGE EXETAINER CODES----------------------
# see readFieldSheets.R
dim(all_exet) #2713

# List of exetainer codes !=8 characters long.
# region 8 stuff plus SG2201, a short tube sample from NAR, so good
all_exet %>%
  filter(!is.na(sample), nchar(sample) != 8) %>%
  print(n=Inf)

# Any duplicates
# It appears that the Region 10 and Cincinnati field crews used exetainers 
# with identical sample codes in 2020.  I asked Kit and Pegasus to investigate
# I asked Katie Buckler to check on duplicated codes from ADA # [2/28/2024]
# Pegasus inadvertantly sent ADA two ...0153 exetainers in 2022 and 2023.
# These are air and DG samples.  Probably discard the air and compare DG to
# dups to determine which lakes they are from.
all_exet %>% janitor::get_dupes(sample) %>% print(n=Inf) #136 dups
# which are not from 2020,  just 0153, see above
all_exet %>% janitor::get_dupes(sample) %>% 
  filter(!grepl("SG20", sample)) %>% # if interested in those other than 2020 codes
  left_join(., lake.list %>% select(lake_id, lab) %>% 
              mutate(lake_id = as.character(lake_id))) %>%
  left_join(gc %>% select(sample, file)) %>%
  select(-file)

# omit duplicates.  We can't determine which lake/site they came from.
all_exet <- all_exet[!(duplicated(all_exet$sample) | duplicated(all_exet$sample, fromLast = TRUE)), ]

dim(all_exet) #2577, down 136, good

# MERGE EXETAINER CODES AND GC DATA-----------------
# [2/23/2026] need to come back and resolve
# any unmatched records
dim(gc) #1289 [2/26/24]
dim(all_exet) #2577
gc_lakeid <- full_join(gc, all_exet)
dim(gc_lakeid) # 2755

# omit rows that don't have gas data.  
# as of [4/2/2024] I haven't read in DG or AIR gc data
# Note that some trap samples were run
# on Shimadzu and therefore don't have n2, ar, or O2 percent.  Filter on ppm variables.
# Had some trouble with this operation.  Ended up using tidyverse 1.3.1 approach
# https://stackoverflow.com/questions/41609912/remove-rows-where-all-variables-are-na-using-dplyr
gc_lakeid <- gc_lakeid %>% rowwise() %>%
  filter(!all(is.na(c_across(contains("ppm"))))) # if all ppm columns are NA, then omit row
dim(gc_lakeid) # 1289

# How many are missing lake_id and/or site_id
# 60 samples with GC data, but no matching code?  
gc_lakeid %>% filter(is.na(lake_id), # no lake_id 
                     !grepl("r", sample)) # exclude reruns which are indicated with an r and are not in field sheets  
# How many are missing lake_id and/or site_id
gc_lakeid %>% filter(is.na(site_id), # no site_id 
                     !grepl("r", sample)) # no reruns.
# the unmatched codes are from several years.  In a few cases the GC codes
# were formatted incorrectly, Kit and Pegasus are working on these.  The
# many 2020 codes were stripped out due to duplication and may account for some of these.
# Have Pegasus look at 21, 22, and 23 codes.Need to revisit
gc_lakeid %>% filter(is.na(site_id), 
                     !grepl("r", sample),
                     !grepl("SG20", sample)) %>% 
  pull(sample) %>%
  write.csv(., "output/exetainersCodesOnGcButNotFieldSheets.csv")
  

# remove records without lake_id, but gotta keep the repeat injections
# from 1289 to 1229, dropped 60,  good (see row 144 above)
gc_lakeid <- gc_lakeid %>% 
  filter(case_when(
    !is.na(lake_id) ~ TRUE, # retain observations that have a lake_id value
    is.na(lake_id) & grepl("r", sample) ~ TRUE)) # keep if no lake_id but have "r'" in code (reruns)
dim(gc_lakeid) #1229


# DEAL WITH REPEAT INJECTIONS---------
# samples that failed qa.qc were rerun.  The failed analyte has a value
# of 1 in the corresponding flag column.  The rerun used the same sample
# ID, but added an "r" somewhere in the name.  
# 1. pull reruns into a new df
rerun <- gc_lakeid %>% 
  filter(grepl("r", sample)) %>% # pull out reruns
  select(matches("ppm|percent"), sample) %>% # pull out data of interest
  mutate(sample = gsub("r", replacement = "", x = sample, ignore.case = TRUE)) %>% # remove "r" from sample
  rename_with(~paste0(., "_rerun"), .cols = matches("ppm|percent")) # append "rerun" to data
dim(rerun) #118

# 2.  Do all reruns have a value for the original analyses?
# 10 reruns without corresponding initial run.  These are likely
# samples where Kit needs to fix Exetainer codes.  Make sure these
# get incorporated into final DF.
rerun$sample[!(rerun$sample %in% gc_lakeid$sample)]

# 3. remove reruns from gc_lakeid
gc_lakeid <- gc_lakeid %>% filter(!grepl("r", sample))
dim(gc_lakeid) #1111, down by 118 from 118, good

# 4. Merge reruns into gc_lakeid
gc_lakeid <- full_join(gc_lakeid, rerun)
dim(gc_lakeid) #1121, up 10 from 1069, these are rerun samples without corresponding initial run.

# 5. replace flagged values with rerun values
gc_lakeid <- gc_lakeid %>%
  mutate(ch4_ppm = case_when(flag_ch4 == 1 ~ ch4_ppm_rerun,
                             is.na(ch4_ppm) & !is.na(ch4_ppm_rerun) ~ ch4_ppm_rerun,
                             TRUE ~ ch4_ppm),
         co2_ppm = case_when(flag_co2 == 1 ~ co2_ppm_rerun,
                             is.na(co2_ppm) & !is.na(co2_ppm_rerun) ~ co2_ppm_rerun,
                             TRUE ~ co2_ppm),
         n2o_ppm = case_when(flag_n2o == 1 ~ n2o_ppm_rerun,
                             is.na(n2o_ppm) & !is.na(n2o_ppm_rerun) ~ n2o_ppm_rerun,
                             TRUE ~ n2o_ppm),
         n2_percent = case_when(flag_n2 == 1 ~ n2_percent_rerun,
                                is.na(n2_percent) & !is.na(n2_percent_rerun) ~ n2_percent_rerun,
                                TRUE ~ n2_percent),
         o2_percent = case_when(flag_o2 == 1 ~ o2_percent_rerun,
                                is.na(o2_percent) & !is.na(o2_percent_rerun) ~ o2_percent_rerun,
                                TRUE ~ o2_percent),
         ar_percent = case_when(flag_ar == 1 ~ ar_percent_rerun,
                                is.na(ar_percent) & !is.na(ar_percent_rerun) ~ ar_percent_rerun,
                                TRUE ~ ar_percent),
         total = sum((ch4_ppm/10000) + (co2_ppm/10000) + (n2o_ppm/10000) + n2_percent + o2_percent + ar_percent)) %>%
  select(-matches("flag|rerun"))

dim(gc_lakeid) #1121




# QA/QC GC REPS--------------

pdf("output/figures/scatterplot3dTrap.pdf",
    paper = "a4r", width = 11, height = 8)  # initiate landscape pdf file)
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
  group_by(lake_id, site_id, visit) %>%
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
  geom_point() 

ggplot(gc_lakeid_agg, aes(ch4_ppm)) + # lots of low CH4
  geom_freqpoly() 

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


