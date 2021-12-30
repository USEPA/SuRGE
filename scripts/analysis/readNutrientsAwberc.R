# NUTRIENT ANALYSIS ON LACHAT CONDUCTED IN AWBERC

# Original data can be found at: L:\Priv\Cin\ORD\Pegasus-ESF\Lachat Data\Nutrients
# Copy of original 2021 data file at SP: 
# ....data\chemistry\nutrients\2021_ESF-EFWS_NutrientData_Updated11082021_AKB.xlsx"

# see readChem.R in mulitResSurvey repo for an example of how to read data.

# Need to create a "nutrients_qual" column to indicate holding time violations.
# Value of "HOLD" if holding time violated, else blank.  Holding time should be
# calculated as difference between "analyte_detection_date" and "collection_date".
# use flag columns (i.e. srp_flag, no2_3_flag) to indicate censored values.  

# The lab quantified inorganics (no2, no2.3, oP (named RP in awberc data file),
# and nh4) in both filtered and unfiltered samples.  We only want inorganic data
# for filtered samples.  Filtered samples can be identified by the "D" proceeding
# the sample collection depth in the 'pos $char4.' column (e.g., D-Sh).

# The lab quantified total nutrients (tp and tn) for both filtered and unfiltered
# samples.  We only want tp and tn for unfiltered samples.  Unfiltered samples
# can be identified by the "T" proceeding the sample collection depth in the 
# 'pos $char4.' column (e.g., T-Sh).

# All analyte names begin with a "T" to indicate "total" (e.g. TNH4).  Ignore this
# and use the analyte names defined in github issue #10.
# SCRIPT TO READ IN WATER CHEM

get_awberc_data <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, range = "A2:AZ12000") %>%
  # d <- read_excel(paste0(cin.awberc.path,
  #                        "2021_ESF-EFWS_NutrientData_Updated11082021_AKB.xlsx"), #
  #                 sheet = "2021 Data", range = "A2:AZ12000") %>%
    janitor::clean_names() %>% # clean up names for rename and select, below
    janitor::remove_empty(which = c("rows", "cols")) %>% # remove empty rows
    rename(rdate = collection_date_cdate, #rename fields
           ddate = analyte_detection_date_ddate,
           finalConc = peak_concentration_corrected_for_dilution_factor,
           analyte = analyte_name_analy,
           tp_tn = tp_tn_adjusted_concentration_full_series_ug_p_l,
           lake_id = site_id_id,
           crossid = c_ross_id_pos, 
           site_id = long_id_subid,
           sample_type = type) %>%
    mutate(nutrients_qual = if_else( # determine if holding time exceeded
      (as.Date(ddate) - as.Date(rdate)) > 28, TRUE, FALSE)) %>% # TRUE = hold time violation
    mutate(finalConc = ifelse( # correct TP and TN are in tp_tn
      analyte %in% c("TP", "TN"),
      tp_tn,
      finalConc)) %>%  
    filter(sample_type != "SPK") %>% # exclude matrix spike
    select(lake_id, site_id, crossid, sample_type, analyte, finalConc, nutrients_qual) %>% # keep only needed fields
    mutate(analyte = str_to_lower(analyte)) %>% #make analyte names lowercase
    mutate(analyte = case_when( # change analyte names where necessary
      analyte == "trp" ~ "op",
      analyte == "tnh4" ~ "nh4",
      analyte == "tno2" ~ "no2",
      analyte == "tno2-3" ~ "no2_3",
      TRUE   ~ analyte)) %>%
    # the commented code below addresses samples not included in SuRGE
    # inventory.  It doesn't hurt anything, but isn't needed
    # mutate(site_id = case_when(
    #   site_id == "BLK" ~ "blank",
    #   site_id == "STD" ~ "standard",
    #   site_id == " " ~ " ",
    #   site_id == " " ~ " ",
    #   TRUE ~ site_id)) %>%
    mutate(crossid = case_when( # I'm not sure this is needed for SuRGE data...
      crossid == " " ~ " ",
      crossid == " " ~ " ",
      crossid == " " ~ " ",
      crossid == " " ~ " ",
      TRUE ~ crossid)) %>%
    # strip character values from site_id, convert to numeric
    mutate(site_id =  as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
    mutate(sample_type = case_when( # recode sample type identifiers
      sample_type == "UKN" ~ "unknown",
      sample_type == "DUP" ~ "duplicate",
      sample_type == "BLK" ~ "blank",
      TRUE ~ sample_type)) %>%
    # sample filtered or unfiltered
    mutate(filter = str_sub(crossid, 1, 1) %>% tolower(.),
           filter = case_when(
             filter == "d" ~ "filtered",
             filter == "t" ~ "unfiltered",
             TRUE ~ filter)) %>%
    # define sample depth
      mutate(sample_depth = str_sub(crossid, 3, nchar(crossid)),
             sample_depth = case_when(
               grepl("d", sample_depth, ignore.case = TRUE) ~ "deep",
               grepl("s", sample_depth, ignore.case = TRUE) ~ "shallow",
               TRUE ~ sample_depth)) %>%
    # strip out unneeded analyses
    # exclude filtered samples run for totals
    filter(!(analyte %in% c("tp", "tn") & filter == "filtered")) %>%
    # exclude unfiltered samples run for inorganics
    filter(!(analyte %in% c("nh4", "op", "no2_3", "no2") & filter == "unfiltered")) %>%
    filter(!(analyte == "turea")) # exclude urea
      

  
  return(d)
  
}


cin.awberc.path <- paste0(userPath, 
                       "data/chemistry/nutrients/")
  
chem21 <- get_awberc_data(cin.awberc.path, 
                          "2021_ESF-EFWS_NutrientData_Updated11082021_AKB.xlsx", 
                          "2021 Data")

# Filter SuRGE data
# Most samples are coded as CH4-###, except the samples from the lacustrine,
# riverine, and transitional zones of Francis Case and Oahe (069 and 070).
chem21 <- chem21 %>%
  filter(grepl(pattern = c("CH4|069|070"), lake_id))

# This data object contains laboratory dups that must be aggregated.  These
# data can be identified as having identical values for all fields except
# finalConc.  They have different values for the 'REP" field in the original data.
# This aggregation can be accomplished by grouping and summarizing.  I suspect
# you will want to write another function to do this.

# we also need to format lake_id values (see Wiki page 'lake_id and site_id formats')



# Replace spaces and unusual characters in column names with ".".
# Note that "(" is a special character in R.  Must precdede with \\ for
# R to read as character.
names(chem) = gsub(pattern = c("\\(| |#|)|/|-"), replacement = ".", x = names(chem))
chem <- rename(chem, rdate = cdate.yymmdd10.,
               finalConc = Peak.Concentration..corrected.for.dilution.factor.,
               analyte = Analyte.Name..ANALY.,
               tp = TP.FINAL.concentration..ug.P.L.,
               site = site.id..ID.,
               longid = long.id..SUBID.) %>%
  select(rdate, site, REP., TYPE, analyte, finalConc,
         UNIT, tp) 


# Check units
distinct(chem, UNIT) # all ug, except mg C/L for TOC
# a few samples from 2016-07-07 with "." for units. doesn't affect me.

#############################################
# Pull out data from multi res survey 2016
# Hocking Lake was coded in chem samples as HOC, but should be HOK.
# Senecaville Lake (SNC, 2016-09-13), entered as SEN in chem file
# Acton Lake (ACN, 2016-05-31), site ids U04 and U18 in chem file for this
# Brookeville SU-35 is coded as SU35_2.
# Cave Run SU4 should be SU46



# Pull out site values that contain the 3 letter code for each reservoir.
# translationKeydf is from masterLibrary
matchPattern <- paste(translationKeydf$site, # 3 letter code in "or" statement
                      collapse = "|")
fChem <- filter(chem, grepl(pattern = matchPattern, x = site))


# The correct TP value is in "tp", not in "finalConc"
# Repace "finalConc" value with "tp" value for analyte == TP
fChem <- mutate(fChem, finalConc = ifelse(analyte == "TP",
                                          tp,
                                          finalConc))
# Change "UKN" to "UNK"
fChem <- mutate(fChem, TYPE = ifelse(grepl("UNK", TYPE), "UKN", TYPE))

#############################################
# Add unique identifiers and fix issues with fChem
fChem <- mutate(fChem, siteAbb = substr(site, 1, 3))
length(unique(fChem$siteAbb))  # 32.  All lakes accounted for

# Which lakes missing?
filter(translationKeydf, !(site %in% fChem$siteAbb)) # All accounted for

# Add "Lake_Name" to fChem
fChem <- merge(fChem, translationKeydf,
               by.x = "siteAbb", by.y ="site", all.x = TRUE)

# Extract site
# Sometimes lake abb and site are seperated by a "_" or "-", but often the
# delimiter is missing.  Remove from all to be consistent.
fChem <- mutate(fChem, site = gsub(pattern = c("_|-"), replacement = "", x = site))

# nchar for site ranges from 3 (S04) to 4 (SU31)
# Define indicator for site nchar
ncharSite <- with(fChem, ifelse(grepl(pattern = c("SU|US"), x = site),
                                4, 3))

# Extract and format siteID
fChem <- mutate(fChem, 
                # Extract siteID using ncharSite
                siteID = substr(site, start = 4, stop = 4 + ncharSite),
                
                # Correct BLANK in TYPE field
                TYPE = ifelse(grepl(c("BLAN|BLK|B"), siteID),
                              "BLANK", TYPE),
                
                # A few blanks use "BLAN" or "BLK" as siteID.  Replace with NA.
                siteID = ifelse(grepl(c("BLAN|BLK"), siteID), 
                                NA, siteID),
                
                # Strip "B" (blank) and "D" (dup) from some siteIDs.
                siteID = gsub(pattern = c("D|B"), replacement = "", x = siteID),
                
                # All site numbers < 10 must be preceeded by a 0.
                siteID = ifelse(siteID == "SU4", "SU04", 
                                ifelse(siteID == "SU7", "SU07",
                                       siteID)),
                
                #Add dash between letters and numbers to match siteID in eqAreaData
                siteID = ifelse(nchar(siteID) == 3,
                                paste(substr(siteID, start = 1, stop = 1), 
                                      "-",
                                      substr(siteID, start = 2, stop = 3),
                                      sep = ""),
                                ifelse(nchar(siteID) == 4,
                                       paste(substr(siteID, start = 1, stop = 2), 
                                             "-",
                                             substr(siteID, start = 3, stop = 4),
                                             sep = ""),
                                       siteID)))

# Wingfoot (WGF) samples collected at U-14 and U-16, but entered in chem file
# as U-16 and U-162.  Wingfoot was unstratified and the chem values are
# very similar.  Randomly assign one as U-16 and the other as U-14
fChem <- mutate(fChem, siteID = ifelse(siteID == "U1-62",
                                       "U-14", siteID))

# Kiser Lake siteID is stratified in fChem (S-02, S-12),
# but should be unstratified (U-02, U-12)
fChem <- mutate(fChem, 
                siteID = ifelse(siteAbb == "KIS", 
                                gsub("S", "U", siteID), # replace S with U
                                siteID))


# Verify all Lake_Name x siteID combinations are in eqAreaData
# Don't inlcude blanks because they don't all have a siteID.
# sum = 0, therefore all match
sum(!with(filter(fChem, TYPE != "BLANK"), # fChem, no blanks 
          paste(Lake_Name, siteID)) %in% # paste Lake_Name and siteID
      with(eqAreaData, paste(Lake_Name, siteID))) # compare to lake x site in eqArea

# Nitrite is listed as NO2 and TNO2.  Should all be TNO2
fChem <- mutate(fChem, analyte = ifelse(analyte == "NO2",
                                        "TNO2", analyte))
###########################################################
# FINALIZE FOR MERGING AND ANALYSIS
# MERGING STRATEGY
# 1) merge station specific data into eqAreaData.  This puts all sonde and water
#    chem data in one place.  Push these data through grtsMeanVariance to get
#    lake-wide mean.

# Take a quick peak for obvious problems
ggplot(fChem, aes(Lake_Name, finalConc)) + 
  geom_point(aes(color = TYPE)) + 
  facet_wrap(~analyte, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))


# Strip Blanks
fChem <-  filter(fChem, TYPE != "BLANK") %>%
  # Exclude unnecessary columns
  select(Lake_Name, siteID, TYPE, analyte, finalConc)

# Aggregate by Lake_Name x site x variable for merging with eqAreaData
# Remove TYPE column
fChem <- select(fChem, -TYPE)

# Aggregate with ddply
fChemAgBySite <- ddply(.data = fChem, .(Lake_Name, siteID, analyte), summarize, 
                       finalConc = mean(finalConc, na.rm = TRUE))

# Cast to wide for merge with eqAreaData
fChemAgBySiteW <- dcast(fChemAgBySite, Lake_Name + siteID ~ analyte, 
                        value.var = "finalConc")


# Missing TP from BVR SU-35 (tube cracked).  Estimate from TRP
tpModel <- lm(TP ~ TRP, data = fChemAgBySiteW)
newdata = data.frame(TRP = fChemAgBySiteW[with(fChemAgBySiteW, siteID == "SU-35" & 
                                                 Lake_Name == "Brookville Lake"),
                                          "TRP"])
tpPredict <- predict(tpModel, newdata = newdata)
fChemAgBySiteW[with(fChemAgBySiteW, siteID == "SU-35" & 
                      Lake_Name == "Brookville Lake"),
               "TP"] = tpPredict

# Merge with eqAreaData
str(eqAreaData) #1426 observations
eqAreaData <- merge(fChemAgBySiteW, eqAreaData, all = TRUE)
str(eqAreaData) # Still 1426, merged as expected

# Look for missing data
# See issues.R
select(eqAreaData, Lake_Name, siteID, TN, TNH4, TNO2, TNO2.3, TOC, TP, TRP) %>%
  filter(!is.na(TP))


