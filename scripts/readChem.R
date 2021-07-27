# SCRIPT TO READ IN WATER CHEM



# READ AND FORMAT WATER CHEM---------------
# Data retrieved from L drive on 11/09/2016
chem16 <- read_excel("ohio2016/inputData/waterChemistry/2016_ESF-EFWS_NutrientData_Updated04072017_SS.xlsx", 
                   sheet = "2016DATA", skip = 1)

chem17 <- read_excel("ohio2016/inputData/waterChemistry/2017_ESF-EFWS_NutrientData_Updated03312017_SS.xlsx", 
                     sheet = "2017DATA", skip = 1)

chem <- merge(chem16, chem17, all = TRUE)


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
chem <- mutate(chem, site = 
                 # fix Hocking
                 ifelse(grepl(pattern = "HOC", x = site),
                        gsub(pattern = "HOC", 
                             replacement = "HOK", 
                             site),
                        
                 # Fix Senacaville
                 ifelse(grepl(pattern = "SEN", x = site),
                         gsub(pattern = "SEN", 
                              replacement = "SNC", 
                              site),
                               
                 # Fix Acton
                 ifelse(site %in% c("U04", "U18"),
                        paste("ACN", site, sep = ""),
                              
                 # Fix Brookeville
                 ifelse(site == "SU35_2",
                        "BVRSU35",
                        
                # Fix Cave Run
                ifelse(site == "CRRSU4",
                       "CRRSU46",
                        site))))))


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


fChem <- mutate(fChem,      # blank and unknown switched at CMLSU01  
                finalConc = ifelse(analyte == "TOC" & site == "CMLUS01",
                                          3.34,
                            ifelse(analyte == "TOC" & site == "CMLUS01BLK",
                                          0.28,
                                   
                            # Looks like sample contained blank water       
                            ifelse(analyte == "TOC" & site == "MILSU01D",
                                          NA,
                                   
                            # blank and unknown switched at MJKS23        
                            ifelse(analyte == "TOC" & site == "MJKS23",
                                   5.06,
                            ifelse(analyte == "TOC" & site == "MJKS23BLK",
                                   0.29,
                                   
                            # Looks like sample contained blank water         
                            ifelse(analyte == "TOC" & site == "TPNSU07D",
                                   NA,
                                   
                            # Tappan Lake SU-28 UNK-TOC is too high 17.7
                            ifelse(analyte == "TOC" & site == "TPNSU28D",
                                   NA,
                                   finalConc)
                )))))))


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

# Strip ugly characters from analyte names
names(fChemAgBySiteW) = gsub(pattern = c("\\(| |#|)|/|-"), 
                             replacement = ".", 
                             x = names(fChemAgBySiteW))


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

# Knox Lake chem data
# From Sam on 4/7/2017
# site ID: KNX_S01, KNXS01D.  I checked the KNX_S01 and KNXS01D samples and there 
# are notes in column AA that we had a lot of troubleshooting that day and there 
# was no volume left to rerun after it was resolved.  This affected TNO2-3 on 
# both samples and TRP also on KNXS01D.  Sorry, Jake – we do everything we can to
# prevent data loss.  