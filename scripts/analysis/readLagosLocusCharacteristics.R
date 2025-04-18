# Package ID: edi.854.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical characteristics of lakes and their watersheds in the conterminous U.S..
# Data set creator:  Nicole Smith - Michigan State University 
# Data set creator:  Katherine Webster - Michigan State University 
# Data set creator:  Lauren Rodriguez - Michigan State University 
# Data set creator:  Kendra Cheruvelil - Michigan State University 
# Data set creator:  Patricia Soranno - Michigan State University 
# Contact:  Kendra Cheruvelil -  Michigan State University  - ksc@msu.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      

# Code below copied from "Code Generation: R" section of EDI

options(HTTPUserAgent="EDI_CodeGen")

# read lake_characteristics. This is listed as file 2 in EDI and assigned to object dt2 
# in code copied from EDI. Will rename to locus_characteristics at end of this script
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/854/1/fd7fe936d290a12bc6dbf5c41047849e" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lagoslakeid",     
                 "lake_waterarea_ha",     
                 "lake_totalarea_ha",     
                 "lake_islandarea_ha",     
                 "lake_perimeter_m",     
                 "lake_islandperimeter_m",     
                 "lake_shorelinedevfactor",     
                 "lake_mbgconhull_length_m",     
                 "lake_mbgconhull_width_m",     
                 "lake_mbgconhull_orientation_deg",     
                 "lake_mbgrect_length_m",     
                 "lake_mbgrect_width_m",     
                 "lake_mbgrect_arearatio",     
                 "lake_meanwidth_m",     
                 "lake_connectivity_class",     
                 "lake_connectivity_fluctuates",     
                 "lake_connectivity_permanent",     
                 "lake_lakes4ha_upstream_ha",     
                 "lake_lakes4ha_upstream_n",     
                 "lake_lakes1ha_upstream_ha",     
                 "lake_lakes1ha_upstream_n",     
                 "lake_lakes10ha_upstream_n",     
                 "lake_lakes10ha_upstream_ha",     
                 "lake_glaciatedlatewisc"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$lagoslakeid)=="factor") dt2$lagoslakeid <-as.numeric(levels(dt2$lagoslakeid))[as.integer(dt2$lagoslakeid) ]               
if (class(dt2$lagoslakeid)=="character") dt2$lagoslakeid <-as.numeric(dt2$lagoslakeid)
if (class(dt2$lake_waterarea_ha)=="factor") dt2$lake_waterarea_ha <-as.numeric(levels(dt2$lake_waterarea_ha))[as.integer(dt2$lake_waterarea_ha) ]               
if (class(dt2$lake_waterarea_ha)=="character") dt2$lake_waterarea_ha <-as.numeric(dt2$lake_waterarea_ha)
if (class(dt2$lake_totalarea_ha)=="factor") dt2$lake_totalarea_ha <-as.numeric(levels(dt2$lake_totalarea_ha))[as.integer(dt2$lake_totalarea_ha) ]               
if (class(dt2$lake_totalarea_ha)=="character") dt2$lake_totalarea_ha <-as.numeric(dt2$lake_totalarea_ha)
if (class(dt2$lake_islandarea_ha)=="factor") dt2$lake_islandarea_ha <-as.numeric(levels(dt2$lake_islandarea_ha))[as.integer(dt2$lake_islandarea_ha) ]               
if (class(dt2$lake_islandarea_ha)=="character") dt2$lake_islandarea_ha <-as.numeric(dt2$lake_islandarea_ha)
if (class(dt2$lake_perimeter_m)=="factor") dt2$lake_perimeter_m <-as.numeric(levels(dt2$lake_perimeter_m))[as.integer(dt2$lake_perimeter_m) ]               
if (class(dt2$lake_perimeter_m)=="character") dt2$lake_perimeter_m <-as.numeric(dt2$lake_perimeter_m)
if (class(dt2$lake_islandperimeter_m)=="factor") dt2$lake_islandperimeter_m <-as.numeric(levels(dt2$lake_islandperimeter_m))[as.integer(dt2$lake_islandperimeter_m) ]               
if (class(dt2$lake_islandperimeter_m)=="character") dt2$lake_islandperimeter_m <-as.numeric(dt2$lake_islandperimeter_m)
if (class(dt2$lake_shorelinedevfactor)=="factor") dt2$lake_shorelinedevfactor <-as.numeric(levels(dt2$lake_shorelinedevfactor))[as.integer(dt2$lake_shorelinedevfactor) ]               
if (class(dt2$lake_shorelinedevfactor)=="character") dt2$lake_shorelinedevfactor <-as.numeric(dt2$lake_shorelinedevfactor)
if (class(dt2$lake_mbgconhull_length_m)=="factor") dt2$lake_mbgconhull_length_m <-as.numeric(levels(dt2$lake_mbgconhull_length_m))[as.integer(dt2$lake_mbgconhull_length_m) ]               
if (class(dt2$lake_mbgconhull_length_m)=="character") dt2$lake_mbgconhull_length_m <-as.numeric(dt2$lake_mbgconhull_length_m)
if (class(dt2$lake_mbgconhull_width_m)=="factor") dt2$lake_mbgconhull_width_m <-as.numeric(levels(dt2$lake_mbgconhull_width_m))[as.integer(dt2$lake_mbgconhull_width_m) ]               
if (class(dt2$lake_mbgconhull_width_m)=="character") dt2$lake_mbgconhull_width_m <-as.numeric(dt2$lake_mbgconhull_width_m)
if (class(dt2$lake_mbgconhull_orientation_deg)=="factor") dt2$lake_mbgconhull_orientation_deg <-as.numeric(levels(dt2$lake_mbgconhull_orientation_deg))[as.integer(dt2$lake_mbgconhull_orientation_deg) ]               
if (class(dt2$lake_mbgconhull_orientation_deg)=="character") dt2$lake_mbgconhull_orientation_deg <-as.numeric(dt2$lake_mbgconhull_orientation_deg)
if (class(dt2$lake_mbgrect_length_m)=="factor") dt2$lake_mbgrect_length_m <-as.numeric(levels(dt2$lake_mbgrect_length_m))[as.integer(dt2$lake_mbgrect_length_m) ]               
if (class(dt2$lake_mbgrect_length_m)=="character") dt2$lake_mbgrect_length_m <-as.numeric(dt2$lake_mbgrect_length_m)
if (class(dt2$lake_mbgrect_width_m)=="factor") dt2$lake_mbgrect_width_m <-as.numeric(levels(dt2$lake_mbgrect_width_m))[as.integer(dt2$lake_mbgrect_width_m) ]               
if (class(dt2$lake_mbgrect_width_m)=="character") dt2$lake_mbgrect_width_m <-as.numeric(dt2$lake_mbgrect_width_m)
if (class(dt2$lake_mbgrect_arearatio)=="factor") dt2$lake_mbgrect_arearatio <-as.numeric(levels(dt2$lake_mbgrect_arearatio))[as.integer(dt2$lake_mbgrect_arearatio) ]               
if (class(dt2$lake_mbgrect_arearatio)=="character") dt2$lake_mbgrect_arearatio <-as.numeric(dt2$lake_mbgrect_arearatio)
if (class(dt2$lake_meanwidth_m)=="factor") dt2$lake_meanwidth_m <-as.numeric(levels(dt2$lake_meanwidth_m))[as.integer(dt2$lake_meanwidth_m) ]               
if (class(dt2$lake_meanwidth_m)=="character") dt2$lake_meanwidth_m <-as.numeric(dt2$lake_meanwidth_m)
if (class(dt2$lake_connectivity_class)!="factor") dt2$lake_connectivity_class<- as.factor(dt2$lake_connectivity_class)
if (class(dt2$lake_connectivity_fluctuates)!="factor") dt2$lake_connectivity_fluctuates<- as.factor(dt2$lake_connectivity_fluctuates)
if (class(dt2$lake_connectivity_permanent)!="factor") dt2$lake_connectivity_permanent<- as.factor(dt2$lake_connectivity_permanent)
if (class(dt2$lake_lakes4ha_upstream_ha)=="factor") dt2$lake_lakes4ha_upstream_ha <-as.numeric(levels(dt2$lake_lakes4ha_upstream_ha))[as.integer(dt2$lake_lakes4ha_upstream_ha) ]               
if (class(dt2$lake_lakes4ha_upstream_ha)=="character") dt2$lake_lakes4ha_upstream_ha <-as.numeric(dt2$lake_lakes4ha_upstream_ha)
if (class(dt2$lake_lakes4ha_upstream_n)=="factor") dt2$lake_lakes4ha_upstream_n <-as.numeric(levels(dt2$lake_lakes4ha_upstream_n))[as.integer(dt2$lake_lakes4ha_upstream_n) ]               
if (class(dt2$lake_lakes4ha_upstream_n)=="character") dt2$lake_lakes4ha_upstream_n <-as.numeric(dt2$lake_lakes4ha_upstream_n)
if (class(dt2$lake_lakes1ha_upstream_ha)=="factor") dt2$lake_lakes1ha_upstream_ha <-as.numeric(levels(dt2$lake_lakes1ha_upstream_ha))[as.integer(dt2$lake_lakes1ha_upstream_ha) ]               
if (class(dt2$lake_lakes1ha_upstream_ha)=="character") dt2$lake_lakes1ha_upstream_ha <-as.numeric(dt2$lake_lakes1ha_upstream_ha)
if (class(dt2$lake_lakes1ha_upstream_n)=="factor") dt2$lake_lakes1ha_upstream_n <-as.numeric(levels(dt2$lake_lakes1ha_upstream_n))[as.integer(dt2$lake_lakes1ha_upstream_n) ]               
if (class(dt2$lake_lakes1ha_upstream_n)=="character") dt2$lake_lakes1ha_upstream_n <-as.numeric(dt2$lake_lakes1ha_upstream_n)
if (class(dt2$lake_lakes10ha_upstream_n)=="factor") dt2$lake_lakes10ha_upstream_n <-as.numeric(levels(dt2$lake_lakes10ha_upstream_n))[as.integer(dt2$lake_lakes10ha_upstream_n) ]               
if (class(dt2$lake_lakes10ha_upstream_n)=="character") dt2$lake_lakes10ha_upstream_n <-as.numeric(dt2$lake_lakes10ha_upstream_n)
if (class(dt2$lake_lakes10ha_upstream_ha)=="factor") dt2$lake_lakes10ha_upstream_ha <-as.numeric(levels(dt2$lake_lakes10ha_upstream_ha))[as.integer(dt2$lake_lakes10ha_upstream_ha) ]               
if (class(dt2$lake_lakes10ha_upstream_ha)=="character") dt2$lake_lakes10ha_upstream_ha <-as.numeric(dt2$lake_lakes10ha_upstream_ha)
if (class(dt2$lake_glaciatedlatewisc)!="factor") dt2$lake_glaciatedlatewisc<- as.factor(dt2$lake_glaciatedlatewisc)

# Convert Missing Values to NA for non-dates

dt2$lagoslakeid <- ifelse((trimws(as.character(dt2$lagoslakeid))==trimws("NA")),NA,dt2$lagoslakeid)               
suppressWarnings(dt2$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lagoslakeid))==as.character(as.numeric("NA"))),NA,dt2$lagoslakeid))
dt2$lake_waterarea_ha <- ifelse((trimws(as.character(dt2$lake_waterarea_ha))==trimws("NA")),NA,dt2$lake_waterarea_ha)               
suppressWarnings(dt2$lake_waterarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_waterarea_ha))==as.character(as.numeric("NA"))),NA,dt2$lake_waterarea_ha))
dt2$lake_totalarea_ha <- ifelse((trimws(as.character(dt2$lake_totalarea_ha))==trimws("NA")),NA,dt2$lake_totalarea_ha)               
suppressWarnings(dt2$lake_totalarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_totalarea_ha))==as.character(as.numeric("NA"))),NA,dt2$lake_totalarea_ha))
dt2$lake_islandarea_ha <- ifelse((trimws(as.character(dt2$lake_islandarea_ha))==trimws("NA")),NA,dt2$lake_islandarea_ha)               
suppressWarnings(dt2$lake_islandarea_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_islandarea_ha))==as.character(as.numeric("NA"))),NA,dt2$lake_islandarea_ha))
dt2$lake_perimeter_m <- ifelse((trimws(as.character(dt2$lake_perimeter_m))==trimws("NA")),NA,dt2$lake_perimeter_m)               
suppressWarnings(dt2$lake_perimeter_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_perimeter_m))==as.character(as.numeric("NA"))),NA,dt2$lake_perimeter_m))
dt2$lake_islandperimeter_m <- ifelse((trimws(as.character(dt2$lake_islandperimeter_m))==trimws("NA")),NA,dt2$lake_islandperimeter_m)               
suppressWarnings(dt2$lake_islandperimeter_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_islandperimeter_m))==as.character(as.numeric("NA"))),NA,dt2$lake_islandperimeter_m))
dt2$lake_shorelinedevfactor <- ifelse((trimws(as.character(dt2$lake_shorelinedevfactor))==trimws("NA")),NA,dt2$lake_shorelinedevfactor)               
suppressWarnings(dt2$lake_shorelinedevfactor <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_shorelinedevfactor))==as.character(as.numeric("NA"))),NA,dt2$lake_shorelinedevfactor))
dt2$lake_mbgconhull_length_m <- ifelse((trimws(as.character(dt2$lake_mbgconhull_length_m))==trimws("NA")),NA,dt2$lake_mbgconhull_length_m)               
suppressWarnings(dt2$lake_mbgconhull_length_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_mbgconhull_length_m))==as.character(as.numeric("NA"))),NA,dt2$lake_mbgconhull_length_m))
dt2$lake_mbgconhull_width_m <- ifelse((trimws(as.character(dt2$lake_mbgconhull_width_m))==trimws("NA")),NA,dt2$lake_mbgconhull_width_m)               
suppressWarnings(dt2$lake_mbgconhull_width_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_mbgconhull_width_m))==as.character(as.numeric("NA"))),NA,dt2$lake_mbgconhull_width_m))
dt2$lake_mbgconhull_orientation_deg <- ifelse((trimws(as.character(dt2$lake_mbgconhull_orientation_deg))==trimws("NA")),NA,dt2$lake_mbgconhull_orientation_deg)               
suppressWarnings(dt2$lake_mbgconhull_orientation_deg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_mbgconhull_orientation_deg))==as.character(as.numeric("NA"))),NA,dt2$lake_mbgconhull_orientation_deg))
dt2$lake_mbgrect_length_m <- ifelse((trimws(as.character(dt2$lake_mbgrect_length_m))==trimws("NA")),NA,dt2$lake_mbgrect_length_m)               
suppressWarnings(dt2$lake_mbgrect_length_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_mbgrect_length_m))==as.character(as.numeric("NA"))),NA,dt2$lake_mbgrect_length_m))
dt2$lake_mbgrect_width_m <- ifelse((trimws(as.character(dt2$lake_mbgrect_width_m))==trimws("NA")),NA,dt2$lake_mbgrect_width_m)               
suppressWarnings(dt2$lake_mbgrect_width_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_mbgrect_width_m))==as.character(as.numeric("NA"))),NA,dt2$lake_mbgrect_width_m))
dt2$lake_mbgrect_arearatio <- ifelse((trimws(as.character(dt2$lake_mbgrect_arearatio))==trimws("NA")),NA,dt2$lake_mbgrect_arearatio)               
suppressWarnings(dt2$lake_mbgrect_arearatio <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_mbgrect_arearatio))==as.character(as.numeric("NA"))),NA,dt2$lake_mbgrect_arearatio))
dt2$lake_meanwidth_m <- ifelse((trimws(as.character(dt2$lake_meanwidth_m))==trimws("NA")),NA,dt2$lake_meanwidth_m)               
suppressWarnings(dt2$lake_meanwidth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_meanwidth_m))==as.character(as.numeric("NA"))),NA,dt2$lake_meanwidth_m))
dt2$lake_connectivity_class <- as.factor(ifelse((trimws(as.character(dt2$lake_connectivity_class))==trimws("NA")),NA,as.character(dt2$lake_connectivity_class)))
dt2$lake_connectivity_fluctuates <- as.factor(ifelse((trimws(as.character(dt2$lake_connectivity_fluctuates))==trimws("NA")),NA,as.character(dt2$lake_connectivity_fluctuates)))
dt2$lake_connectivity_permanent <- as.factor(ifelse((trimws(as.character(dt2$lake_connectivity_permanent))==trimws("NA")),NA,as.character(dt2$lake_connectivity_permanent)))
dt2$lake_lakes4ha_upstream_ha <- ifelse((trimws(as.character(dt2$lake_lakes4ha_upstream_ha))==trimws("NA")),NA,dt2$lake_lakes4ha_upstream_ha)               
suppressWarnings(dt2$lake_lakes4ha_upstream_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_lakes4ha_upstream_ha))==as.character(as.numeric("NA"))),NA,dt2$lake_lakes4ha_upstream_ha))
dt2$lake_lakes4ha_upstream_n <- ifelse((trimws(as.character(dt2$lake_lakes4ha_upstream_n))==trimws("NA")),NA,dt2$lake_lakes4ha_upstream_n)               
suppressWarnings(dt2$lake_lakes4ha_upstream_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_lakes4ha_upstream_n))==as.character(as.numeric("NA"))),NA,dt2$lake_lakes4ha_upstream_n))
dt2$lake_lakes1ha_upstream_ha <- ifelse((trimws(as.character(dt2$lake_lakes1ha_upstream_ha))==trimws("NA")),NA,dt2$lake_lakes1ha_upstream_ha)               
suppressWarnings(dt2$lake_lakes1ha_upstream_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_lakes1ha_upstream_ha))==as.character(as.numeric("NA"))),NA,dt2$lake_lakes1ha_upstream_ha))
dt2$lake_lakes1ha_upstream_n <- ifelse((trimws(as.character(dt2$lake_lakes1ha_upstream_n))==trimws("NA")),NA,dt2$lake_lakes1ha_upstream_n)               
suppressWarnings(dt2$lake_lakes1ha_upstream_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_lakes1ha_upstream_n))==as.character(as.numeric("NA"))),NA,dt2$lake_lakes1ha_upstream_n))
dt2$lake_lakes10ha_upstream_n <- ifelse((trimws(as.character(dt2$lake_lakes10ha_upstream_n))==trimws("NA")),NA,dt2$lake_lakes10ha_upstream_n)               
suppressWarnings(dt2$lake_lakes10ha_upstream_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_lakes10ha_upstream_n))==as.character(as.numeric("NA"))),NA,dt2$lake_lakes10ha_upstream_n))
dt2$lake_lakes10ha_upstream_ha <- ifelse((trimws(as.character(dt2$lake_lakes10ha_upstream_ha))==trimws("NA")),NA,dt2$lake_lakes10ha_upstream_ha)               
suppressWarnings(dt2$lake_lakes10ha_upstream_ha <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$lake_lakes10ha_upstream_ha))==as.character(as.numeric("NA"))),NA,dt2$lake_lakes10ha_upstream_ha))
dt2$lake_glaciatedlatewisc <- as.factor(ifelse((trimws(as.character(dt2$lake_glaciatedlatewisc))==trimws("NA")),NA,as.character(dt2$lake_glaciatedlatewisc)))

# write compressed version of .csv
write_csv(dt2, paste0(userPath, "data/siteDescriptors/locus_characteristics.csv.gz"))

rm(dt2)
