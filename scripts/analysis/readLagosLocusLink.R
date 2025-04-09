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

# read lake_link. This is listed as file 4 in EDI and assigned to object dt4 
# in code copied from EDI. Will rename to locus_link at end of this script
inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/854/1/5488e333ce818597fa3dbfc9b4e0c131" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lagoslakeid",     
                 "lake_nhdid",     
                 "lake_reachcode",     
                 "lake_namegnis",     
                 "lake_namelagos",     
                 "lake_county",     
                 "lake_countyfips",     
                 "lake_lat_decdeg",     
                 "lake_lon_decdeg",     
                 "lake_centroidstate",     
                 "nhdhr_area_sqkm",     
                 "nhdhr_fdate",     
                 "nhdhr_gnisid",     
                 "lagosus_legacysiteid",     
                 "lagosus_legacysitelabel",     
                 "lagosus_legacyprogram",     
                 "wqp_monitoringlocationidentifier",     
                 "wqp_monitoringlocationname",     
                 "wqp_providername",     
                 "nhdplusv2_comid",     
                 "nhdplusv2_reachcode",     
                 "nhdplusv2_area_sqkm",     
                 "lagosne_lagoslakeid",     
                 "lagosne_legacysiteid",     
                 "nla2007_siteid",     
                 "nla2012_siteid",     
                 "nhdplusv2_lakes_n",     
                 "lagosne_lakes_n",     
                 "wqp_sites_n",     
                 "lagosus_legacyids_n"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$lagoslakeid)=="factor") dt4$lagoslakeid <-as.numeric(levels(dt4$lagoslakeid))[as.integer(dt4$lagoslakeid) ]               
if (class(dt4$lagoslakeid)=="character") dt4$lagoslakeid <-as.numeric(dt4$lagoslakeid)
if (class(dt4$lake_nhdid)!="factor") dt4$lake_nhdid<- as.factor(dt4$lake_nhdid)
if (class(dt4$lake_reachcode)!="factor") dt4$lake_reachcode<- as.factor(dt4$lake_reachcode)
if (class(dt4$lake_namegnis)!="factor") dt4$lake_namegnis<- as.factor(dt4$lake_namegnis)
if (class(dt4$lake_namelagos)!="factor") dt4$lake_namelagos<- as.factor(dt4$lake_namelagos)
if (class(dt4$lake_county)!="factor") dt4$lake_county<- as.factor(dt4$lake_county)
if (class(dt4$lake_countyfips)!="factor") dt4$lake_countyfips<- as.factor(dt4$lake_countyfips)
if (class(dt4$lake_lat_decdeg)=="factor") dt4$lake_lat_decdeg <-as.numeric(levels(dt4$lake_lat_decdeg))[as.integer(dt4$lake_lat_decdeg) ]               
if (class(dt4$lake_lat_decdeg)=="character") dt4$lake_lat_decdeg <-as.numeric(dt4$lake_lat_decdeg)
if (class(dt4$lake_lon_decdeg)=="factor") dt4$lake_lon_decdeg <-as.numeric(levels(dt4$lake_lon_decdeg))[as.integer(dt4$lake_lon_decdeg) ]               
if (class(dt4$lake_lon_decdeg)=="character") dt4$lake_lon_decdeg <-as.numeric(dt4$lake_lon_decdeg)
if (class(dt4$lake_centroidstate)!="factor") dt4$lake_centroidstate<- as.factor(dt4$lake_centroidstate)
if (class(dt4$nhdhr_area_sqkm)=="factor") dt4$nhdhr_area_sqkm <-as.numeric(levels(dt4$nhdhr_area_sqkm))[as.integer(dt4$nhdhr_area_sqkm) ]               
if (class(dt4$nhdhr_area_sqkm)=="character") dt4$nhdhr_area_sqkm <-as.numeric(dt4$nhdhr_area_sqkm)
if (class(dt4$nhdhr_fdate)!="factor") dt4$nhdhr_fdate<- as.factor(dt4$nhdhr_fdate)
if (class(dt4$nhdhr_gnisid)!="factor") dt4$nhdhr_gnisid<- as.factor(dt4$nhdhr_gnisid)
if (class(dt4$lagosus_legacysiteid)!="factor") dt4$lagosus_legacysiteid<- as.factor(dt4$lagosus_legacysiteid)
if (class(dt4$lagosus_legacysitelabel)!="factor") dt4$lagosus_legacysitelabel<- as.factor(dt4$lagosus_legacysitelabel)
if (class(dt4$lagosus_legacyprogram)!="factor") dt4$lagosus_legacyprogram<- as.factor(dt4$lagosus_legacyprogram)
if (class(dt4$wqp_monitoringlocationidentifier)!="factor") dt4$wqp_monitoringlocationidentifier<- as.factor(dt4$wqp_monitoringlocationidentifier)
if (class(dt4$wqp_monitoringlocationname)!="factor") dt4$wqp_monitoringlocationname<- as.factor(dt4$wqp_monitoringlocationname)
if (class(dt4$wqp_providername)!="factor") dt4$wqp_providername<- as.factor(dt4$wqp_providername)
if (class(dt4$nhdplusv2_comid)!="factor") dt4$nhdplusv2_comid<- as.factor(dt4$nhdplusv2_comid)
if (class(dt4$nhdplusv2_reachcode)!="factor") dt4$nhdplusv2_reachcode<- as.factor(dt4$nhdplusv2_reachcode)
if (class(dt4$nhdplusv2_area_sqkm)=="factor") dt4$nhdplusv2_area_sqkm <-as.numeric(levels(dt4$nhdplusv2_area_sqkm))[as.integer(dt4$nhdplusv2_area_sqkm) ]               
if (class(dt4$nhdplusv2_area_sqkm)=="character") dt4$nhdplusv2_area_sqkm <-as.numeric(dt4$nhdplusv2_area_sqkm)
if (class(dt4$lagosne_lagoslakeid)=="factor") dt4$lagosne_lagoslakeid <-as.numeric(levels(dt4$lagosne_lagoslakeid))[as.integer(dt4$lagosne_lagoslakeid) ]               
if (class(dt4$lagosne_lagoslakeid)=="character") dt4$lagosne_lagoslakeid <-as.numeric(dt4$lagosne_lagoslakeid)
if (class(dt4$lagosne_legacysiteid)!="factor") dt4$lagosne_legacysiteid<- as.factor(dt4$lagosne_legacysiteid)
if (class(dt4$nla2007_siteid)!="factor") dt4$nla2007_siteid<- as.factor(dt4$nla2007_siteid)
if (class(dt4$nla2012_siteid)!="factor") dt4$nla2012_siteid<- as.factor(dt4$nla2012_siteid)
if (class(dt4$nhdplusv2_lakes_n)=="factor") dt4$nhdplusv2_lakes_n <-as.numeric(levels(dt4$nhdplusv2_lakes_n))[as.integer(dt4$nhdplusv2_lakes_n) ]               
if (class(dt4$nhdplusv2_lakes_n)=="character") dt4$nhdplusv2_lakes_n <-as.numeric(dt4$nhdplusv2_lakes_n)
if (class(dt4$lagosne_lakes_n)=="factor") dt4$lagosne_lakes_n <-as.numeric(levels(dt4$lagosne_lakes_n))[as.integer(dt4$lagosne_lakes_n) ]               
if (class(dt4$lagosne_lakes_n)=="character") dt4$lagosne_lakes_n <-as.numeric(dt4$lagosne_lakes_n)
if (class(dt4$wqp_sites_n)=="factor") dt4$wqp_sites_n <-as.numeric(levels(dt4$wqp_sites_n))[as.integer(dt4$wqp_sites_n) ]               
if (class(dt4$wqp_sites_n)=="character") dt4$wqp_sites_n <-as.numeric(dt4$wqp_sites_n)
if (class(dt4$lagosus_legacyids_n)=="factor") dt4$lagosus_legacyids_n <-as.numeric(levels(dt4$lagosus_legacyids_n))[as.integer(dt4$lagosus_legacyids_n) ]               
if (class(dt4$lagosus_legacyids_n)=="character") dt4$lagosus_legacyids_n <-as.numeric(dt4$lagosus_legacyids_n)

# Convert Missing Values to NA for non-dates

dt4$lagoslakeid <- ifelse((trimws(as.character(dt4$lagoslakeid))==trimws("NA")),NA,dt4$lagoslakeid)               
suppressWarnings(dt4$lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagoslakeid))==as.character(as.numeric("NA"))),NA,dt4$lagoslakeid))
dt4$lake_nhdid <- as.factor(ifelse((trimws(as.character(dt4$lake_nhdid))==trimws("NA")),NA,as.character(dt4$lake_nhdid)))
dt4$lake_reachcode <- as.factor(ifelse((trimws(as.character(dt4$lake_reachcode))==trimws("NA")),NA,as.character(dt4$lake_reachcode)))
dt4$lake_namegnis <- as.factor(ifelse((trimws(as.character(dt4$lake_namegnis))==trimws("NA")),NA,as.character(dt4$lake_namegnis)))
dt4$lake_namelagos <- as.factor(ifelse((trimws(as.character(dt4$lake_namelagos))==trimws("NA")),NA,as.character(dt4$lake_namelagos)))
dt4$lake_county <- as.factor(ifelse((trimws(as.character(dt4$lake_county))==trimws("NA")),NA,as.character(dt4$lake_county)))
dt4$lake_countyfips <- as.factor(ifelse((trimws(as.character(dt4$lake_countyfips))==trimws("NA")),NA,as.character(dt4$lake_countyfips)))
dt4$lake_lat_decdeg <- ifelse((trimws(as.character(dt4$lake_lat_decdeg))==trimws("NA")),NA,dt4$lake_lat_decdeg)               
suppressWarnings(dt4$lake_lat_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_lat_decdeg))==as.character(as.numeric("NA"))),NA,dt4$lake_lat_decdeg))
dt4$lake_lon_decdeg <- ifelse((trimws(as.character(dt4$lake_lon_decdeg))==trimws("NA")),NA,dt4$lake_lon_decdeg)               
suppressWarnings(dt4$lake_lon_decdeg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lake_lon_decdeg))==as.character(as.numeric("NA"))),NA,dt4$lake_lon_decdeg))
dt4$lake_centroidstate <- as.factor(ifelse((trimws(as.character(dt4$lake_centroidstate))==trimws("NA")),NA,as.character(dt4$lake_centroidstate)))
dt4$nhdhr_area_sqkm <- ifelse((trimws(as.character(dt4$nhdhr_area_sqkm))==trimws("NA")),NA,dt4$nhdhr_area_sqkm)               
suppressWarnings(dt4$nhdhr_area_sqkm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdhr_area_sqkm))==as.character(as.numeric("NA"))),NA,dt4$nhdhr_area_sqkm))
dt4$nhdhr_fdate <- as.factor(ifelse((trimws(as.character(dt4$nhdhr_fdate))==trimws("NA")),NA,as.character(dt4$nhdhr_fdate)))
dt4$nhdhr_gnisid <- as.factor(ifelse((trimws(as.character(dt4$nhdhr_gnisid))==trimws("NA")),NA,as.character(dt4$nhdhr_gnisid)))
dt4$lagosus_legacysiteid <- as.factor(ifelse((trimws(as.character(dt4$lagosus_legacysiteid))==trimws("NA")),NA,as.character(dt4$lagosus_legacysiteid)))
dt4$lagosus_legacysitelabel <- as.factor(ifelse((trimws(as.character(dt4$lagosus_legacysitelabel))==trimws("NA")),NA,as.character(dt4$lagosus_legacysitelabel)))
dt4$lagosus_legacyprogram <- as.factor(ifelse((trimws(as.character(dt4$lagosus_legacyprogram))==trimws("NA")),NA,as.character(dt4$lagosus_legacyprogram)))
dt4$wqp_monitoringlocationidentifier <- as.factor(ifelse((trimws(as.character(dt4$wqp_monitoringlocationidentifier))==trimws("NA")),NA,as.character(dt4$wqp_monitoringlocationidentifier)))
dt4$wqp_monitoringlocationname <- as.factor(ifelse((trimws(as.character(dt4$wqp_monitoringlocationname))==trimws("NA")),NA,as.character(dt4$wqp_monitoringlocationname)))
dt4$wqp_providername <- as.factor(ifelse((trimws(as.character(dt4$wqp_providername))==trimws("NA")),NA,as.character(dt4$wqp_providername)))
dt4$nhdplusv2_comid <- as.factor(ifelse((trimws(as.character(dt4$nhdplusv2_comid))==trimws("NA")),NA,as.character(dt4$nhdplusv2_comid)))
dt4$nhdplusv2_reachcode <- as.factor(ifelse((trimws(as.character(dt4$nhdplusv2_reachcode))==trimws("NA")),NA,as.character(dt4$nhdplusv2_reachcode)))
dt4$nhdplusv2_area_sqkm <- ifelse((trimws(as.character(dt4$nhdplusv2_area_sqkm))==trimws("NA")),NA,dt4$nhdplusv2_area_sqkm)               
suppressWarnings(dt4$nhdplusv2_area_sqkm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdplusv2_area_sqkm))==as.character(as.numeric("NA"))),NA,dt4$nhdplusv2_area_sqkm))
dt4$lagosne_lagoslakeid <- ifelse((trimws(as.character(dt4$lagosne_lagoslakeid))==trimws("NA")),NA,dt4$lagosne_lagoslakeid)               
suppressWarnings(dt4$lagosne_lagoslakeid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosne_lagoslakeid))==as.character(as.numeric("NA"))),NA,dt4$lagosne_lagoslakeid))
dt4$lagosne_legacysiteid <- as.factor(ifelse((trimws(as.character(dt4$lagosne_legacysiteid))==trimws("NA")),NA,as.character(dt4$lagosne_legacysiteid)))
dt4$nla2007_siteid <- as.factor(ifelse((trimws(as.character(dt4$nla2007_siteid))==trimws("NA")),NA,as.character(dt4$nla2007_siteid)))
dt4$nla2012_siteid <- as.factor(ifelse((trimws(as.character(dt4$nla2012_siteid))==trimws("NA")),NA,as.character(dt4$nla2012_siteid)))
dt4$nhdplusv2_lakes_n <- ifelse((trimws(as.character(dt4$nhdplusv2_lakes_n))==trimws("NA")),NA,dt4$nhdplusv2_lakes_n)               
suppressWarnings(dt4$nhdplusv2_lakes_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$nhdplusv2_lakes_n))==as.character(as.numeric("NA"))),NA,dt4$nhdplusv2_lakes_n))
dt4$lagosne_lakes_n <- ifelse((trimws(as.character(dt4$lagosne_lakes_n))==trimws("NA")),NA,dt4$lagosne_lakes_n)               
suppressWarnings(dt4$lagosne_lakes_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosne_lakes_n))==as.character(as.numeric("NA"))),NA,dt4$lagosne_lakes_n))
dt4$wqp_sites_n <- ifelse((trimws(as.character(dt4$wqp_sites_n))==trimws("NA")),NA,dt4$wqp_sites_n)               
suppressWarnings(dt4$wqp_sites_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$wqp_sites_n))==as.character(as.numeric("NA"))),NA,dt4$wqp_sites_n))
dt4$lagosus_legacyids_n <- ifelse((trimws(as.character(dt4$lagosus_legacyids_n))==trimws("NA")),NA,dt4$lagosus_legacyids_n)               
suppressWarnings(dt4$lagosus_legacyids_n <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$lagosus_legacyids_n))==as.character(as.numeric("NA"))),NA,dt4$lagosus_legacyids_n))


locus_link <- dt4
rm(dt4)
