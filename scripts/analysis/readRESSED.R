temp <- tempfile()
req <- ("https://water.usgs.gov/osw/ressed/download2013/ressed_export_20130404.json.zip")
ressed <- httr::GET(url = req)

ressed <- unz(temp, "ressed_export_20130404.json.zip")


link <- unzip("https://water.usgs.gov/osw/ressed/download2013/ressed_export_20130404.json.zip")

ressed <- jsonlite::fromJSON(paste0(userPath, "data/siteDescriptors/ressed_export_20130404.json"))

bind_rows(ressed$ressed$storage_type)
bind_rows(ressed$ressed$stat_def)
bind_rows(ressed$ressed$reservoir)
bind_rows(ressed$ressed$reservoir[c(1:26, 30:40, 45)])
bind_rows(ressed$ressed$reservoir[])
ressed_var <- names(ressed$ressed$reservoir)
ressed_var[29]


str(ressed$ressed$reservoir$stat)
ressed$ressed$reservoir$stat[988][]
str(ressed)

mdb_path <- paste0(userPath, "data/siteDescriptors/RESSED_v1.2.mdb")
Hmisc::mdb.get(mdb_path)


#### Libraries =================================================================
library(RODBC)
library(dplyr)
library(dbplyr)

#### Import database ===========================================================
# Define connection strings
dbq_string <- paste0("DBQ=", paste0(userPath, "data/siteDescriptors/"),"RESSED_v1.2.mdb")
driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
db_connect_string <- paste0(driver_string, dbq_string)

# Create .accdb connection
con <- odbcDriverConnect(db_connect_string)

# See Tables
sqlTables(con)

#### Extract tables to data.frames
sqlFetch(con, "Lat_Lon_New")
sqlFetch(con, "RSED06")
sqlFetch(con, "RSED09")
