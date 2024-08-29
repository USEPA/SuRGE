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

