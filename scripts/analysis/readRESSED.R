## Link SuRGE Lakes to RESSED
## August 23, 2024

dbq_string <- paste0("DBQ=", paste0(userPath, "data/siteDescriptors/"),"RESSED_v1.2.mdb")
driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
db_connect_string <- paste0(driver_string, dbq_string)

con <- odbcDriverConnect(db_connect_string)
