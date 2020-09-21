# SCRIPT TO PERFORM A QUICK PREVIEW OF LGR GHG DATA 

# LIBRARIES---------------
# library(ggplot2) # load from masterLibrary
# library(scales)  # load from masterLibrary
# source("ohio2016/scriptsAndRmd/masterLibrary.R")


# READ DATA -----------------
# List of .txt files containing data from 2016 survey
txtFiles <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/data/greenhouseGasAnalyzer/", 
                       pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

# Directories contain _s, _l, and _b files that don't contain data of interest.
# Strip these files out.
txtFiles <- txtFiles[!grepl(pattern = "_s|_l|_b", x = txtFiles)] # exclude files with _l or _s or _b

ggaList <- list()  # Empty list to hold results

for (i in 1:length(txtFiles)) {  # loop to read and format each file
  if (!grepl(pattern = "2017", x = txtFiles[i])) {
  gga.i <- read.table(paste("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/data/greenhouseGasAnalyzer/", 
                            txtFiles[i], sep=""),
                      sep=",",  # comma separate
                      skip=1,  # Skip first line of file.  Header info
                      colClasses = c("character", rep("numeric", 21), rep("NULL", 71)),
                      as.is=TRUE, # Prevent conversion to factor
                      header=TRUE, # Import column names
                      fill=TRUE)  # Needed to deal with empty cells in last column
  }
  
  if (grepl(pattern = "2017", x = txtFiles[i])) {
    gga.i <- read.table(paste("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/multiResSurvey2016/data/greenhouseGasAnalyzer/", 
                              txtFiles[i], sep=""),
                        sep=",",  # comma separate
                        skip=1,  # Skip first line of file.  Header info
                        colClasses = c("character", rep("numeric", 21), rep("NULL", 6)),
                        as.is=TRUE, # Prevent conversion to factor
                        header=TRUE, # Import column names
                        fill=TRUE) 
  }
  # FORMAT DATA
# gga.i <- gga.i[1:(which(gga.i$Time == "-----BEGIN PGP MESSAGE-----") - 1), ]  # Remove PGP message
  gga.i$Time <- gsub("^\\s+|\\s+$", "", gga.i$Time)  #  Strip white spaces
  gga.i$Date <- substr(gga.i$Time, start=1, stop=10)  # Extract date
  gga.i$Second <- round(  # extract second, round to integer
    as.numeric(
      substr(gga.i$Time, start=nchar(gga.i$Time) - 5, stop=nchar(gga.i$Time))
    ), 
    digits=0)
  gga.i$Second <- ifelse(gga.i$Second == 60, 59, gga.i$Second)  # POSIXcr can't handle 60 seconds
  gga.i$hms <- paste(substr(gga.i$Time, start=12, stop=17), gga.i$Second, sep="")  # time vector
  gga.i$RDateTime <- as.POSIXct(paste(gga.i$Date, gga.i$hms,sep=""),
                                format="%m/%d/%Y%H:%M:%S",
                                tz = "UTC")  # POSIXct
  gga.i$RDate <- as.Date(gga.i$Date, format = "%m/%d/%Y")  # format as R Date oject
  names(gga.i)[grep("ppm", names(gga.i))] = gsub("^X.", "", names(gga.i)[grep("X", names(gga.i))]) # replace "X." with ""
  gga.i <- select(gga.i, RDate, RDateTime, CH4._ppm, CO2._ppm, GasT_C)  # select columns of interest
  
  ggaList[[i]] <- gga.i  # dump in list
}  # End of loop, < 1 minute

# Merge files
gga <- do.call("rbind", ggaList)  # Coerces list into dataframe.



# Merge in floating chamber data from Cowan Lake where manual samples
# were collected because water was sucked into LGR.
# Need to format file and add time stamp from field data sheets.
cowanChmTimes <- c(16101, "2016-06-03 13:33:00",
                   16102, "2016-06-03 13:36:00",
                   16103, "2016-06-03 13:39:10",
                   16104, "2016-06-03 13:42:26",
                   16115, "2016-06-03 14:19:00",
                   16116, "2016-06-03 14:22:10",
                   16117, "2016-06-03 14:25:30",
                   16118, "2016-06-03 14:28:00",
                   16108, "2016-06-03 13:59:00",
                   16109, "2016-06-03 14:02:00",
                   16110, "2016-06-03 14:05:46",
                   16111, "2016-06-03 14:08:00",
                   16119, "2016-06-03 14:40:00",
                   16120, "2016-06-03 14:43:40",
                   16121, "2016-06-03 14:46:00",
                   16122, "2016-06-03 14:49:11",
                   16126, "2016-06-03 15:00:00",
                   16127, "2016-06-03 15:03:29",
                   16128, "2016-06-03 15:06:04",
                   16129, "2016-06-03 15:09:08",
                   16133, "2016-06-03 15:18:00",
                   16134, "2016-06-03 15:21:00",
                   16135, "2016-06-03 15:24:14",
                   16136, "2016-06-03 15:27:01",
                   16094, "2016-06-03 13:19:00",
                   16095, "2016-06-03 13:23:16",
                   16096, "2016-06-03 13:25:19",
                   16097, "2016-06-03 13:28:10")

cowanChmTimesDf <- data.frame(RDateTime = 
                              as.POSIXct(cowanChmTimes[seq(2, length(cowanChmTimes), by = 2)],
                                         origin = "1970-01-01 00:00:00", tz= "UTC"),
                            sample = 
                              cowanChmTimes[seq(1, length(cowanChmTimes), by = 2)],
                            RDate = as.Date(
                              substr(
                                cowanChmTimes[seq(2, length(cowanChmTimes), by = 2)],
                                start = 1, stop = 10)),
                            GasT_C = 27.8) # from weatherunderground for Wilmington 

# Merge times with gas concentration
cowanChmTimesGas <- merge(cowanChm, cowanChmTimesDf) %>%
  select(-sample, -n2o.ppm) %>%
  rename(CH4._ppm = ch4.ppm, CO2._ppm = co2.ppm)

# Merge GC chamber data from Cowan with gga data
gga <- rbind(gga, cowanChmTimesGas) # rbind is smart enough to match col names



# BASIC PLOTS-----------------
# ggplot(gga, aes(RDateTime, CH4._ppm)) + geom_point() +
#   scale_x_datetime(labels=date_format ("%m/%d %H:%M"))
# 
# ggplot(gga, aes(RDateTime, CO2._ppm)) + geom_point() +
#   scale_x_datetime(labels=date_format ("%m/%d %H:%M"))


