# SCRIPT TO PERFORM A QUICK PREVIEW OF LGR GHG DATA 

# LIBRARIES---------------
# library(ggplot2) # load from masterLibrary
# library(scales)  # load from masterLibrary
# source("ohio2016/scriptsAndRmd/masterLibrary.R")


# READ DATA -----------------
# List of .txt files containing data 
txtFiles <- list.files(paste0(userPath,"data/ADA"), 
                       pattern=c("gga|micro"), recursive = TRUE) # per B.3.5.1, files should contain 'gga' or 'micro'

# Directories contain _s, _l, and _b files that don't contain data of interest.
# Strip these files out.
txtFiles <- txtFiles[grepl(pattern = "_f", x = txtFiles) & # grab only lgr files with data we need
                       !grepl(pattern = "zip", x = txtFiles) & # exclude .zip files
                       !grepl(pattern = "Needs to be organized", x = txtFiles)] # temp file to be deleted

ggaList <- list()  # Empty list to hold results

for (i in 1:length(txtFiles)) {  # loop to read and format each file
  
  if (grepl(pattern = "gga", x = txtFiles[i])) { 
    # I think this will work for all UGGA files.  The colClasses argument skips the final 71 columns of data.
    # this is needed because one analyzer produces empty columns, while the other doesn't.  This will throw
    # warning message for smaller file, but that is ok.
  gga.i <- read.table(paste("../../../data/", 
                            txtFiles[i], sep=""),
                      sep=",",  # comma separate
                      skip=1,  # Skip first line of file.  Header info
                      colClasses = c("character", rep("numeric", 21), rep("NULL", 71)),
                      as.is=TRUE, # Prevent conversion to factor
                      header=TRUE, # Import column names
                      fill=TRUE) %>% # Needed to deal with empty cells in last column
  mutate(lab = sub("\\/.*", "", txtFiles[i]), # assign data to particular field crew
         # extract lakeId 
         # this works for most, but not R10 2018 lakes sub("(.*_)(\\d+)_.+", "\\2", txtFiles[i])
         lakeId = strsplit(txtFiles[i], "_")[[1]][2]) # split into list, grab second element
}
 

# MGGA FORMAT
if (grepl(pattern = "micro", x = txtFiles[i])) { 
  # slightly different colClasses values.
  gga.i <- read.table(paste0(userPath,"data/ADA/", 
                            txtFiles[i]),
                      sep=",",  # comma separate
                      skip=1,  # Skip first line of file.  Header info
                      colClasses = c(rep("character", 2), rep("numeric", 31)), 
                      as.is=TRUE, # Prevent conversion to factor
                      header=TRUE, # Import column names
                      fill=TRUE) %>%
    mutate(lab = sub("\\/.*", "", txtFiles[i]), # assign data to particular field crew
           lakeId = sub("(.*_)(\\d+)_.+", "\\2", txtFiles[i]))  # extract lakeId
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
  gga.i <- select(gga.i, lab, lakeId, RDate, RDateTime, CH4._ppm, CO2._ppm, GasT_C)  # select columns of interest
  
  ggaList[[i]] <- gga.i  # dump in list
}  # End of loop, < 1 minute

# Merge files
gga <- do.call("rbind", ggaList)  %>% # Coerces list into dataframe.
  filter(CH4._ppm < 500) # filter out clearly erroneous values




# BASIC PLOTS-----------------
ggplot(gga, aes(RDateTime, CH4._ppm)) + geom_point() +
  scale_x_datetime(labels=date_format ("%m/%d %H:%M")) + 
  facet_wrap(~lab + lakeId, scales = "free", 
             labeller = label_wrap_gen(multi_line=FALSE)) # facet labels in same row
 
ggsave("output/figures/ch4profile.tiff")

ggplot(gga, aes(RDateTime, CO2._ppm)) + geom_point() +
  scale_x_datetime(labels=date_format ("%m/%d %H:%M")) +
  facet_wrap(~lab + lakeId, scales = "free", 
             labeller = label_wrap_gen(multi_line=FALSE)) # facet labels in same row

ggsave("output/figures/co2profile.tiff")


# Try an interactive version for each lake
plotCh4 <- gga %>% filter(lakeId == "167") %>%
  ggplot(aes(RDateTime, CH4._ppm)) + geom_point() +
  scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
  ggtitle("167")
ggplotly(plotCh4)  
  
plotCo2 <- gga %>% filter(lakeId == "167") %>%
  ggplot(aes(RDateTime, CO2._ppm)) + geom_point() +
  scale_x_datetime(labels=date_format ("%m/%d %H:%M")) +
  ggtitle("167")
ggplotly(plotCo2)  

