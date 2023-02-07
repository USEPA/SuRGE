# SCRIPT TO PERFORM A QUICK PREVIEW OF LGR GHG DATA 

# LIBRARIES---------------
# library(ggplot2) # load from masterLibrary
# library(scales)  # load from masterLibrary
# source("ohio2016/scriptsAndRmd/masterLibrary.R")


# READ DATA -----------------
# List of .txt files containing data 
labs <- c("CIN", "RTP", "NAR", "USGS", "ADA", "R10", "DOE") # data directory for each lab
txtFiles <- character(0) # vector to catch file names
for (i in 1:length(labs)) {
  txtFiles.i <- list.files(paste0(userPath,"data/", labs[i]), 
                           pattern=c("gga|micro"), # per B.3.5.2, files should contain 'gga' or 'micro'
                           recursive = TRUE)
  # append new file paths to txtFiles
  txtFiles <- c(txtFiles, 
                paste0("data/", labs[i], "/", txtFiles.i)) # add "data/lab" to file paths 
}

# Directories contain _s, _l, and _b files that don't contain data of interest.
# Strip these files out.
txtFiles <- txtFiles[grepl(pattern = c("_f|-f"), x = txtFiles) & # grab only lgr files with data we need )should be _f, but allowing -f)
                       !grepl(pattern = "zip", x = txtFiles) & # exclude .zip files
                       !grepl(pattern = "Needs to be organized", x = txtFiles) & # temp file to be deleted
                       !grepl(pattern = "MGGA Archive and Calibration", x= txtFiles) &
                       !grepl(pattern = "2022 field season", x= txtFiles)] # CIN folder that will be deleted

ggaList <- list()  # Empty list to hold results

tic() # 50 seconds 2/6/2023
for (i in 1:length(txtFiles)) {  # loop to read and format each file
  print(i)
  if (grepl(pattern = "gga", x = txtFiles[i])) { 
    # I think this will work for all UGGA files.  The colClasses argument skips the final 71 columns of data.
    # this is needed because one analyzer produces empty columns, while the other doesn't.  This will throw
    # warning message for smaller file, but that is ok.
    gga.i <- read.table(paste(userPath, txtFiles[i], sep=""),
                        sep=",",  # comma separate
                        quote="\"",
                        skip=1,  # Skip first line of file.  Header info
                        # colClasses = c("character", rep("numeric", 25), rep("character", 2)),
                        as.is=TRUE, # Prevent conversion to factor
                        header=TRUE, # Import column names
                        fill=TRUE) %>% # Needed to deal with empty cells in last column
      # assign data to particular field crew
      mutate(lab = str_split(txtFiles[i], "/")[[1]][2], # extract 2nd element from 1st list element 
             # extract lake_id 
             # sub("(.*_)(\\d+)_.+", "\\2", txtFiles[i]) works for most, but not R10 2018 lakes.  below is more general 
             # case_when accomodates the inclusion of lacustrine, transitional, and riverine where needed.
             # code also forces lake_id to be character
             lake_id = case_when(grepl("lacustrine", txtFiles[i]) ~ 
                                   paste0(as.numeric(strsplit(txtFiles[i], "_")[[1]][2]), "_lacustrine"),
                                 grepl("transitional", txtFiles[i]) ~ 
                                   paste0(as.numeric(strsplit(txtFiles[i], "_")[[1]][2]), "_transitional"),
                                 grepl("riverine", txtFiles[i]) ~ 
                                   paste0(as.numeric(strsplit(txtFiles[i], "_")[[1]][2]), "_riverine"),
                                 TRUE ~ as.character(as.numeric(strsplit(txtFiles[i], "_")[[1]][2])))
      )
  }
  
  # MGGA FORMAT
  if (grepl(pattern = "micro", x = txtFiles[i])) { 
    # slightly different colClasses values.
    gga.i <- read.table(paste0(userPath, txtFiles[i]),
                        sep=",",  # comma separate
                        skip=1,  # Skip first line of file.  Header info
                        #colClasses = c(rep("character", 2), rep("numeric", 31)),  # needed to comment out for DOE
                        as.is=TRUE, # Prevent conversion to factor
                        header=TRUE, # Import column names
                        fill=TRUE) %>%
      # assign data to particular field crew
      mutate(lab = str_split(txtFiles[i], "/")[[1]][2], # extract 2nd element from 1st list element 
             # extract lake_id 
             # sub("(.*_)(\\d+)_.+", "\\2", txtFiles[i]) works for most, but not R10 2018 lakes.  below is more general 
             # case_when accomodates the inclusion of lacustrine, transitional, and riverine where needed.
             # code also forces lake_id to be character
             lake_id = case_when(grepl("lacustrine", txtFiles[i]) ~ 
                                   paste0(as.numeric(strsplit(txtFiles[i], "_")[[1]][2]), "_lacustrine"),
                                 grepl("transitional", txtFiles[i]) ~ 
                                   paste0(as.numeric(strsplit(txtFiles[i], "_")[[1]][2]), "_transitional"),
                                 grepl("riverine", txtFiles[i]) ~ 
                                   paste0(as.numeric(strsplit(txtFiles[i], "_")[[1]][2]), "_riverine"),
                                 TRUE ~ as.character(as.numeric(strsplit(txtFiles[i], "_")[[1]][2])))
      )
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
  gga.i <- select(gga.i, lab, lake_id, RDate, RDateTime, CH4._ppm, CO2._ppm, GasT_C)  # select columns of interest
  
  ggaList[[i]] <- gga.i  # dump in list
}  # End of loop
toc()

# Merge files
gga <- do.call("rbind", ggaList)  %>% # Coerces list into dataframe.
  filter(CH4._ppm < 500) # filter out clearly erroneous values


# write to disk
save(gga, file = "output/gga.RData")

# BASIC PLOTS-----------------
ggplot(gga, aes(RDateTime, CH4._ppm)) + geom_point() +
  scale_x_datetime(labels=date_format ("%m/%d %H:%M")) + 
  facet_wrap(~lab + lake_id, scales = "free", 
             labeller = label_wrap_gen(multi_line=FALSE)) # facet labels in same row
 
# ggsave("output/figures/ch4profile.tiff")

ggplot(gga, aes(RDateTime, CO2._ppm)) + geom_point() +
  scale_x_datetime(labels=date_format ("%m/%d %H:%M")) +
  facet_wrap(~lab + lake_id, scales = "free", 
             labeller = label_wrap_gen(multi_line=FALSE)) # facet labels in same row

# ggsave("output/figures/co2profile.tiff")


# Try an interactive version for each lake
plotCh4 <- gga %>% filter(lake_id == "045", CH4._ppm > 0) %>%
  ggplot(aes(RDateTime, CH4._ppm)) + geom_point() +
  scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
  ggtitle("045")
ggplotly(plotCh4)  
  
plotCo2 <- gga %>% filter(lake_id == "010") %>%
  ggplot(aes(RDateTime, CO2._ppm)) + geom_point() +
  scale_x_datetime(labels=date_format ("%m/%d %H:%M")) +
  ggtitle("045")
ggplotly(plotCo2)  

