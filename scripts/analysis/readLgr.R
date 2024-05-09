# SCRIPT TO PERFORM A QUICK PREVIEW OF LGR GHG DATA 

# LIBRARIES---------------
# library(ggplot2) # load from masterLibrary
# library(scales)  # load from masterLibrary
# library(lubridate) 
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
txtFiles <- txtFiles[grepl(pattern = c("_f|-f"), x = txtFiles) & # grab only lgr files with data we need; should be _f, but allowing -f)
                       !grepl(pattern = "zip", x = txtFiles) & # exclude .zip files
                       !grepl(pattern = "xls", x = txtFiles) & # exclude excel files
                       !grepl(pattern = "Needs to be organized", x = txtFiles) & # temp file to be deleted
                       !grepl(pattern = "MGGA Archive and Calibration", x= txtFiles) &
                       !grepl(pattern = "2022 field season", x= txtFiles) & # CIN folder that will be deleted
                       !grepl(pattern = "SuRGE chamber data calculations SAJ", x= txtFiles)] # Scott's folder

#Check that you have txtFiles for all lakes
chk <- str_split (txtFiles, "/")
ck2<-matrix(unlist(chk),ncol=5,byrow=T)
laken<-unique(substr(ck2[,3],5,7))
lab<-unique(ck2[,2])

ggaList <- list()  # Empty list to hold results

tic() # 25 seconds 9/28/2023
for (i in 1:length(txtFiles)) {  # loop to read and format each file #length(txtFiles) 92 is tab delimited.  must convert to comma
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
             visit= ifelse(grepl("visit2",str_split(txtFiles[i],"/")[[1]][3]),"2","1"),
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
         gga.i <- read.table(paste0(userPath, txtFiles[i]),
                          sep=",",  # comma separate
                          skip=1,  # Skip first line of file.  Header info
                          #colClasses = c(rep("character", 2), rep("numeric", 31)),  # needed to comment out for DOE
                          as.is=TRUE, # Prevent conversion to factor
                          header=TRUE, # Import column names
                          fill=TRUE) %>% 
    # assign data to particular field crew
      mutate(lab = str_split(txtFiles[i], "/")[[1]][2], # extract 2nd element from 1st list element 
             visit= ifelse(grepl("visit2",str_split(txtFiles[i],"/")[[1]][3]),"2","1"),
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
  gga.i$RDate <- as.Date(gga.i$Date, format = "%m/%d/%Y")  # format as R Date object
  names(gga.i)[grep("ppm", names(gga.i))] = gsub("^X.", "", names(gga.i)[grep("X", names(gga.i))]) # replace "X." with ""
  gga.i <- gga.i %>% select(lab, visit, lake_id, RDate, RDateTime, CH4.d_ppm, CO2.d_ppm, H2O._ppm, GasT_C) %>% # select columns of interest
    rename(CH4._ppm = CH4.d_ppm, CO2._ppm = CO2.d_ppm)

  
  ggaList[[i]] <- gga.i  # dump in list
}  # End of loop
toc()

# Merge files
gga <- do.call("rbind", ggaList)  %>% # Coerces list into dataframe.
  filter(CH4._ppm < 500) # filter out clearly erroneous values

#Pull in one more gga file that has same exact times from same lake with broken clock after lunch (232)
fix <- read.table(paste(userPath, "data/CIN/CH4_232_Sheyenne/GGA/fga_2001-12-31_f0031.txt" , sep=""),
                  sep=",",  # comma separate
                  quote="\"",
                  skip=1,  # Skip first line of file.  Header info
                  # colClasses = c("character", rep("numeric", 25), rep("character", 2)),
                  as.is=TRUE, # Prevent conversion to factor
                  header=TRUE, # Import column names
                  fill=TRUE) %>% # Needed to deal with empty cells in last column
  # assign data to particular field crew
  mutate(lab="CIN",visit="1",lake_id = "232b")#label this b for now to distinguish from first gga file from same lake with broken clock


fix$Time <- gsub("^\\s+|\\s+$", "", fix$Time)  #  Strip white spaces
fix$Date <- substr(fix$Time, start=1, stop=10)  # Extract date
fix$Second <- round(  # extract second, round to integer
  as.numeric(
    substr(fix$Time, start=nchar(fix$Time) - 5, stop=nchar(fix$Time))
  ), 
  digits=0)
fix$Second <- ifelse(fix$Second == 60, 59, fix$Second)  # POSIXcr can't handle 60 seconds
fix$hms <- paste(substr(fix$Time, start=12, stop=17), fix$Second, sep="")  # time vector
fix$RDateTime <- as.POSIXct(paste(fix$Date, fix$hms,sep=""),
                              format="%m/%d/%Y%H:%M:%S",
                              tz = "UTC")  # POSIXct
fix$RDate <- as.Date(fix$Date, format = "%m/%d/%Y")  # format as R Date object
names(fix)[grep("ppm", names(fix))] = gsub("^X.", "", names(fix)[grep("X", names(fix))]) # replace "X." with ""
fix <- fix %>% select(lab, visit, lake_id, RDate, RDateTime, CH4.d_ppm, CO2.d_ppm, H2O._ppm, GasT_C) %>% # select columns of interest
  rename(CH4._ppm = CH4.d_ppm, CO2._ppm = CO2.d_ppm)

gga<-rbind(gga,fix)

# FIX DATES------
# summer 2021 MGGA internal battery died, causing date to default to 2001-12-31
# load .csv file with records from CIN lab 

CIN_adjustments<-read.csv(paste0(userPath,"data/CIN/Time_Adjustments.csv"))
CIN_adjustments$wrong.datetime<-as.POSIXct(CIN_adjustments$wrong.datetime,format="%m/%d/%Y %H:%M",tz = "UTC")
CIN_adjustments$right.datetime<-as.POSIXct(CIN_adjustments$right.datetime,format="%m/%d/%Y %H:%M",tz = "UTC")
CIN_adjustments$TO<-difftime(CIN_adjustments$right.datetime, CIN_adjustments$wrong.datetime, tz="UTC",
                                      units =  "mins")
CIN_adjustments$Time.Offset<-time_length(CIN_adjustments$TO,unit="second")

gga$RDateTime_adj<-ifelse(gga$lake_id=="67",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[1]),
                      ifelse(gga$lake_id=="68",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[2]),
                             ifelse(gga$lake_id=="69_riverine",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[3]),
                                    ifelse(gga$lake_id=="70_transitional",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[4]),
                                           ifelse(gga$lake_id=="71",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[5]),
                                                  ifelse(gga$lake_id=="72",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[6]),
                                                      ifelse(gga$lake_id=="75",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[7]),
                                                             ifelse(gga$lake_id=="79",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[8]),
                                                                    ifelse(gga$lake_id=="149",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[9]),
                                                                           ifelse(gga$lake_id=="231",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[10]),
                                                                                  ifelse(gga$lake_id=="232",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[11]),
                                                                                         ifelse(gga$lake_id=="232b",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[15]),
                                                                                         ifelse(gga$lake_id=="236",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[12]),
                                                                                                ifelse(gga$lake_id=="237",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[13]),
                                                                                                       ifelse(gga$lake_id=="69_lacustrine",gga$RDateTime+dseconds(CIN_adjustments$Time.Offset[14]),gga$RDateTime)))))))))))))))

gga$RDateTime<-as_datetime(gga$RDateTime_adj)

gga$lake_id<-ifelse(gga$lake_id=="232b","232",gga$lake_id) #collapse the two sets of data from lake 232 back together now that they can be distinguished

# BASIC PLOTS-----------------
# ggplot(gga, aes(RDateTime, CH4._ppm)) + geom_point() +
#   scale_x_datetime(labels=date_format ("%m/%d %H:%M")) + 
#   facet_wrap(~lab + lake_id, scales = "free", 
#              labeller = label_wrap_gen(multi_line=FALSE)) # facet labels in same row
#  
# # ggsave("output/figures/ch4profile.tiff")
# 
# ggplot(gga, aes(RDateTime, CO2._ppm)) + geom_point() +
#   scale_x_datetime(labels=date_format ("%m/%d %H:%M")) +
#   facet_wrap(~lab + lake_id, scales = "free", 
#              labeller = label_wrap_gen(multi_line=FALSE)) # facet labels in same row
# 
# # ggsave("output/figures/co2profile.tiff")
# 
# 
# # Try an interactive version for each lake
# plotCh4 <- gga %>% filter(lake_id == "045", CH4._ppm > 0) %>%
#   ggplot(aes(RDateTime, CH4._ppm)) + geom_point() +
#   scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
#   ggtitle("045")
# ggplotly(plotCh4)  
#   
# plotCo2 <- gga %>% filter(lake_id == "010") %>%
#   ggplot(aes(RDateTime, CO2._ppm)) + geom_point() +
#   scale_x_datetime(labels=date_format ("%m/%d %H:%M")) +
#   ggtitle("045")
# ggplotly(plotCo2)  

