# 3.6.2 -> 4.4.0
library(devtools) # this was needed to install LAGOSUS and hydrolinkgs from github.  Keeping this library call so
                  # the package is captured by renv in case it is needed for fresh clones.
                  # might not be necessary
library(sf)
library(elevatr) # fro Digital Elevation Maps
library(raster) # working with rasters
library(terra) # working with rasters
library(tidyverse)
library(readxl)
library(janitor) # format dataframe names
library(stringi) # character manipulation
library(scales) # for ggplot2 datetime formatting 
library(lutz) # time zones from coordinates
library(plotly) # interactive plots (readLgr.R)
library(spsurvey) # lake design. originally used 4.1.2 on R3.6.2.  Upgraded to R 4.4, and spSurvey 5.5.1
library(leaflet) # for lake design printables
library(mapview) # for lake design printables
library(tmap) # mapping
library(tigris) # get states data. Alternative to USABoundaries
library(mapsf) # map with Puerto Rico inset
library(tictoc) # timing operations
library(gridExtra) # grid.arrange() for multiple panels per page on .pdf
library(lubridate) #for adjusting time offsets in readLGR
library(minpack.lm) #for the exponential modeling of diffusive flux
library(dttr2) # NA_Date_
library(LAGOSUS)
#library(hydrolinks)
library(corrplot)
library(StreamCatTools) # read lakeCat
library(jtools) # visualize regression models (effect_plot)
#library(StepReg) # stepwise selection based on p-value (`stepwise`)
library(cowplot) # arranging ggplot plots into grid
library(ggh4x) # ggplot2 hacks
library(ggallin) # pseudolog10_trans
library(httr) #this is needed for downloading Lagos trophic status data
library(RODBC) #RESSED
library(dataRetrieval) #NWIS data retrieval for water levels
library(rLakeAnalyzer) #for center buoyancy and thermocline depth

library(conflicted)
conflicted::conflict_scout()
conflict_prefer("select", "dplyr") # select() will call dplyr::select()
conflict_prefer("filter", "dplyr") # filter() will call dplyr::filter()
conflict_prefer("rename", "dplyr") # filter() will call dplyr::rename()
