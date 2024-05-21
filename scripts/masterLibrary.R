# 3.6.2 -> 4.4.0
library(devtools) # this was needed to install LAGOSUS and hydrolinkgs from github.  Keeping this library call so
                  # the package is captured by renv in case it is needed for fresh clones.
                  # might not be necessary
library(sf)
library(tidyverse)
library(readxl)
library(janitor) # format dataframe names
library(scales) # for ggplot2 datetime formatting 
library(plotly) # interactive plots (readLgr.R)
library(spsurvey) # lake design. originally used 4.1.2 on R3.6.2.  Upgraded to R 4.4, not tested yet
library(leaflet) # for lake design printables
library(mapview) # for lake design printables
library(tictoc) # timing operations
library(gridExtra) # grid.arrange() for multiple panels per page on .pdf
library(lubridate) #for adjusting time offsets in readLGR
library(minpack.lm) #for the exponential modeling of diffusive flux
library(dttr2) # NA_Date_
library(LAGOSUS)
library(hydrolinks)


library(conflicted)
conflicted::conflict_scout()
conflict_prefer("select", "dplyr") # select() will call dplyr::select()
conflict_prefer("filter", "dplyr") # filter() will call dplyr::filter()
conflict_prefer("rename", "dplyr") # filter() will call dplyr::rename()
