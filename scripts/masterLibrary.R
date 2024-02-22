# 3.6.2
library(sf)
library(tidyverse)
library(readxl)
library(janitor) # format dataframe names
library(scales) # for ggplot2 datetime formatting 
library(plotly) # interactive plots (readLgr.R)
library(spsurvey) # lake design
library(leaflet) # for lake design printables
library(mapview) # for lake design printables
library(tictoc) # timing operations
library(gridExtra) # grid.arrange() for multiple panels per page on .pdf
library(lubridate) #for adjusting time offsets in readLGR

library(conflicted)
conflicted::conflict_scout()
conflict_prefer("select", "dplyr") # select() will call dplyr::select()
conflict_prefer("filter", "dplyr") # filter() will call dplyr::filter()
conflict_prefer("rename", "dplyr") # filter() will call dplyr::rename()
