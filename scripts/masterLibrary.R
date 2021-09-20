# 3.6.2
library(sf)
library(tidyverse)
library(scales) # for ggplot2 datetime formatting 
library(plotly) # interactive plots (readLgr.R)
library(spsurvey) # lake design
library(leaflet) # for lake design printables
library(mapview) # for lake design printables

library(conflicted)
conflict_prefer("select", "dplyr") # select() will call dplyr::select()
conflict_prefer("filter", "dplyr") # filter() will call dplyr::filter()
