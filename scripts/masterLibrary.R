library(sf) # for spatial data
library(tidyverse) # dplyr, ggplot
library(reshape2)
library(readxl) # read_excel
library(USAboundaries) # for states map
library(spsurvey) # for GRTS design

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
