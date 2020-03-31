library(sf) # for spatial data
library(tidyverse) # dplyr, ggplot
library(reshape2)
library(readxl) # read_excel
library(USAboundaries) # for states map
library(spsurvey) # for GRTS design
library(gridExtra) # multiple plots in one panel

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# ENFORCE EPA FORMAT ON NLA NAMES
toEPA <- function(X1){
  names(X1) = tolower(names(X1))
  names(X1) = gsub(pattern = "_", replacement = ".", x = names(X1))
  X1
}
