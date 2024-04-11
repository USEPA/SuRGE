# define data subset
site.lake.visit.i <- unique(paste(gga_4$site_id, gga_4$lake_id, gga_4$visit))[i]

# extract unique site, lake, and visit using stringr::
site.i <- stringr::word(site.lake.visit.i, 1) %>% as.numeric() # extract characters before space.  site_id is numeric.
lake.i <- stringr::word(site.lake.visit.i, 2) # extract characters after space. lake_id is character  
visit.i <- stringr::word(site.lake.visit.i, 3) # extract characters after space. lake_id is character 


# extract unique site, lake, and visit using gsub
# https://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
site.i <- gsub(" .*$", "", site.lake.visit.i) %>% as.numeric()# A space (), then any character (.) any number of times (*) until the end of the string ($).
# https://stackoverflow.com/questions/72899184/r-extracting-after-first-space
lake.i <- gsub(" [^ ]+$|^.*? ", "", site.lake.visit.i) # Remove the first/last space and everything before/after it
visit.i <- gsub(".*? ", "", site.lake.visit.i) #Remove the first space and everything before it


# compare speed
library(microbenchmark)
# gsub is 8 times faster
microbenchmark(site.i <- stringr::word(site.lake.visit.i, 1) %>% as.numeric(),
               site.i <- gsub(" .*$", "", site.lake.visit.i) %>% as.numeric())

# gsub is 6 times faster
microbenchmark(lake.i <- stringr::word(site.lake.visit.i, 2),
               lake.i <- gsub(" [^ ]+$|^.*? ", "", site.lake.visit.i))

# gsub is 10 times faster
microbenchmark(visit.i <- stringr::word(site.lake.visit.i, 3),
               visit.i <- gsub(".*? ", "", site.lake.visit.i))
