# READ LAKE SURVEY DESIGNS


# 1. Create a list of file paths where the data are stored.  
labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS", "PR")
paths <- paste0(userPath,  "lakeDsn/", labs)


# Function for reading survey design files.

get_dsn <- function(paths){
  #d <-  
  fs::dir_ls(path = paths, # see above
             regexp = ".gpkg", # file names containing this pattern
             recurse = TRUE) %>% # look in all subdirectories
    .[grepl("merc", ., ignore.case = TRUE)] %>% # arbitrarily grab files in merc projection (duplicated in WGS)
    .[!grepl("merc326low", .)] %>% # used merc326high
    .[!grepl("lowMerc287", .)] %>% # used originalMerc287
    .[!grepl(c("originalMerc265|highMerc265"), .)] %>% # used lowMerc265
    .[!grepl(c("homeMerc249|highMerc249|lowMerc249|originalMerc249"), .)] %>% # used midMerc249
    .[!grepl("merc188.gpkg", .)] %>% # used merc188high.gpkg
    .[!grepl(c("xxx|mercharsha"), .)] %>% # omit some randos
    # exclude designs not used (e.g. landowner denial)
    .[!grepl(c("merc015|merc183|merc194|merc212|merc213|merc056|merc185|merc318|merc325|merc009|merc244|merc285"), .)] %>%
    #.[grepl(c("069|070"), .)] %>% # troubleshooting Oahe and Francis Case
    #.[grepl(c("239"), .)] %>% # troubleshooting 2018 R10
    #.[1:5] %>%
    # map will read each file in fs_path list generated above
    purrr::map(~st_layers(.x)$name) %>% # get names of all gpkg layers
    purrr::map(~.x[grepl("allSites", .x, ignore.case = TRUE) & !grepl("buffer", .x)]) %>% # get layer with all sites
    purrr::imap(~st_read(.y, layer = .x)) %>% # read .gpkg (captured as list element name (.y)) and specified layer (.x)
    purrr::imap(~st_drop_geometry(.x) %>% # omit spatial
                  as_tibble(.) %>%
                  janitor::clean_names(.) %>% # surge convention
                  # format lake and site id values
                  mutate(lake_id = case_when(grepl("070transitional", .y) ~ "70_transitional",
                                             grepl("070riverine", .y) ~ "70_riverine",
                                             grepl("070lacustrine", .y) ~ "70_lacustrine",
                                             grepl("069transitional", .y) ~ "69_transitional",
                                             grepl("069riverine", .y) ~ "69_riverine",
                                             grepl("069lacustrine", .y) ~ "69_lacustrine",
                                             TRUE ~ as.character(lake_site_id)) %>%
                           tolower(.) %>% # i.e. CH4 -> ch4
                           str_remove(., "ch4-") %>% # remove any ch4_ from lake_id
                           str_remove(., "^0+"), #remove leading zeroes i.e. 078->78
                         site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
                  # if present, select, doesn't throw error if not present
                  select_if(names(.) %in% c("lake_id", "site_id", "section", "stratum", "panel", "wgt")) %>%
                  # if present, rename to specify design details are specific to the sites within a lake,
                  # as opposed to the overall SuRGE design where details are specific to a lake
                  rename_if(names(.) %in% c("stratum", "section", "panel", "wgt"), ~paste0("site_", .))) %>% 
    map_dfr(., bind_rows) # rbinds into one df
}

# read lake designs
lake_dsn <- get_dsn(paths)

# 1033 Falls Lake
dsn_1033 <- st_read(paste0(userPath, "lakeDsn/2016_survey/fallsLake/fallsLakeSitesEqArea.shp")) %>%  
  st_drop_geometry %>% # omit spatial
  as_tibble %>% 
  janitor::clean_names(.) %>% # surge convention
  # format lake and site id values
  mutate(lake_id = "1033",
         site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>%
  rename(section = mdcaty) %>%
  # if present, select, doesn't throw error if not present
  select_if(names(.) %in% c("lake_id", "site_id", "section", "stratum", "panel", "wgt")) %>%
  rename_if(names(.) %in% c("stratum", "section", "panel", "wgt"), ~paste0("site_", .)) 

# bind falls lakes and all others
lake_dsn <- rbind(lake_dsn, dsn_1033)


# unique lake_id in lake designs
lake_dsn_ids <- lake_dsn %>%
  distinct(lake_id)

# any lake_id values in lake designs not in exhaustive list of surge lakes?
lake_dsn_ids %>% filter(!(lake_id %in% unique(lake.list.all$lake_id))) # none, good

# any lake_id values in exhaustive list of surge lakes not in design files?
# only 2016 sites. Adjusted weights are in dat_2016.
unique(lake.list.all$lake_id)[!(unique(lake.list.all$lake_id) %in% lake_dsn_ids$lake_id)] #>=1001 (all 2016)
