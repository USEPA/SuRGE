# LOAD SURGE SPATIAL DATA---------------
# Load final design.  Design (NLA_Methane_Design_Lakes_20191206.shp) provided by
# Olsen.  I converted to .xlsx, added a few columns (e.g. laboratory), updated 
# with site status (e.g. land owner denial, oversample sites, etc), wrote out to .gpkg 
# (see surgeDsn/readSurgeDesign20191206.R), and converted to .gdb in GIS Pro.
# Here we read the .xlsx file and convert to spatial, but could also read from
# .gpkg or .gdb
# Read in data
surgeDsn <- readxl::read_xlsx("../../../surgeDsn/SuRGE_design_20191206_eval_status.xlsx") %>%
  select(-xcoord_1, -ycoord_1) %>% # remove xcoord and ycoord, holdover from Tony's .shp
  janitor::clean_names() %>% # GIS is picky about names
  filter(!(site_id %in% c("CH4-1033"))) %>% # exclude Falls Lake 
  mutate(study = case_when(sample_year == 2016 ~ "2016 Regional Study",
                           #sample_year == 2018 ~ "2018 Regional Study",
                           site_id %in% c("CH4-999", "CH4-1000") ~ "Hand picked",
                           TRUE ~ "SuRGE"))


# Convert to sf object
surgeDsn.sf <- st_as_sf(surgeDsn, coords = c("lon_dd83", "lat_dd83"), 
                        crs = 4269) %>% # NAD83
  st_transform(., crs = 5070) # Conus Albers

# Filter to sampled sites
surgeDsnSampled <- surgeDsn.sf %>%
  # filter to sampled sites
  filter(eval_status_code == "S")



# READ ECOREGION SHAPEFILE PROVIDED BY MARC WEBER ------------------
# Original shapefile provided by Marc Weber on 1/3/2017 in Albers.
# Simplified by Alex Hall.

ecoR <- st_read(dsn = "inputData/ecoregions",
                layer = "aggr_ecoregions_simple") %>%
  st_make_valid()

# Check CRS
st_crs(ecoR) # 3857
ecoR <- st_transform(ecoR, 5070) # convert to CONUS Albers
st_crs(ecoR) # 5070

# quick map test
ggplot(ecoR) +
  geom_sf(aes(fill = WSA9_NAME))


# SET UP CUSTOM COLORS FOR ECOREGIONS---------
# Custom color pallette for ecoregion polygons.  Attempted to mirror
# https://www.epa.gov/national-aquatic-resource-surveys/
# ecoregional-results-national-lakes-assessment-2012
ecoR <- left_join(ecoR,
                  tribble(~ WSA9_NAME, ~ cols,
                          "Coastal Plains", "orange1",
                          "Northern Appalachians", "lightpink1",
                          "Northern Plains", "darksalmon",
                          "Southern Appalachians", "mediumturquoise",
                          "Southern Plains", "khaki4",
                          "Temperate Plains", "forestgreen", 
                          "Upper Midwest", "deepskyblue4",
                          "Western Mountains", "saddlebrown",
                          "Xeric", "lightskyblue4"))




# RETRIVE AND PREPARE STATES MAP
states <- tigris::states(cb = TRUE) %>% # coarse resolution
  janitor::clean_names() %>%
  select(name) %>%
  # filter geographies not in SuRGE
  filter(!name %in% c("Commonwealth of the Northern Mariana Islands",
                      "Guam", "United States Virgin Islands", "American Samoa",
                      "Alaska", "Hawaii")) %>% # CONUS + PR
  st_transform(5070)

# State boundaries for overlay
sb <- states %>%
  st_cast("MULTILINESTRING")

# Intersect ecoR with states
states.eco <- st_intersection(states, st_make_valid(ecoR)) %>%
  st_collection_extract("POLYGON") %>%
  # no puerto rico in ecoR, so the intersection excludes this geometry.
  # add puerto rico back in
  bind_rows(.,
            states %>% 
              filter(name == "Puerto Rico") %>%
              mutate(cols = "white"))



# BREAK GEOGRAPHIES FOR MAPPING-------------
# define CONUS and PR geographies
# CONUS
conus <- states.eco %>% # state + climate polygons.  plots with color = NA to hide border lines
  filter(name != "Puerto Rico") 

conus.lines <- sb %>% # state border lines, no border lines for climate
  filter(name != "Puerto Rico")

conus.surge <- surgeDsnSampled %>%
  filter(site_id != "CH4-1000") # exclude Puerto Rico

# Puerto Rico
pr <- states.eco %>%
  filter(name == "Puerto Rico") %>%
  st_transform(crs = 32619)

pr.lines <- sb %>%
  filter(name == "Puerto Rico") %>%
  st_transform(crs = 32619) %>%
  st_cast("MULTILINESTRING")

pr.surge <- surgeDsnSampled %>%
  filter(site_id == "CH4-1000") %>% # grab Puerto Rico
  st_transform(crs = 32619)


# MAKE MAP MFMAP-----
## Create a theme
mapsf::mf_theme(
  #bg = "#a19e9d", 
  bg = NA,
  fg = "#000000", 
  mar = c(0,0,0,0), 
  tab = FALSE, inner = TRUE, line = 3, pos = "center", 
  cex = 1, font = 3)


mf_export(x = conus,
          filename = "output/figures/surgeMainSitesByStudy.png",
          width = 1200, expandBB = c(.3,.3,0.1,.1))

# CONUS
mf_inset_on(x = conus, fig = c(.2,.99,.02,.99)) #c(X1,X2,Y1,Y2)
mf_map(conus, var = "WSA9_NAME", type = "typo",
       border = NA, # don't include outline of climate zone
       pal = unique(conus$cols), # duplicate color records per climate zone, must use unique
       leg_pos = NA)
mf_map(x = conus.lines,
       type = "base",
       col = "black",
       add = TRUE,
       lwd = 2)
mf_map(x = conus.surge, var = "study",
       type = "symb",
       pch = c(21:23),
       pal = c("darkred", "green", "darkblue"),
       cex = 1.5,
       add = TRUE,
       leg_pos = NA)
# Scale Bar
mf_scale(size = 500, pos = "bottomright", scale_units = "mi",
         cex = 1.5)
mf_inset_off()

# Puerto Rico
mf_inset_on(x = pr, fig = c(0.65, 0.77, 0.02, 0.15)) # Where is the inset on the map?
# display the target municipality
mf_map(pr, var = "WSA9_NAME", type = "typo",
       border = NA, # don't include ecoregion outline
       pal = pr$color,
       leg_pos = NA)
mf_map(x = pr.lines,
       type = "base",
       col = "black",
       add = TRUE,
       lwd = 2)
mf_map(pr.surge, var = "study",
       type = "symb",
       pal = "green",
       pch = 22,
       cex = 1.5,
       add = TRUE,
       leg_pos = NA)

# Title inset?
mf_title("Puerto Rico", 
         bg = NA,
         pos = "center",
         tab = FALSE,
         cex = 1.5,
         line = 1,
         inner = TRUE)
# Scale Bar
mf_scale(size = 100, pos = "bottomleft", scale_units = "mi",
         cex = 1.5)

# close the inset
mf_inset_off()



# Add Map Title
# mf_title("Climate Zones of the U.S.",cex = 2)

# Add ecoregions legend
mf_inset_on(states, fig = c(0,0.3,0.1,0.8))

mf_legend(type = "typo", pos = "left",
          val = ecoR$WSA9_NAME, pal = ecoR$cols,
          title = "Ecoregions",
          val_cex = 1.25, title_cex = 2)
mf_inset_off()


# Add study legend for points
mf_inset_on(states, fig = c(0, 0.3, 0.85, 0.99)) #x1,x2,y1,y2

mf_legend(type = "symb", 
          pos = "left",
          val = c("2016 Regional Survey", "Hand picked", "SuRGE"),
          pch = 21:23,
          pal = c("darkred", "green", "darkblue"),
          title = "Study",
          #cex = 2, # size of symbols. This breaks symbols in rows 2 and 3?
          val_cex = 1.5, # size of symbol labels
          title_cex = 2)
mf_inset_off()

dev.off()


# EXAMPLE RESERVOIR SURVEY-------------
st_layers(paste0(userPath, "lakeDsn/CIN/CH4-013/merc013.gpkg")) # see layer names
p_polygon <- st_read(paste0("/vsizip/", userPath, "lakeDsn/2016_survey/brookville/brookevilleEqArea84.zip"))
p_points <- st_read(paste0("/vsizip/", userPath, "lakeDsn/2016_survey/brookville/brookevilleMainSites84.zip")) %>%
  mutate(site_type = case_when(siteID == "SU-07" ~ "index",
                               TRUE ~ "routine"))
p_bb <- st_read(paste0(userPath, "lakeDsn/2016_survey/brookville/brookville_bb.shp"))


ggplot() +
  
  # bounding box to make room for scale, north arrow, etc
  geom_sf(data = p_bb, fill = NA, color = NA) +
  
  # lake polygon
  geom_sf(data = p_polygon, aes(fill = section)) + # color by section
  scale_fill_manual(breaks = c("trib", "north", "south"), # reorder to spatially align with reservoir
                    values = c("#CCCCFF", "#9999FF", "#6666FF")) + # custom colors
  guides(fill = guide_legend(position = "inside")) + # put legend in plotting space
  
  # sites within reservoir
  geom_sf(data = p_points, aes(size = site_type)) +
  scale_size_manual(values = c(4,2)) +
  guides(size = guide_legend(position = "inside")) + # put legend in plotting space
  
  # spatial-aware automagic scale bar
  ggspatial::annotation_scale(location = "bl",
                              pad_x = unit(2, "cm")) +
  
  # spatial-aware automagic north arrow
  ggspatial::annotation_north_arrow(location = "br", 
                                    which_north = "true",
                                    pad_x = unit(2, "cm"),
                                    height = unit(1, "cm"), 
                                    width = unit(1, "cm")) +
  
  ggtitle("Brookville Reservoir, IN") +
  
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position.inside = c(0.8, 0.5))

ggsave("output/figures/brookvilleDesign.png", width = 4.02, height = 6.64)


