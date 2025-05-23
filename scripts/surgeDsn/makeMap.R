# Map---------------
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
  filter(!(site_id %in% c("CH4-1033", "CH4-1000"))) %>% # exclude Falls Lake and Puerto Rico
  mutate(study = case_when(sample_year == 2016 ~ "2016 Regional Study",
                           sample_year == 2018 ~ "2018 Regional Study",
                           TRUE ~ "SuRGE"))

unique(surgeDsn$lab)

# Convert to sf object
surgeDsn.sf <- st_as_sf(surgeDsn, coords = c("lon_dd83", "lat_dd83"), 
                        crs = 4269) %>% # NAD83
  st_transform(., crs = 5070) # Conus Albers

# Filter to sampled sites (2018, 2020, 2021, 2022, and 2023)
surgeDsnSampled <- surgeDsn.sf %>%
  # filter to sampled sites
  filter(eval_status_code == "S")
  
  
# READ NLA17 SAMPLED SITES-------
# loaded object is 'dg'
load(paste0(Sys.getenv("USERPROFILE"),
            "/Environmental Protection Agency (EPA)/",
            "ORD NLA17 Dissolved Gas - Documents/",
            "inputData/dg.2021-02-01.RData"))

coords <- data.frame(longitude = dg$map.lon.dd, latitude = dg$map.lat.dd)

dg.sf <- st_as_sf(dg, coords = c("map.lon.dd", "map.lat.dd"), 
                  crs = 4269) %>% # standard for lat/lon
  st_transform(5070) # project to CONUS ALBERS for plotting



# READ ECOREGION SHAPEFILE PROVIDED BY MARC WEBER ------------------
# Original shapefile provided by Marc Weber on 1/3/2017 in Albers.
# Simplified by Alex Hall.

ecoR <- st_read(dsn = "inputData/ecoregions",
                layer = "aggr_ecoregions_simple")

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
cols <- c("Coastal Plains" = "orange1",
          "Northern Appalachians" = "lightpink1",
          "Northern Plains" = "darksalmon",
          "Southern Appalachians" = "mediumturquoise",
          "Southern Plains" = "khaki4",
          "Temperate Plains" = "forestgreen", 
          "Upper Midwest" = "deepskyblue4",
          "Western Mountains" = "saddlebrown",
          "Xeric" = "lightskyblue4")


# sf APPROACH----------------
# Get states map
states <- USAboundaries::us_states() %>%
  dplyr::filter(!name %in% c('Alaska','Hawaii','Puerto Rico')) %>% # CONUS
  st_transform(5070)

# # All manmade-sampled 2017 NLA
# ggplot() +
#   geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME)) +
#   geom_sf(data = states, color = "cornsilk3", fill = NA, size = 0.1) +
#   geom_sf(data = nla17.sf, size = 1) +
#   scale_fill_manual("Ecoregion", values = cols) +
#   ggtitle("NLA17 Target Sampled Man-Made") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         plot.margin = unit(c(0,40,0,0),"mm"), # extend right plot mar for legend 
#         legend.position = c(1.08, .5), # move legend to immediate right of plot  
#         legend.text = element_text(size = 6),
#         legend.title = element_text(size = 6),
#         legend.key.size = unit(0.5, "cm"),
#         plot.title = element_text(hjust = 0.5)) 
# 
# ggsave(filename="output/figures/manmadeSampled17.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")



# SuRGE main sites by lab

ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME)) +
  geom_sf(data = states, color = "cornsilk3", fill = NA, size = 0.1) +
  geom_sf(data = filter(surgeDsn.sf, eval_status_code == "S"), size = 3, aes(color = lab)) + # see dsgn.R
 scale_fill_manual("Ecoregion", values = cols) +
  ggtitle("SuRGE Sample Sites") +
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
        plot.margin = unit(c(0,40,0,0),"mm"), # extend right plot mar for legend 
        legend.position = c(1.08, .5), # move legend to immediate right of plot  
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.5, "cm")) 


ggsave(filename="output/figures/surgeMainSitesByLab.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")


# SuRGE main sites

ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME)) +
  scale_fill_manual("Ecoregion", values = cols) +
  geom_sf(data = states, color = "cornsilk3", fill = NA, size = 0.5) +
  geom_sf(data = surgeDsnSampled, aes(shape = study, color = study), size = 2) +  
  scale_color_manual(values = c("darkred", "grey10", "darkblue")) +
  guides(color=guide_legend(title=NULL)) + # Suppress "study" legend title
  guides(shape=guide_legend(title=NULL)) + # Suppress "study" legend title
  #ggtitle("SuRGE Sample Sites") +
theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,40,0,0),"mm"), # extend right plot mar for legend 
        legend.position = c(1.08, .5), # move legend to immediate right of plot  
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, "cm")) 


ggsave(filename="output/figures/surgeMainSitesByStudy.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")


# SuRGE main sites with NLA

ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME)) +
  geom_sf(data = states, color = "cornsilk3", fill = NA, size = 0.5) +
  geom_sf(data = dg.sf, size = 1) +
  geom_sf(data = filter(surgeDsn.sf, eval_status_code == "S"), size = 3, color = "red") + # see dsgn.R
  scale_fill_manual("Ecoregion", values = cols) +
  ggtitle("Reservoir Sample Sites") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,40,0,0),"mm"), # extend right plot mar for legend 
        legend.position = c(1.08, .5), # move legend to immediate right of plot  
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.5, "cm")) 


ggsave(filename="output/figures/surgeMainSitesNla17sites.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
