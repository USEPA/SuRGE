# Map---------------

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
states <- us_states() %>%
  dplyr::filter(!name %in% c('Alaska','Hawaii', 'Puerto Rico')) %>% # CONUS
  st_transform(5070)


# All manmade-sampled 2012 NLA
ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME)) +
  geom_sf(data = states, color = "cornsilk3", fill = NA, size = 0.1) +
  geom_sf(data = nla2012, size = 1) +
  scale_fill_manual("Ecoregion", values = cols) +
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

ggsave(filename="output/figures/manmadeSampled.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")



# NRS main sites

ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME)) +
  geom_sf(data = states, color = "cornsilk3", fill = NA, size = 0.1) +
  geom_sf(data = filter(nrs, panel != "OverSamp"), size = 1) + # see dsgn.R
 scale_fill_manual("Ecoregion", values = cols) +
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


ggsave(filename="output/figures/nrsMainSites.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")
