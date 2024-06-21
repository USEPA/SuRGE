# MAPS--------------
## get data--------
# sf for plotting
dat_agg_sf <- st_as_sf(dat_agg, coords = c("long", "lat"), 
                       crs = 4269) %>% # standard for lat/lon
  st_transform(5070) # project to CONUS ALBERS for plotting


# read in ecoregion polygons
ecoR <- st_read(dsn = paste0(userPath, 
                             "/data/spatial"),
                layer = "aggr_ecoregions_simple",
                quiet = TRUE)

# Check CRS
#st_crs(ecoR) # 3857
ecoR <- st_transform(ecoR, 5070) # convert to CONUS Albers
#st_crs(ecoR) # 5070

# SET UP CUSTOM COLORS FOR ECOREGIONS
# Custom color pallette for ecoregion polygons. Attempted to mirror
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


## Load state boundaries
states <- USAboundaries::us_states() %>%
  dplyr::filter(!state_name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")) %>%
  st_transform(5070) # convert to CONUS Albers

## CH4 diffusion---------
p_ch4_diff <- ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME), alpha = 0.5) +
  scale_fill_manual("Ecoregion", 
                    values = cols,
                    guide =  "none") + # turn of ecoregion legend
  ## move ecoregion legend to top.  this makes map smaller than images below
  # guides(fill = guide_legend(position = "top", # move ecoregion legend to top
  #                            # remove points from the boxes in the ecoregion legend
  #                            # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
  #                            override.aes = list(shape = NA))) + 
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = dat_agg_sf %>% 
            filter(visit == 1, lake_id != 1000), # exclude PR
          aes(size = ch4_diffusion_best),
          shape = 1,
          show.legend = "point") +
  scale_size(name = expression(CH[4]~diffusion~(mg~CH[4]~m^{-2}~hr^{-1})),
             range = c(0.5, 8), # custom size range
             breaks = c(1, 10, 20, 40)) + # custom breaks
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
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.5, "cm"))

# ggsave("output/figures/ch4diffusionBubblePlot.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")

## CH4 ebullition---------
p_ch4_ebu <- ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME), alpha = 0.5) + 
  # guide argument below removes points from the boxes in the ecoregion legend
  # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
  scale_fill_manual("Ecoregion", 
                    values = cols,
                    guide =  "none") + # turns off ecoregion legend
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = dat_agg_sf %>% 
            filter(visit == 1, lake_id != 1000), # exclude PR
          aes(size = ch4_ebullition),
          shape = 1,
          show.legend = "point") +
  scale_size(name = expression(CH[4]~ebullition~(mg~CH[4]~m^{-2}~hr^{-1})),
             range = c(0.5, 8), # custom size range
             breaks = c(1, 10, 20, 40)) + # custom breaks
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
        legend.position.inside = c(1, 1), # move legend to immediate right of plot  
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.5, "cm"))

# ggsave("output/figures/ch4ebullitionBubblePlot.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")

## CH4 total-----------
p_ch4_tot <- ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME), alpha = 0.5) +
  scale_fill_manual("Ecoregion", 
                    values = cols,
                    guide = "none") +
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = dat_agg_sf %>% 
            filter(visit == 1, lake_id != 1000), # exclude PR
          aes(size = ch4_total),
          shape = 1,
          show.legend = "point") +
  # guide argument below removes points from the boxes in the ecoregion legend
  # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/

  scale_size(name = expression(CH[4]~total~(mg~CH[4]~m^{-2}~hr^{-1})),
             range = c(0.5, 8), # custom size range
             breaks = c(1, 10, 25, 50)) + # custom breaks
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
        legend.position.inside = c(1, 1), # move legend to immediate right of plot  
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.5, "cm"))

# ggsave("output/figures/ch4totalBubblePlot.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")

plot_grid(p_ch4_diff, p_ch4_ebu, p_ch4_tot, ncol = 1)
ggsave("output/figures/ch4emissions3panel.tiff",
       width=10,height=10, units="in",
       dpi=800,compression="lzw")

## co2 diffusion------
ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME), alpha = 0.5) +
  scale_fill_manual("Ecoregion", 
                    values = cols) + # turn of ecoregion legend
  ## move ecoregion legend to top.
  guides(fill = guide_legend(position = "top", # move ecoregion legend to top
                             # remove points from the boxes in the ecoregion legend
                             # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
                             override.aes = list(shape = NA))) +
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = dat_agg_sf %>% 
            filter(visit == 1, 
                   lake_id != 1000, # exclude PR
                   co2_diffusion_best != 0,
                   !is.na(co2_diffusion_best)) %>%
            mutate(co2_diffusion_best_abs = abs(co2_diffusion_best),
                   co2_status = case_when(co2_diffusion_best < 0 ~ "sink",
                                          co2_diffusion_best > 0 ~ "source",
                                          is.na(co2_diffusion_best) ~ NA_character_,
                                          TRUE ~ "Fly you fools!")),
          aes(size = co2_diffusion_best_abs,
              shape = co2_status),
          show.legend = "point") +
  scale_size(name = expression(CO[2]~diffusion~(mg~CO[2]~m^{-2}~hr^{-1}))) + #,
             #range = c(0.5, 8), # custom size range
             #breaks = c(1, 10, 20, 40)) + # custom breaks
  scale_shape(name = element_blank()) +
  guides(shape = guide_legend(override.aes = list(size=5)))
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
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(0.5, "cm"))

ggsave("output/figures/co2diffusionBubblePlot.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

## NLA 17 sites for consideration-----
lake_list_sf <- st_as_sf(lake.list, coords = c("lon_dd83", "lat_dd83"), 
                         crs = 4269) %>% # standard for lat/lon
  st_transform(5070) # project to CONUS ALBERS for plotting


## map
ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME))  + # alpha = 0.5
  scale_fill_manual("Ecoregion", 
                    values = cols,
                    guide = "none") +
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = lake_list_sf %>% 
            filter(visit == 1, lake_id < 1000)) +# exclude PR and 2016 lakes
          #shape = 2) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank())

ggsave("output/figures/nla17ManMadeGreaterThan8Ha.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")




