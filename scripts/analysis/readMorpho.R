# Jeff Hollister used lakeMorpho to generate morpho indices for SuRGE lakes.
# Read in data.  All units are in m, m2, m3, or unitless  (i.e. ratio)


# Read from surge_morpho github repo
morpho <- read.csv(paste0(userPath, "data/siteDescriptors/morphometry/surge_morpho.csv")) %>% 
  as_tibble() %>% 
  select(-source) %>%
  pivot_wider(names_from = variables, values_from = values) %>%
  janitor::clean_names() %>% # lake_id is integer
  # calculate additional metrics. Casas-Ruiz et al 2021
  mutate(
    circularity = (4 * pi * surface_area) / shoreline_length^2,
    dynamic_ratio = shoreline_length / mean_depth,
    #force the 14 lakes that have a littoral fraction over 100 to 100
    littoral_fraction = ifelse(littoral_fraction<=100,littoral_fraction,100)
    #littoral_fraction = 1 - ((1 - (3/max_depth))^((max_depth/mean_depth) - 1)) # Jeff provides this metric
    )
